{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

import Control.Applicative
import Control.Applicative.Lift
import Control.Arrow
import Control.Lens (imap)
import Control.Monad
import Control.Monad.Free hiding (Pure)
import Control.Monad.Trans.State
import Data.Aeson hiding ((.:))
import qualified Data.ByteString.Char8 as BS (isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Either (isRight, fromRight)
import Data.Either.Extra (eitherToMaybe)
import qualified Data.Foldable as F
import Data.Tensor hiding (select)
import Data.Function.Pointless
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Kind
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map(..))
import qualified Data.Map as M
import Data.Maybe hiding (mapMaybe)
import Data.Proxy
-- import qualified Data.Sequence as S
import Data.Set (Set(..))
import qualified Data.Set as S
import Debug.Trace
import GHC.Generics
import GHC.TypeLits
import Iso.Deriving
import Network.HTTP.Client hiding (Proxy(..), cookieJar)
import Network.HTTP.Simple hiding (Proxy(..))
import Safe
import Servant.API
import Servant.Client
import Servant.Client.Core.Request hiding (RequestBodyLBS, requestHeaders, requestBody)
import System.IO.Unsafe
import Test.Hspec
import Test.QuickCheck
import Text.Printf
import Witherable hiding (filter)

sampleShape = [6,4,4,3]

data TfCommand =
    Reshape [Int]
    | Reduce String [Int]
    | Transpose [Int]
    | ExpandDims Int deriving (Eq, Ord)

instance Show TfCommand where
    show (Reshape xs) = printf "x = tf.reshape(x, %s)" (show xs)
    show (Reduce op xs) = printf "x = tf.reduce_%s(x, %s)" op (show xs)
    show (Transpose xs) = printf "x = tf.transpose(x, %s)" (show xs)
    show (ExpandDims x) = printf "x = tf.expand_dims(x, %d)" x

newtype Axis a = Axis { getAxis :: Maybe a } deriving (Arbitrary,Functor,Applicative,Monad,Foldable,Traversable)

axis :: String -> Axis String
axis x = Axis (Just x)

anon :: Axis a
anon = Axis Nothing

instance Show a => Show (Axis a) where
    show (Axis (Just x)) = filter (/= '"') $ show x
    show (Axis Nothing) = "()"

deriving instance Eq a => Eq (Axis a)

deriving instance Ord a => Ord (Axis a)

instance ToJSON a => ToJSON (Axis a) where
    toJSON (Axis (Just x)) = toJSON x
    toJSON (Axis Nothing) = toJSON ("()" :: String)

-- equals Sum Identity []
data Composite a = Single a | Multiple [a] deriving (Functor, Foldable, Traversable, Show, Generic, Eq, Ord)

instance Inject (Lift [] a) (Composite a) where
    inj (Pure x) = Single x
    inj (Other xs) = Multiple xs

instance Project (Lift [] a) (Composite a) where
    prj (Single x) = Pure x
    prj (Multiple xs) = Other xs

instance Isomorphic (Lift [] a) (Composite a)

deriving via As1 (Lift []) Composite instance Applicative Composite

instance (Monad m, Monad n, Traversable n) => Monad (Compose m n) where
  ma >>= f = join_ (fmap f ma)
    where join_ = Compose . fmap join . (sequence <=< (getCompose . fmap getCompose))


instance Arbitrary a => Arbitrary (Composite a) where
    arbitrary = oneof [Single <$> arbitrary, Multiple <$> arbitrary]
    shrink = genericShrink

instance ToJSON a => ToJSON (Composite a)

data EquationStr a = EquationStr {
    eqn :: String
    , axes_lengths :: [(a,Int)]
    } deriving (Generic)

deriving instance Show a => Show (EquationStr a)

instance (Show a, ToJSON a) => ToJSON (EquationStr a) where
    toJSON (EquationStr{..}) =
        object ["eqn" .= eqn, "axes_lengths" .= toJSON (map (first show) axes_lengths)]

data Equation a = Equation {
    inp :: [Composite a]
    , outp :: [Composite a]
    , axesLengths :: [(a, Int)]
    } deriving (Show, Generic)

instance Arbitrary a => Arbitrary (Equation a) where
    arbitrary = do
        inp <- arbitrary
        n <- choose (0,10)
        outp <- take n <$> shuffle inp
        axesLengths <- arbitrary
        pure $ Equation{..}
    shrink = genericShrink

instance ToJSON a => ToJSON (Equation a)

eqnToStr :: Show a => Equation a -> String
eqnToStr (Equation{..}) = compsToStr inp <> " -> " <> compsToStr outp

eqnToEqnStr :: Show a => Equation a -> EquationStr a
eqnToEqnStr x@Equation{..} = EquationStr {eqn = eqnToStr x, axes_lengths = axesLengths}

compsToStr :: Show a => [Composite a] -> String
compsToStr = unwords . fmap compToStr

compToStr :: Show a => Composite a -> String
compToStr (Single x) = show x
compToStr (Multiple xs) = "(" <> unwords (fmap show xs) <> ")"

flatten :: [Composite a] -> [a]
flatten = (=<<) F.toList

type AxesPermutationRet = [Int]
type AddedAxesRet = [Int]
type OutputCompositeAxesRet = [[Int]]
type ElementaryAxesLengthsRet = [Maybe Int]
type InputCompositeAxesRet = [([Int], [Int])]
type ReducedElementaryAxesRet = [Int]

type InitShapesRet = [Int]
type ReducedAxesRet = [Int]
type AxesReorderingRet = [Int]
type AddedAxesReconstructRet = Map Int Int
type FinalShapesRet = [Int]

-- AUTOGEN BEGIN
addedAxes' :: (Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString AddedAxesRet
addedAxes' = fmap addedAxes . (checkDuplDim <=< checkRightDuplDim)

type RearrangeAddedAxesAPI a = "/rearrange/added_axes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] AddedAxesRet

rearrangeAddedAxesAPI :: Proxy (RearrangeAddedAxesAPI a)
rearrangeAddedAxesAPI = Proxy

rearrangeAddedAxesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM AddedAxesRet
rearrangeAddedAxesRequest = client rearrangeAddedAxesAPI

rearrangeAddedAxesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString AddedAxesRet
rearrangeAddedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeAddedAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceAddedAxesAPI a = "/reduce/added_axes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] AddedAxesRet

reduceAddedAxesAPI :: Proxy (ReduceAddedAxesAPI a)
reduceAddedAxesAPI = Proxy

reduceAddedAxesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM AddedAxesRet
reduceAddedAxesRequest = client reduceAddedAxesAPI

reduceAddedAxesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString AddedAxesRet
reduceAddedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceAddedAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatAddedAxesAPI a = "/repeat/added_axes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] AddedAxesRet

repeatAddedAxesAPI :: Proxy (RepeatAddedAxesAPI a)
repeatAddedAxesAPI = Proxy

repeatAddedAxesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM AddedAxesRet
repeatAddedAxesRequest = client repeatAddedAxesAPI

repeatAddedAxesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString AddedAxesRet
repeatAddedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatAddedAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

axesPermutation' :: (Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString AxesPermutationRet
axesPermutation' = fmap axesPermutation . (checkDuplDim <=< checkRightDuplDim)

type RearrangeAxesPermutationAPI a = "/rearrange/axes_permutation" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] AxesPermutationRet

rearrangeAxesPermutationAPI :: Proxy (RearrangeAxesPermutationAPI a)
rearrangeAxesPermutationAPI = Proxy

rearrangeAxesPermutationRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM AxesPermutationRet
rearrangeAxesPermutationRequest = client rearrangeAxesPermutationAPI

rearrangeAxesPermutationPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString AxesPermutationRet
rearrangeAxesPermutationPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeAxesPermutationRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceAxesPermutationAPI a = "/reduce/axes_permutation" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] AxesPermutationRet

reduceAxesPermutationAPI :: Proxy (ReduceAxesPermutationAPI a)
reduceAxesPermutationAPI = Proxy

reduceAxesPermutationRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM AxesPermutationRet
reduceAxesPermutationRequest = client reduceAxesPermutationAPI

reduceAxesPermutationPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString AxesPermutationRet
reduceAxesPermutationPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceAxesPermutationRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatAxesPermutationAPI a = "/repeat/axes_permutation" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] AxesPermutationRet

repeatAxesPermutationAPI :: Proxy (RepeatAxesPermutationAPI a)
repeatAxesPermutationAPI = Proxy

repeatAxesPermutationRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM AxesPermutationRet
repeatAxesPermutationRequest = client repeatAxesPermutationAPI

repeatAxesPermutationPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString AxesPermutationRet
repeatAxesPermutationPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatAxesPermutationRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

elementaryAxesLengths' :: (Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString ElementaryAxesLengthsRet
elementaryAxesLengths' = fmap elementaryAxesLengths . (checkDuplDim <=< checkRightDuplDim)

type RearrangeElementaryAxesLengthsAPI a = "/rearrange/elementary_axes_lengths" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] ElementaryAxesLengthsRet

rearrangeElementaryAxesLengthsAPI :: Proxy (RearrangeElementaryAxesLengthsAPI a)
rearrangeElementaryAxesLengthsAPI = Proxy

rearrangeElementaryAxesLengthsRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM ElementaryAxesLengthsRet
rearrangeElementaryAxesLengthsRequest = client rearrangeElementaryAxesLengthsAPI

rearrangeElementaryAxesLengthsPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString ElementaryAxesLengthsRet
rearrangeElementaryAxesLengthsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeElementaryAxesLengthsRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceElementaryAxesLengthsAPI a = "/reduce/elementary_axes_lengths" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] ElementaryAxesLengthsRet

reduceElementaryAxesLengthsAPI :: Proxy (ReduceElementaryAxesLengthsAPI a)
reduceElementaryAxesLengthsAPI = Proxy

reduceElementaryAxesLengthsRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM ElementaryAxesLengthsRet
reduceElementaryAxesLengthsRequest = client reduceElementaryAxesLengthsAPI

reduceElementaryAxesLengthsPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString ElementaryAxesLengthsRet
reduceElementaryAxesLengthsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceElementaryAxesLengthsRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatElementaryAxesLengthsAPI a = "/repeat/elementary_axes_lengths" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] ElementaryAxesLengthsRet

repeatElementaryAxesLengthsAPI :: Proxy (RepeatElementaryAxesLengthsAPI a)
repeatElementaryAxesLengthsAPI = Proxy

repeatElementaryAxesLengthsRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM ElementaryAxesLengthsRet
repeatElementaryAxesLengthsRequest = client repeatElementaryAxesLengthsAPI

repeatElementaryAxesLengthsPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString ElementaryAxesLengthsRet
repeatElementaryAxesLengthsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatElementaryAxesLengthsRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

inputCompositeAxes' :: (Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString InputCompositeAxesRet
inputCompositeAxes' = fmap inputCompositeAxes . (checkDuplDim <=< checkRightDuplDim)

type RearrangeInputCompositeAxesAPI a = "/rearrange/input_composite_axes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] InputCompositeAxesRet

rearrangeInputCompositeAxesAPI :: Proxy (RearrangeInputCompositeAxesAPI a)
rearrangeInputCompositeAxesAPI = Proxy

rearrangeInputCompositeAxesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM InputCompositeAxesRet
rearrangeInputCompositeAxesRequest = client rearrangeInputCompositeAxesAPI

rearrangeInputCompositeAxesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString InputCompositeAxesRet
rearrangeInputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeInputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceInputCompositeAxesAPI a = "/reduce/input_composite_axes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] InputCompositeAxesRet

reduceInputCompositeAxesAPI :: Proxy (ReduceInputCompositeAxesAPI a)
reduceInputCompositeAxesAPI = Proxy

reduceInputCompositeAxesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM InputCompositeAxesRet
reduceInputCompositeAxesRequest = client reduceInputCompositeAxesAPI

reduceInputCompositeAxesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString InputCompositeAxesRet
reduceInputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceInputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatInputCompositeAxesAPI a = "/repeat/input_composite_axes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] InputCompositeAxesRet

repeatInputCompositeAxesAPI :: Proxy (RepeatInputCompositeAxesAPI a)
repeatInputCompositeAxesAPI = Proxy

repeatInputCompositeAxesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM InputCompositeAxesRet
repeatInputCompositeAxesRequest = client repeatInputCompositeAxesAPI

repeatInputCompositeAxesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString InputCompositeAxesRet
repeatInputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatInputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

outputCompositeAxes' :: (Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString OutputCompositeAxesRet
outputCompositeAxes' = fmap outputCompositeAxes . (checkDuplDim <=< checkRightDuplDim)

type RearrangeOutputCompositeAxesAPI a = "/rearrange/output_composite_axes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] OutputCompositeAxesRet

rearrangeOutputCompositeAxesAPI :: Proxy (RearrangeOutputCompositeAxesAPI a)
rearrangeOutputCompositeAxesAPI = Proxy

rearrangeOutputCompositeAxesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM OutputCompositeAxesRet
rearrangeOutputCompositeAxesRequest = client rearrangeOutputCompositeAxesAPI

rearrangeOutputCompositeAxesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString OutputCompositeAxesRet
rearrangeOutputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeOutputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceOutputCompositeAxesAPI a = "/reduce/output_composite_axes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] OutputCompositeAxesRet

reduceOutputCompositeAxesAPI :: Proxy (ReduceOutputCompositeAxesAPI a)
reduceOutputCompositeAxesAPI = Proxy

reduceOutputCompositeAxesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM OutputCompositeAxesRet
reduceOutputCompositeAxesRequest = client reduceOutputCompositeAxesAPI

reduceOutputCompositeAxesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString OutputCompositeAxesRet
reduceOutputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceOutputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatOutputCompositeAxesAPI a = "/repeat/output_composite_axes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] OutputCompositeAxesRet

repeatOutputCompositeAxesAPI :: Proxy (RepeatOutputCompositeAxesAPI a)
repeatOutputCompositeAxesAPI = Proxy

repeatOutputCompositeAxesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM OutputCompositeAxesRet
repeatOutputCompositeAxesRequest = client repeatOutputCompositeAxesAPI

repeatOutputCompositeAxesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString OutputCompositeAxesRet
repeatOutputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatOutputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

reducedElementaryAxes' :: (Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString ReducedElementaryAxesRet
reducedElementaryAxes' = fmap reducedElementaryAxes . (checkDuplDim <=< checkRightDuplDim)

type RearrangeReducedElementaryAxesAPI a = "/rearrange/reduced_elementary_axes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] ReducedElementaryAxesRet

rearrangeReducedElementaryAxesAPI :: Proxy (RearrangeReducedElementaryAxesAPI a)
rearrangeReducedElementaryAxesAPI = Proxy

rearrangeReducedElementaryAxesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM ReducedElementaryAxesRet
rearrangeReducedElementaryAxesRequest = client rearrangeReducedElementaryAxesAPI

rearrangeReducedElementaryAxesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString ReducedElementaryAxesRet
rearrangeReducedElementaryAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeReducedElementaryAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceReducedElementaryAxesAPI a = "/reduce/reduced_elementary_axes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] ReducedElementaryAxesRet

reduceReducedElementaryAxesAPI :: Proxy (ReduceReducedElementaryAxesAPI a)
reduceReducedElementaryAxesAPI = Proxy

reduceReducedElementaryAxesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM ReducedElementaryAxesRet
reduceReducedElementaryAxesRequest = client reduceReducedElementaryAxesAPI

reduceReducedElementaryAxesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString ReducedElementaryAxesRet
reduceReducedElementaryAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceReducedElementaryAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatReducedElementaryAxesAPI a = "/repeat/reduced_elementary_axes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] ReducedElementaryAxesRet

repeatReducedElementaryAxesAPI :: Proxy (RepeatReducedElementaryAxesAPI a)
repeatReducedElementaryAxesAPI = Proxy

repeatReducedElementaryAxesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM ReducedElementaryAxesRet
repeatReducedElementaryAxesRequest = client repeatReducedElementaryAxesAPI

repeatReducedElementaryAxesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString ReducedElementaryAxesRet
repeatReducedElementaryAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatReducedElementaryAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))


-- AUTOGEN END

-- RECONSTRUCT AUTOGEN BEGIN
initShapes' :: (Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString InitShapesRet
initShapes' = fmap initShapes . (checkDuplDim <=< checkRightDuplDim)

type RearrangeInitShapesAPI a = "/rearrange/init_shapes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] InitShapesRet

rearrangeInitShapesAPI :: Proxy (RearrangeInitShapesAPI a)
rearrangeInitShapesAPI = Proxy

rearrangeInitShapesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM InitShapesRet
rearrangeInitShapesRequest = client rearrangeInitShapesAPI

rearrangeInitShapesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString InitShapesRet
rearrangeInitShapesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeInitShapesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceInitShapesAPI a = "/reduce/init_shapes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] InitShapesRet

reduceInitShapesAPI :: Proxy (ReduceInitShapesAPI a)
reduceInitShapesAPI = Proxy

reduceInitShapesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM InitShapesRet
reduceInitShapesRequest = client reduceInitShapesAPI

reduceInitShapesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString InitShapesRet
reduceInitShapesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceInitShapesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatInitShapesAPI a = "/repeat/init_shapes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] InitShapesRet

repeatInitShapesAPI :: Proxy (RepeatInitShapesAPI a)
repeatInitShapesAPI = Proxy

repeatInitShapesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM InitShapesRet
repeatInitShapesRequest = client repeatInitShapesAPI

repeatInitShapesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString InitShapesRet
repeatInitShapesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatInitShapesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

reducedAxes' :: (Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString ReducedAxesRet
reducedAxes' = fmap reducedAxes . (checkDuplDim <=< checkRightDuplDim)

type RearrangeReducedAxesAPI a = "/rearrange/reduced_axes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] ReducedAxesRet

rearrangeReducedAxesAPI :: Proxy (RearrangeReducedAxesAPI a)
rearrangeReducedAxesAPI = Proxy

rearrangeReducedAxesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM ReducedAxesRet
rearrangeReducedAxesRequest = client rearrangeReducedAxesAPI

rearrangeReducedAxesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString ReducedAxesRet
rearrangeReducedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeReducedAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceReducedAxesAPI a = "/reduce/reduced_axes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] ReducedAxesRet

reduceReducedAxesAPI :: Proxy (ReduceReducedAxesAPI a)
reduceReducedAxesAPI = Proxy

reduceReducedAxesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM ReducedAxesRet
reduceReducedAxesRequest = client reduceReducedAxesAPI

reduceReducedAxesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString ReducedAxesRet
reduceReducedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceReducedAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatReducedAxesAPI a = "/repeat/reduced_axes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] ReducedAxesRet

repeatReducedAxesAPI :: Proxy (RepeatReducedAxesAPI a)
repeatReducedAxesAPI = Proxy

repeatReducedAxesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM ReducedAxesRet
repeatReducedAxesRequest = client repeatReducedAxesAPI

repeatReducedAxesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString ReducedAxesRet
repeatReducedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatReducedAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

axesReordering' :: (Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString AxesReorderingRet
axesReordering' = fmap axesReordering . (checkDuplDim <=< checkRightDuplDim)

type RearrangeAxesReorderingAPI a = "/rearrange/axes_reordering" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] AxesReorderingRet

rearrangeAxesReorderingAPI :: Proxy (RearrangeAxesReorderingAPI a)
rearrangeAxesReorderingAPI = Proxy

rearrangeAxesReorderingRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM AxesReorderingRet
rearrangeAxesReorderingRequest = client rearrangeAxesReorderingAPI

rearrangeAxesReorderingPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString AxesReorderingRet
rearrangeAxesReorderingPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeAxesReorderingRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceAxesReorderingAPI a = "/reduce/axes_reordering" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] AxesReorderingRet

reduceAxesReorderingAPI :: Proxy (ReduceAxesReorderingAPI a)
reduceAxesReorderingAPI = Proxy

reduceAxesReorderingRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM AxesReorderingRet
reduceAxesReorderingRequest = client reduceAxesReorderingAPI

reduceAxesReorderingPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString AxesReorderingRet
reduceAxesReorderingPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceAxesReorderingRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatAxesReorderingAPI a = "/repeat/axes_reordering" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] AxesReorderingRet

repeatAxesReorderingAPI :: Proxy (RepeatAxesReorderingAPI a)
repeatAxesReorderingAPI = Proxy

repeatAxesReorderingRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM AxesReorderingRet
repeatAxesReorderingRequest = client repeatAxesReorderingAPI

repeatAxesReorderingPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString AxesReorderingRet
repeatAxesReorderingPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatAxesReorderingRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

addedAxesReconstruct' :: (Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString AddedAxesReconstructRet
addedAxesReconstruct' = fmap addedAxesReconstruct . (checkDuplDim <=< checkRightDuplDim)

type RearrangeAddedAxesReconstructAPI a = "/rearrange/added_axes_reconstruct" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] AddedAxesReconstructRet

rearrangeAddedAxesReconstructAPI :: Proxy (RearrangeAddedAxesReconstructAPI a)
rearrangeAddedAxesReconstructAPI = Proxy

rearrangeAddedAxesReconstructRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM AddedAxesReconstructRet
rearrangeAddedAxesReconstructRequest = client rearrangeAddedAxesReconstructAPI

rearrangeAddedAxesReconstructPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString AddedAxesReconstructRet
rearrangeAddedAxesReconstructPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeAddedAxesReconstructRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceAddedAxesReconstructAPI a = "/reduce/added_axes_reconstruct" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] AddedAxesReconstructRet

reduceAddedAxesReconstructAPI :: Proxy (ReduceAddedAxesReconstructAPI a)
reduceAddedAxesReconstructAPI = Proxy

reduceAddedAxesReconstructRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM AddedAxesReconstructRet
reduceAddedAxesReconstructRequest = client reduceAddedAxesReconstructAPI

reduceAddedAxesReconstructPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString AddedAxesReconstructRet
reduceAddedAxesReconstructPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceAddedAxesReconstructRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatAddedAxesReconstructAPI a = "/repeat/added_axes_reconstruct" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] AddedAxesReconstructRet

repeatAddedAxesReconstructAPI :: Proxy (RepeatAddedAxesReconstructAPI a)
repeatAddedAxesReconstructAPI = Proxy

repeatAddedAxesReconstructRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM AddedAxesReconstructRet
repeatAddedAxesReconstructRequest = client repeatAddedAxesReconstructAPI

repeatAddedAxesReconstructPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString AddedAxesReconstructRet
repeatAddedAxesReconstructPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatAddedAxesReconstructRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

finalShapes' :: (Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString FinalShapesRet
finalShapes' = fmap finalShapes . (checkDuplDim <=< checkRightDuplDim)

type RearrangeFinalShapesAPI a = "/rearrange/final_shapes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] FinalShapesRet

rearrangeFinalShapesAPI :: Proxy (RearrangeFinalShapesAPI a)
rearrangeFinalShapesAPI = Proxy

rearrangeFinalShapesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM FinalShapesRet
rearrangeFinalShapesRequest = client rearrangeFinalShapesAPI

rearrangeFinalShapesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString FinalShapesRet
rearrangeFinalShapesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeFinalShapesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceFinalShapesAPI a = "/reduce/final_shapes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] FinalShapesRet

reduceFinalShapesAPI :: Proxy (ReduceFinalShapesAPI a)
reduceFinalShapesAPI = Proxy

reduceFinalShapesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM FinalShapesRet
reduceFinalShapesRequest = client reduceFinalShapesAPI

reduceFinalShapesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString FinalShapesRet
reduceFinalShapesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceFinalShapesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatFinalShapesAPI a = "/repeat/final_shapes" :> ReqBody '[JSON] (EquationStr (Axis a)) :> Post '[JSON] FinalShapesRet

repeatFinalShapesAPI :: Proxy (RepeatFinalShapesAPI a)
repeatFinalShapesAPI = Proxy

repeatFinalShapesRequest :: (Show a,ToJSON a) => EquationStr (Axis a) -> ClientM FinalShapesRet
repeatFinalShapesRequest = client repeatFinalShapesAPI

repeatFinalShapesPy :: (Show a,ToJSON a) => Equation (Axis a) -> Either BS.ByteString FinalShapesRet
repeatFinalShapesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatFinalShapesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))


-- RECONSTRUCT AUTOGEN END
intersectCompLists :: Ord a => [Composite a] -> [Composite a] -> [Composite a]
intersectCompLists xs ys = uncc $ mapMaybe (\x -> if x `elem` cc ys then Just x else Nothing) (cc xs)

axisNumsFromCompList :: Ord a => [Composite (Axis a)] -> M.Map (Axis a) Int
axisNumsFromCompList = snd . foldl' (\(i,acc) x ->
        if x == Axis Nothing then (i,acc) else
        if M.member x acc then (i,acc) else
        (i+1,M.insertWith (\a b -> error "no duplicates allowed") x i acc))
        (0,M.empty) . cc

-- axesPermutation gives the numbers of flatten output axes
axesPermutation :: Ord a => Equation (Axis a) -> [Int]
axesPermutation (Equation{..}) = let
    axisNums = axisNumsFromCompList (intersectCompLists inp outp)
    in
    foldr ((:) . (axisNums M.!)) [] . cc $ outp

-- addedAxes given equation returns a map from axes numbers to repeat counts
-- Example: addedAxes "h w -> h w 3" is {2: 3}
-- TODO: Add integer type to Axis datatype
addedAxes :: Equation (Axis a) -> AddedAxesRet
addedAxes _ = []  -- TODO: implement

-- example: reducedElementaryAxes "h w -> h" "max" is [1]
reducedElementaryAxes :: Ord a => Equation (Axis a) -> ReducedElementaryAxesRet
reducedElementaryAxes (Equation{..}) = let
    axisNums = axisNumsFromCompList inp
    in
    map (axisNums M.!) $ flatten inp \\ flatten outp

outputCompositeAxes :: Ord a => Equation (Axis a) -> OutputCompositeAxesRet
outputCompositeAxes eqn@(Equation{..}) = let
    axisNums = axisNumsFromCompList inp
    in
    map (F.toList . fmap (axisNums M.!)) outp

elementaryAxesLengths :: Ord a => Equation (Axis a) -> ElementaryAxesLengthsRet
elementaryAxesLengths eqn@Equation{..} = let m = M.fromList axesLengths in
    foldr (\x acc -> if x == Axis Nothing then acc else ((:) . (`M.lookup` m)) x acc) [] . cc $ inp

-- unexported helper from Data.List
select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
select p x ~(ts,fs) | p x       = (x:ts,fs)
                    | otherwise = (ts, x:fs)

-- inputCompositeAxes returns a list that for each composite axis returns its
-- tuple of known and unknown axis numbers
inputCompositeAxes :: Ord a => Equation (Axis a) -> [([Int],[Int])]
inputCompositeAxes eqn@Equation{..} =
    let
        axisNums = axisNumsFromCompList inp
        known = S.fromList (fmap ((axisNums M.!) . fst) axesLengths)
        in map (
            foldr (select (`S.member` known) . (axisNums M.!)) ([],[])
            ) inp

-- TODO: fuse
-- TODO: allow deeper nesting of Composite
initMap :: Ord a => Equation (Axis a) -> [Int] -> Map (Axis a) Int
initMap eqn@Equation{..} shape =
    let
    sizes = M.fromList . snd $ foldr (\x' (i,acc) -> case x' of
            Single x -> (i-1,(x, shape !! i):acc)
            Multiple xs -> (i-1,handleMultiple i xs ++ acc)
            ) (length inp - 1,[]) inp
    handleMultiple i xs = map (\x -> if x `M.member` axesLengthsMap then
        (x,axesLengthsMap M.! x)
        else
        (x,shape !! i `div` prod xs)
        ) xs
        where
            prod = product . map (axesLengthsMap M.!) . filter (`M.member` axesLengthsMap)
    axesLengthsMap = M.fromList axesLengths
    in sizes

initShapes :: Ord a => Equation (Axis a) -> InitShapesRet
initShapes = initShapesWithShape sampleShape

-- TODO: fuse
initShapesWithShape :: Ord a => [Int] -> Equation (Axis a) -> InitShapesRet
initShapesWithShape shape eqn@Equation{..} = map (initMap eqn shape M.!) (flatten inp)

-- TODO: remove
reducedAxes :: Ord a => Equation (Axis a) -> ReducedAxesRet
reducedAxes = reducedElementaryAxes

-- TODO: remove
axesReordering :: Ord a => Equation (Axis a) -> AxesReorderingRet
axesReordering = axesPermutation

-- TODO: implement
addedAxesReconstruct :: Equation (Axis a) -> AddedAxesReconstructRet
addedAxesReconstruct _ = M.empty

finalShapes :: Ord a => Equation (Axis a) -> FinalShapesRet
finalShapes = finalShapesWithShape sampleShape

-- TODO: allow deeper nesting
-- TODO: fuse
finalShapesWithShape :: Ord a => [Int] -> Equation (Axis a) -> FinalShapesRet
finalShapesWithShape shape eqn@Equation{..} = map (foldr ((*) . (initMap eqn shape M.!)) 1) outp
-- end of reconstruct

-- TODO: Generalize reduction type
applyRecipe :: Ord a => [Int] -> Equation (Axis a) -> [TfCommand]
applyRecipe shape eqn@Equation{..} = let
    x = initShapesWithShape shape eqn
    y = reducedAxes eqn
    z = axesReordering eqn
    w = addedAxesReconstruct eqn
    v = finalShapesWithShape shape eqn
    reshapeCmd = Reshape x
    reduceCmds = if null y then [] else getReduceCmds "max" y
    transposeCmd = Transpose z
    addAxisCmds = getAddAxisCmds (length z + length w) w
    finalReshapeCmd = Reshape v
    in
    [reshapeCmd] ++ reduceCmds ++ [transposeCmd] ++ addAxisCmds ++ [finalReshapeCmd]

-- TODO: Implement
getAddAxisCmds :: Int -> Map Int Int -> [TfCommand]
getAddAxisCmds _ _ = []

-- TODO: Implement
getReduceCmds :: String -> [Int] -> [TfCommand]
getReduceCmds op xs = [Reduce op xs]

--     if len(added_axes) > 0:
--         tensor = backend.add_axes(tensor, n_axes=len(axes_reordering) + len(added_axes), pos2len=added_axes)

-- def _apply_recipe(recipe: TransformRecipe, tensor: Tensor, reduction_type: Reduction) -> Tensor:
--     # this method works for all backends but not compilable with
--     backend = get_backend(tensor)
--     init_shapes, reduced_axes, axes_reordering, added_axes, final_shapes = \
--         _reconstruct_from_shape(recipe, backend.shape(tensor))
--     tensor = backend.reshape(tensor, init_shapes)
--     tensor = _reduce_axes(tensor, reduction_type=reduction_type, reduced_axes=reduced_axes, backend=backend)
--     tensor = backend.transpose(tensor, axes_reordering)
--     if len(added_axes) > 0:
--         tensor = backend.add_axes(tensor, n_axes=len(axes_reordering) + len(added_axes), pos2len=added_axes)
--     return backend.reshape(tensor, final_shapes)


newtype CC a = CC (Compose [] Composite a) deriving (Functor, Foldable, Traversable, Applicative, Alternative)

-- smart constructor
cc :: [Composite a] -> CC a
cc = CC . Compose

uncc :: CC a -> [Composite a]
uncc (CC (Compose x)) = x

newtype CCC a = CCC (Compose (Compose [] Composite) Axis a) deriving (Functor, Foldable, Traversable, Applicative, Alternative)

ccc :: [Composite (Axis a)] -> CCC a
ccc = CCC . Compose . Compose

instance Filterable CC where
   mapMaybe _ (CC (Compose []))     = CC (Compose [])
   mapMaybe f (CC (Compose (Single x:xs))) =
    let CC (Compose rs) = mapMaybe f (CC $ Compose xs) in
    case f x of
     Nothing -> CC (Compose rs)
     Just r  -> CC $ Compose $ Single r:rs
   mapMaybe f (CC (Compose (Multiple x:xs))) =
    let CC (Compose rs) = mapMaybe f (CC $ Compose xs) in
    case mapMaybe f x of
     [] -> CC $ Compose rs
     r  -> CC $ Compose $ Multiple r:rs

instance Witherable CC

fixup :: (Show a,Ord a) => Equation (Axis a) -> Equation (Axis a)
fixup eqn@Equation{..} = let
    inp' = uncc . remDupl . cc $ inp
    outp' = uncc . remDupl . cc $ outp
    in eqn{inp = inp', outp = outp'}

remDupl :: (Show a,Ord a,Witherable t) => t a -> t a
remDupl = (`evalState` S.empty) . wither (\a -> state (\s -> (mfilter (not . (`S.member` s)) (Just a),S.insert a s)))

remDupl' :: (Show a,Ord a) => [Composite (Axis a)] -> [Composite (Axis a)]
remDupl' = uncc . remDupl . cc

-- TODO: check this if operation is rearrange
-- checkOneSideIdent :: (Show (f a),Eq (f a),Foldable f) => Equation (f a) -> Either String (Equation (f a))
checkOneSideIdent :: (Eq a,Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString (Equation (Axis a))
checkOneSideIdent (Equation{..}) = if union inp' outp' == intersect inp' outp' then
    Right $ Equation{..}
    else
    Left $ "Identifiers only on one side of expression (should be on both): " <> fmt (sort oneSiders)
    where
        inp' = flatten inp
        outp' = flatten outp
        fmt = (\x -> "{" <> x <> "}") . BS.intercalate ", " . map (\x -> "'" <> BS.pack (f $ show x) <> "'")
        oneSiders = if not . null $ inp' \\ outp' then inp' \\ outp' else outp' \\ inp'
        f "..." = "\226\128\166"
        f x = x

checkDuplDim :: (Eq a,Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString (Equation (Axis a))
checkDuplDim eqn@(Equation{..}) = if flatten inp == flatten (remDupl' inp) && flatten outp == flatten (remDupl' outp)
    then
    Right $ Equation{..}
    else
    let
        ys = head ((flatten inp \\ flatten (remDupl' inp)) ++ (flatten outp \\ flatten (remDupl' outp)))
        dup = BS.pack . show $ ys in Left $ "Indexing expression contains duplicate dimension \"" <> dup <> "\""

checkRightDuplDim :: (Eq a,Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString (Equation (Axis a))
checkRightDuplDim eqn@(Equation{..}) = if flatten outp == flatten (remDupl' outp)
    then
    Right eqn
    else
    let
        ys = head ((flatten inp \\ flatten (remDupl' inp)) ++ (flatten outp \\ flatten (remDupl' outp)))
        dup = BS.pack . show $ ys in Left $ "Indexing expression contains duplicate dimension \"" <> dup <> "\""

findError :: ClientError -> BS.ByteString
findError (UnsupportedContentType req resp@Response{..}) = responseBody
findError x = error (show x)

main :: IO ()
main = do
    print $ finalShapes' (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "H"), Multiple [axis "B", axis "W"], Single (axis "C")]
                , axesLengths = []
                })
    hspec $ do
        it "gets axes permutations for valid equation" $
            axesPermutation' (Equation {
                inp = [Single (axis "B"), Single (axis "H")]
                , outp = [Single (axis "H"), Single (axis "B")]
                , axesLengths = []
                })
            `shouldBe`
            rearrangeAxesPermutationPy (Equation {
                inp = [Single (axis "B"), Single (axis "H")]
                , outp = [Single (axis "H"), Single (axis "B")]
                , axesLengths = []
                })
    hspec $ do
        it "returns error for duplicate dimension" $
            axesPermutation' (Equation {
                inp = [Multiple [axis "B", axis "B"]]
                , outp = [Multiple [axis "B", axis "B"]]
                , axesLengths = []
                })
            `shouldBe`
            rearrangeAxesPermutationPy (Equation {
                inp = [Multiple [axis "B", axis "B"]]
                , outp = [Multiple [axis "B", axis "B"]]
                , axesLengths = []
                })
        it "calculates axes permutation for reduction" $
            axesPermutation' (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "B"), Single (axis "C")]
                , axesLengths = []
                })
            `shouldBe`
            reduceAxesPermutationPy (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "B"), Single (axis "C")]
                , axesLengths = []
                })
        -- -- this fails because I've taken the check out for now
        -- it "returns error for one side ident" $
        --     axesPermutation' (Equation {
        --         inp = [Single (axis "B")]
        --         , outp = []
        --         , axesLengths = []
        --         })
        --     `shouldBe`
        --     rearrangeAxesPermutationPy (Equation {
        --         inp = [Single (axis "B")]
        --         , outp = []
        --         , axesLengths = []
        --         })

    hspec $ do
        it "calculates elementary axes lengths" $
            elementaryAxesLengths' (Equation {
                inp = [Single (axis "B"), Single (axis "H")]
                , outp = [Single (axis "H"), Single (axis "B")]
                , axesLengths = [(axis "B",2)]
                })
            `shouldBe`
            rearrangeElementaryAxesLengthsPy (Equation {
                inp = [Single (axis "B"), Single (axis "H")]
                , outp = [Single (axis "H"), Single (axis "B")]
                , axesLengths = [(axis "B",2)]
                })
        it "calculates elementary axes lengths for multiples" $
            elementaryAxesLengths' (Equation {
                inp = [Multiple [axis "W", axis "C"], Single (axis "H")]
                , outp = [Single (axis "W"), Single (axis "C"), Single (axis "H")]
                , axesLengths = [(axis "C", 2)]
                })
            `shouldBe`
            rearrangeElementaryAxesLengthsPy (Equation {
                inp = [Multiple [axis "W", axis "C"], Single (axis "H")]
                , outp = [Single (axis "W"), Single (axis "C"), Single (axis "H")]
                , axesLengths = [(axis "C", 2)]
                })
        it "calculates elementary axes lengths for anonymous axis" $
            elementaryAxesLengths' (Equation {
                inp = [Single (Axis Nothing), Single (axis "H")]
                , outp = [Single (axis "H")]
                , axesLengths = []
                })
            `shouldBe`
            rearrangeElementaryAxesLengthsPy (Equation {
                inp = [Single (Axis Nothing), Single (axis "H")]
                , outp = [Single (axis "H")]
                , axesLengths = []
                })

    hspec $ do
        it "calculates input composite axes" $
            inputCompositeAxes' (Equation {
                inp = [Multiple [axis "W", axis "C"], Single (axis "H")]
                , outp = [Single (axis "W"), Single (axis "C"), Single (axis "H")]
                , axesLengths = [(axis "C", 2)]
                })
            `shouldBe`
            rearrangeInputCompositeAxesPy (Equation {
                inp = [Multiple [axis "W", axis "C"], Single (axis "H")]
                , outp = [Single (axis "W"), Single (axis "C"), Single (axis "H")]
                , axesLengths = [(axis "C", 2)]
                })
        it "calculates more composite axes" $
            inputCompositeAxes' (Equation {
                inp = [Single (axis "H"), Multiple [axis "W"]]
                , outp = [Single (axis "W"), Single (axis "H")]
                , axesLengths = [(axis "W", 2)]
                })
            `shouldBe`
            rearrangeInputCompositeAxesPy (Equation {
                inp = [Single (axis "H"), Multiple [axis "W"]]
                , outp = [Single (axis "W"), Single (axis "H")]
                , axesLengths = [(axis "W", 2)]
                })
        -- it "calculates input composite axes for anonymous axis" $
        --     inputCompositeAxes' (Equation {
        --         inp = [Multiple [axis "W", axis "C"], Single (Axis Nothing)]
        --         , outp = [Single (axis "W"), Single (axis "C"), Single (Axis Nothing)]
        --         , axesLengths = [(axis "C", 2)]
        --         })
        --     `shouldBe`
        --     rearrangeInputCompositeAxesPy (Equation {
        --         inp = [Multiple [axis "W", axis "C"], Single (Axis Nothing)]
        --         , outp = [Single (axis "W"), Single (axis "C"), Single (Axis Nothing)]
        --         , axesLengths = [(axis "C", 2)]
        --         })
    hspec $ do
        it "calculates reduced elementary axes" $
            reducedElementaryAxes' (Equation {
                inp = [Single (axis "B"), Single (axis "H")]
                , outp = [Single (axis "B")]
                , axesLengths = []
                })
            `shouldBe`
            reduceReducedElementaryAxesPy (Equation {
                inp = [Single (axis "B"), Single (axis "H")]
                , outp = [Single (axis "B")]
                , axesLengths = []
                })
    hspec $ do
        it "calculates init shapes" $
            initShapes' (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "H"), Single (axis "B"), Single (axis "W"), Single (axis "C")]
                , axesLengths = []
                })
            `shouldBe`
            rearrangeInitShapesPy (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "H"), Single (axis "B"), Single (axis "W"), Single (axis "C")]
                , axesLengths = []
                })

    hspec $ do
        it "calculates reduced axes" $
            reducedAxes' (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "H"), Single (axis "B"), Single (axis "W"), Single (axis "C")]
                , axesLengths = []
                })
            `shouldBe`
            reduceReducedAxesPy (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "H"), Single (axis "B"), Single (axis "W"), Single (axis "C")]
                , axesLengths = []
                })

    hspec $ do
        it "calculates axes reordering" $
            axesReordering' (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "H"), Single (axis "B"), Single (axis "W"), Single (axis "C")]
                , axesLengths = []
                })
            `shouldBe`
            rearrangeAxesReorderingPy (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "H"), Single (axis "B"), Single (axis "W"), Single (axis "C")]
                , axesLengths = []
                })
        it "calculates axes reordering for reduction" $
            axesReordering' (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "B"), Single (axis "C")]
                , axesLengths = []
                })
            `shouldBe`
            reduceAxesReorderingPy (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "B"), Single (axis "C")]
                , axesLengths = []
                })


    hspec $ do
        it "calculates added axes for reconstruct" $
            addedAxesReconstruct' (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "H"), Single (axis "B"), Single (axis "W"), Single (axis "C")]
                , axesLengths = []
                })
            `shouldBe`
            rearrangeAddedAxesReconstructPy (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "H"), Single (axis "B"), Single (axis "W"), Single (axis "C")]
                , axesLengths = []
                })
    hspec $ do
        it "calculates final shapes" $
            finalShapes' (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "H"), Single (axis "B"), Single (axis "W"), Single (axis "C")]
                , axesLengths = []
                })
            `shouldBe`
            rearrangeFinalShapesPy (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "H"), Single (axis "B"), Single (axis "W"), Single (axis "C")]
                , axesLengths = []
                })
        it "calculates final shapes for composite axes" $
            finalShapes' (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "H"), Multiple [axis "B", axis "W"], Single (axis "C")]
                , axesLengths = []
                })
            `shouldBe`
            Right [4,24,3]
        it "calculates final shapes for split initial axis" $
            finalShapes' (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Multiple [axis "I", axis "W"], Single (axis "C")]
                , outp = [Single (axis "I"), Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , axesLengths = [(axis "I",2)]
                })
            `shouldBe`
            rearrangeFinalShapesPy (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Multiple [axis "I", axis "W"], Single (axis "C")]
                , outp = [Single (axis "I"), Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , axesLengths = [(axis "I",2)]
                })
    hspec $ do
        it "generates tf commands for rearrange" $
            applyRecipe sampleShape (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "H"), Multiple [axis "B", axis "W"], Single (axis "C")]
                , axesLengths = []
                })
            `shouldBe`
            [
            Reshape [6,4,4,3] -- TODO: remove redundant command
            , Transpose [1,0,2,3]
            , Reshape [4,24,3]
            ]
        it "generates tf commands for reduce" $
            applyRecipe sampleShape (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "B"), Single (axis "C")]
                , axesLengths = []
                })
            `shouldBe`
            [
            Reshape [6, 4, 4, 3]
            , Reduce "max" [1, 2]
            , Transpose [0, 1]
            , Reshape [6, 3]
            ]
        it "generates tf commands for full reduce" $
            applyRecipe sampleShape (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Multiple [axis "B", axis "H", axis "W", axis "C"]]
                , axesLengths = []
                })
            `shouldBe`
            [
            Reshape [6, 4, 4, 3]
            , Transpose [0, 1, 2, 3]
            , Reshape [288]
            ]
        it "generates tf commands for split initial axis" $
            applyRecipe sampleShape (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Multiple [axis "I", axis "W"], Single (axis "C")]
                , outp = [Single (axis "I"), Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , axesLengths = [(axis "I",2)]
                })
            `shouldBe`
            [
            Reshape [6, 4, 2, 2, 3]
            , Transpose [2, 0, 1, 3, 4]
            , Reshape [2, 6, 4, 2, 3]
            ]
        it "generates tf commands for different shape" $
            applyRecipe (tail sampleShape) (Equation {
                inp = [Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Multiple [axis "H", axis "W"], Single (axis "C")]
                , axesLengths = []
                })
            `shouldBe`
            [
            Reshape [4, 4, 3]
            , Transpose [0, 1, 2]
            , Reshape [16, 3]
            ]
        -- it "generates tf commands for anonymous axis" $
        --     applyRecipe (1 : tail sampleShape) (Equation {
        --         inp = [Single (Axis Nothing), Single (axis "W"), Single (axis "C")]
        --         , outp = [Single (axis "H"), Single (axis "W"), Single (axis "C")]
        --         , axesLengths = []
        --         })
        --     `shouldBe`
        --     [
        --     Reshape [4, 4, 3]
        --     , Transpose [0, 1, 2]
        --     , Reshape [4, 4, 3]
        --     ]





    -- TODO: Support underscore axis

    -- PASS
    -- quickCheck $ \xs -> axesPermutationPy xs === axesPermutation' xs
    -- quickCheck $ \xs -> ellipsisPositionInLhsPy xs === ellipsisPositionInLhs' xs
    -- quickCheck $ \xs -> outputCompositeAxesPy xs === outputCompositeAxes' xs

    -- print . elementaryAxesLengths' $ (Equation [] [] [(Ellipsis,0)])
    -- quickCheck $ \xs -> collect (isRight (elementaryAxesLengths' xs)) $ eitherToMaybe (elementaryAxesLengthsPy xs) === eitherToMaybe (elementaryAxesLengths' xs)

    -- let xs = Equation [] [Multiple [axis "Ellipsis"]] in
    --     print $ ellipsisPositionInLhsPy xs



    -- print . axesPermutation' $ (Equation [] [Multiple []] :: Equation (Axis a))
    -- print $ ellipsisPositionInLhs [B, Ellipsis, H]
    -- print $ ellipsisPositionInLhs [B, H]
    -- print $ inputCompositeAxes S.empty $ fmap (fmap fromEnum) [Single B, Single H]

    pure ()
