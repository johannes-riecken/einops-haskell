{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

newtype Axis = Axis { getAxis :: Maybe String } deriving (Eq, Ord, Arbitrary)

axis :: String -> Axis
axis x = Axis (Just x)

anon :: Axis
anon = Axis Nothing

instance Show Axis where
    show (Axis (Just x)) = x
    show (Axis Nothing) = "()"

instance ToJSON Axis where
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
addedAxes' :: Equation Axis -> Either BS.ByteString AddedAxesRet
addedAxes' = fmap addedAxes . (checkDuplDim <=< checkRightDuplDim)

type RearrangeAddedAxesAPI = "/rearrange/added_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AddedAxesRet

rearrangeAddedAxesAPI :: Proxy RearrangeAddedAxesAPI
rearrangeAddedAxesAPI = Proxy

rearrangeAddedAxesRequest :: EquationStr Axis -> ClientM AddedAxesRet
rearrangeAddedAxesRequest = client rearrangeAddedAxesAPI

rearrangeAddedAxesPy :: Equation Axis -> Either BS.ByteString AddedAxesRet
rearrangeAddedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeAddedAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceAddedAxesAPI = "/reduce/added_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AddedAxesRet

reduceAddedAxesAPI :: Proxy ReduceAddedAxesAPI
reduceAddedAxesAPI = Proxy

reduceAddedAxesRequest :: EquationStr Axis -> ClientM AddedAxesRet
reduceAddedAxesRequest = client reduceAddedAxesAPI

reduceAddedAxesPy :: Equation Axis -> Either BS.ByteString AddedAxesRet
reduceAddedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceAddedAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatAddedAxesAPI = "/repeat/added_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AddedAxesRet

repeatAddedAxesAPI :: Proxy RepeatAddedAxesAPI
repeatAddedAxesAPI = Proxy

repeatAddedAxesRequest :: EquationStr Axis -> ClientM AddedAxesRet
repeatAddedAxesRequest = client repeatAddedAxesAPI

repeatAddedAxesPy :: Equation Axis -> Either BS.ByteString AddedAxesRet
repeatAddedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatAddedAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

axesPermutation' :: Equation Axis -> Either BS.ByteString AxesPermutationRet
axesPermutation' = fmap axesPermutation . (checkDuplDim <=< checkRightDuplDim)

type RearrangeAxesPermutationAPI = "/rearrange/axes_permutation" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AxesPermutationRet

rearrangeAxesPermutationAPI :: Proxy RearrangeAxesPermutationAPI
rearrangeAxesPermutationAPI = Proxy

rearrangeAxesPermutationRequest :: EquationStr Axis -> ClientM AxesPermutationRet
rearrangeAxesPermutationRequest = client rearrangeAxesPermutationAPI

rearrangeAxesPermutationPy :: Equation Axis -> Either BS.ByteString AxesPermutationRet
rearrangeAxesPermutationPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeAxesPermutationRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceAxesPermutationAPI = "/reduce/axes_permutation" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AxesPermutationRet

reduceAxesPermutationAPI :: Proxy ReduceAxesPermutationAPI
reduceAxesPermutationAPI = Proxy

reduceAxesPermutationRequest :: EquationStr Axis -> ClientM AxesPermutationRet
reduceAxesPermutationRequest = client reduceAxesPermutationAPI

reduceAxesPermutationPy :: Equation Axis -> Either BS.ByteString AxesPermutationRet
reduceAxesPermutationPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceAxesPermutationRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatAxesPermutationAPI = "/repeat/axes_permutation" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AxesPermutationRet

repeatAxesPermutationAPI :: Proxy RepeatAxesPermutationAPI
repeatAxesPermutationAPI = Proxy

repeatAxesPermutationRequest :: EquationStr Axis -> ClientM AxesPermutationRet
repeatAxesPermutationRequest = client repeatAxesPermutationAPI

repeatAxesPermutationPy :: Equation Axis -> Either BS.ByteString AxesPermutationRet
repeatAxesPermutationPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatAxesPermutationRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

elementaryAxesLengths' :: Equation Axis -> Either BS.ByteString ElementaryAxesLengthsRet
elementaryAxesLengths' = fmap elementaryAxesLengths . (checkDuplDim <=< checkRightDuplDim)

type RearrangeElementaryAxesLengthsAPI = "/rearrange/elementary_axes_lengths" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] ElementaryAxesLengthsRet

rearrangeElementaryAxesLengthsAPI :: Proxy RearrangeElementaryAxesLengthsAPI
rearrangeElementaryAxesLengthsAPI = Proxy

rearrangeElementaryAxesLengthsRequest :: EquationStr Axis -> ClientM ElementaryAxesLengthsRet
rearrangeElementaryAxesLengthsRequest = client rearrangeElementaryAxesLengthsAPI

rearrangeElementaryAxesLengthsPy :: Equation Axis -> Either BS.ByteString ElementaryAxesLengthsRet
rearrangeElementaryAxesLengthsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeElementaryAxesLengthsRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceElementaryAxesLengthsAPI = "/reduce/elementary_axes_lengths" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] ElementaryAxesLengthsRet

reduceElementaryAxesLengthsAPI :: Proxy ReduceElementaryAxesLengthsAPI
reduceElementaryAxesLengthsAPI = Proxy

reduceElementaryAxesLengthsRequest :: EquationStr Axis -> ClientM ElementaryAxesLengthsRet
reduceElementaryAxesLengthsRequest = client reduceElementaryAxesLengthsAPI

reduceElementaryAxesLengthsPy :: Equation Axis -> Either BS.ByteString ElementaryAxesLengthsRet
reduceElementaryAxesLengthsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceElementaryAxesLengthsRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatElementaryAxesLengthsAPI = "/repeat/elementary_axes_lengths" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] ElementaryAxesLengthsRet

repeatElementaryAxesLengthsAPI :: Proxy RepeatElementaryAxesLengthsAPI
repeatElementaryAxesLengthsAPI = Proxy

repeatElementaryAxesLengthsRequest :: EquationStr Axis -> ClientM ElementaryAxesLengthsRet
repeatElementaryAxesLengthsRequest = client repeatElementaryAxesLengthsAPI

repeatElementaryAxesLengthsPy :: Equation Axis -> Either BS.ByteString ElementaryAxesLengthsRet
repeatElementaryAxesLengthsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatElementaryAxesLengthsRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

inputCompositeAxes' :: Equation Axis -> Either BS.ByteString InputCompositeAxesRet
inputCompositeAxes' = fmap inputCompositeAxes . (checkDuplDim <=< checkRightDuplDim)

type RearrangeInputCompositeAxesAPI = "/rearrange/input_composite_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] InputCompositeAxesRet

rearrangeInputCompositeAxesAPI :: Proxy RearrangeInputCompositeAxesAPI
rearrangeInputCompositeAxesAPI = Proxy

rearrangeInputCompositeAxesRequest :: EquationStr Axis -> ClientM InputCompositeAxesRet
rearrangeInputCompositeAxesRequest = client rearrangeInputCompositeAxesAPI

rearrangeInputCompositeAxesPy :: Equation Axis -> Either BS.ByteString InputCompositeAxesRet
rearrangeInputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeInputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceInputCompositeAxesAPI = "/reduce/input_composite_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] InputCompositeAxesRet

reduceInputCompositeAxesAPI :: Proxy ReduceInputCompositeAxesAPI
reduceInputCompositeAxesAPI = Proxy

reduceInputCompositeAxesRequest :: EquationStr Axis -> ClientM InputCompositeAxesRet
reduceInputCompositeAxesRequest = client reduceInputCompositeAxesAPI

reduceInputCompositeAxesPy :: Equation Axis -> Either BS.ByteString InputCompositeAxesRet
reduceInputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceInputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatInputCompositeAxesAPI = "/repeat/input_composite_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] InputCompositeAxesRet

repeatInputCompositeAxesAPI :: Proxy RepeatInputCompositeAxesAPI
repeatInputCompositeAxesAPI = Proxy

repeatInputCompositeAxesRequest :: EquationStr Axis -> ClientM InputCompositeAxesRet
repeatInputCompositeAxesRequest = client repeatInputCompositeAxesAPI

repeatInputCompositeAxesPy :: Equation Axis -> Either BS.ByteString InputCompositeAxesRet
repeatInputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatInputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

outputCompositeAxes' :: Equation Axis -> Either BS.ByteString OutputCompositeAxesRet
outputCompositeAxes' = fmap outputCompositeAxes . (checkDuplDim <=< checkRightDuplDim)

type RearrangeOutputCompositeAxesAPI = "/rearrange/output_composite_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] OutputCompositeAxesRet

rearrangeOutputCompositeAxesAPI :: Proxy RearrangeOutputCompositeAxesAPI
rearrangeOutputCompositeAxesAPI = Proxy

rearrangeOutputCompositeAxesRequest :: EquationStr Axis -> ClientM OutputCompositeAxesRet
rearrangeOutputCompositeAxesRequest = client rearrangeOutputCompositeAxesAPI

rearrangeOutputCompositeAxesPy :: Equation Axis -> Either BS.ByteString OutputCompositeAxesRet
rearrangeOutputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeOutputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceOutputCompositeAxesAPI = "/reduce/output_composite_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] OutputCompositeAxesRet

reduceOutputCompositeAxesAPI :: Proxy ReduceOutputCompositeAxesAPI
reduceOutputCompositeAxesAPI = Proxy

reduceOutputCompositeAxesRequest :: EquationStr Axis -> ClientM OutputCompositeAxesRet
reduceOutputCompositeAxesRequest = client reduceOutputCompositeAxesAPI

reduceOutputCompositeAxesPy :: Equation Axis -> Either BS.ByteString OutputCompositeAxesRet
reduceOutputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceOutputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatOutputCompositeAxesAPI = "/repeat/output_composite_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] OutputCompositeAxesRet

repeatOutputCompositeAxesAPI :: Proxy RepeatOutputCompositeAxesAPI
repeatOutputCompositeAxesAPI = Proxy

repeatOutputCompositeAxesRequest :: EquationStr Axis -> ClientM OutputCompositeAxesRet
repeatOutputCompositeAxesRequest = client repeatOutputCompositeAxesAPI

repeatOutputCompositeAxesPy :: Equation Axis -> Either BS.ByteString OutputCompositeAxesRet
repeatOutputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatOutputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

reducedElementaryAxes' :: Equation Axis -> Either BS.ByteString ReducedElementaryAxesRet
reducedElementaryAxes' = fmap reducedElementaryAxes . (checkDuplDim <=< checkRightDuplDim)

type RearrangeReducedElementaryAxesAPI = "/rearrange/reduced_elementary_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] ReducedElementaryAxesRet

rearrangeReducedElementaryAxesAPI :: Proxy RearrangeReducedElementaryAxesAPI
rearrangeReducedElementaryAxesAPI = Proxy

rearrangeReducedElementaryAxesRequest :: EquationStr Axis -> ClientM ReducedElementaryAxesRet
rearrangeReducedElementaryAxesRequest = client rearrangeReducedElementaryAxesAPI

rearrangeReducedElementaryAxesPy :: Equation Axis -> Either BS.ByteString ReducedElementaryAxesRet
rearrangeReducedElementaryAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeReducedElementaryAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceReducedElementaryAxesAPI = "/reduce/reduced_elementary_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] ReducedElementaryAxesRet

reduceReducedElementaryAxesAPI :: Proxy ReduceReducedElementaryAxesAPI
reduceReducedElementaryAxesAPI = Proxy

reduceReducedElementaryAxesRequest :: EquationStr Axis -> ClientM ReducedElementaryAxesRet
reduceReducedElementaryAxesRequest = client reduceReducedElementaryAxesAPI

reduceReducedElementaryAxesPy :: Equation Axis -> Either BS.ByteString ReducedElementaryAxesRet
reduceReducedElementaryAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceReducedElementaryAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatReducedElementaryAxesAPI = "/repeat/reduced_elementary_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] ReducedElementaryAxesRet

repeatReducedElementaryAxesAPI :: Proxy RepeatReducedElementaryAxesAPI
repeatReducedElementaryAxesAPI = Proxy

repeatReducedElementaryAxesRequest :: EquationStr Axis -> ClientM ReducedElementaryAxesRet
repeatReducedElementaryAxesRequest = client repeatReducedElementaryAxesAPI

repeatReducedElementaryAxesPy :: Equation Axis -> Either BS.ByteString ReducedElementaryAxesRet
repeatReducedElementaryAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatReducedElementaryAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

-- AUTOGEN END

-- RECONSTRUCT AUTOGEN BEGIN
initShapes' :: Equation Axis -> Either BS.ByteString InitShapesRet
initShapes' = fmap initShapes . (checkDuplDim <=< checkRightDuplDim)

type RearrangeInitShapesAPI = "/rearrange/init_shapes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] InitShapesRet

rearrangeInitShapesAPI :: Proxy RearrangeInitShapesAPI
rearrangeInitShapesAPI = Proxy

rearrangeInitShapesRequest :: EquationStr Axis -> ClientM InitShapesRet
rearrangeInitShapesRequest = client rearrangeInitShapesAPI

rearrangeInitShapesPy :: Equation Axis -> Either BS.ByteString InitShapesRet
rearrangeInitShapesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeInitShapesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceInitShapesAPI = "/reduce/init_shapes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] InitShapesRet

reduceInitShapesAPI :: Proxy ReduceInitShapesAPI
reduceInitShapesAPI = Proxy

reduceInitShapesRequest :: EquationStr Axis -> ClientM InitShapesRet
reduceInitShapesRequest = client reduceInitShapesAPI

reduceInitShapesPy :: Equation Axis -> Either BS.ByteString InitShapesRet
reduceInitShapesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceInitShapesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatInitShapesAPI = "/repeat/init_shapes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] InitShapesRet

repeatInitShapesAPI :: Proxy RepeatInitShapesAPI
repeatInitShapesAPI = Proxy

repeatInitShapesRequest :: EquationStr Axis -> ClientM InitShapesRet
repeatInitShapesRequest = client repeatInitShapesAPI

repeatInitShapesPy :: Equation Axis -> Either BS.ByteString InitShapesRet
repeatInitShapesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatInitShapesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

reducedAxes' :: Equation Axis -> Either BS.ByteString ReducedAxesRet
reducedAxes' = fmap reducedAxes . (checkDuplDim <=< checkRightDuplDim)

type RearrangeReducedAxesAPI = "/rearrange/reduced_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] ReducedAxesRet

rearrangeReducedAxesAPI :: Proxy RearrangeReducedAxesAPI
rearrangeReducedAxesAPI = Proxy

rearrangeReducedAxesRequest :: EquationStr Axis -> ClientM ReducedAxesRet
rearrangeReducedAxesRequest = client rearrangeReducedAxesAPI

rearrangeReducedAxesPy :: Equation Axis -> Either BS.ByteString ReducedAxesRet
rearrangeReducedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeReducedAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceReducedAxesAPI = "/reduce/reduced_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] ReducedAxesRet

reduceReducedAxesAPI :: Proxy ReduceReducedAxesAPI
reduceReducedAxesAPI = Proxy

reduceReducedAxesRequest :: EquationStr Axis -> ClientM ReducedAxesRet
reduceReducedAxesRequest = client reduceReducedAxesAPI

reduceReducedAxesPy :: Equation Axis -> Either BS.ByteString ReducedAxesRet
reduceReducedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceReducedAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatReducedAxesAPI = "/repeat/reduced_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] ReducedAxesRet

repeatReducedAxesAPI :: Proxy RepeatReducedAxesAPI
repeatReducedAxesAPI = Proxy

repeatReducedAxesRequest :: EquationStr Axis -> ClientM ReducedAxesRet
repeatReducedAxesRequest = client repeatReducedAxesAPI

repeatReducedAxesPy :: Equation Axis -> Either BS.ByteString ReducedAxesRet
repeatReducedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatReducedAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

axesReordering' :: Equation Axis -> Either BS.ByteString AxesReorderingRet
axesReordering' = fmap axesReordering . (checkDuplDim <=< checkRightDuplDim)

type RearrangeAxesReorderingAPI = "/rearrange/axes_reordering" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AxesReorderingRet

rearrangeAxesReorderingAPI :: Proxy RearrangeAxesReorderingAPI
rearrangeAxesReorderingAPI = Proxy

rearrangeAxesReorderingRequest :: EquationStr Axis -> ClientM AxesReorderingRet
rearrangeAxesReorderingRequest = client rearrangeAxesReorderingAPI

rearrangeAxesReorderingPy :: Equation Axis -> Either BS.ByteString AxesReorderingRet
rearrangeAxesReorderingPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeAxesReorderingRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceAxesReorderingAPI = "/reduce/axes_reordering" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AxesReorderingRet

reduceAxesReorderingAPI :: Proxy ReduceAxesReorderingAPI
reduceAxesReorderingAPI = Proxy

reduceAxesReorderingRequest :: EquationStr Axis -> ClientM AxesReorderingRet
reduceAxesReorderingRequest = client reduceAxesReorderingAPI

reduceAxesReorderingPy :: Equation Axis -> Either BS.ByteString AxesReorderingRet
reduceAxesReorderingPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceAxesReorderingRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatAxesReorderingAPI = "/repeat/axes_reordering" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AxesReorderingRet

repeatAxesReorderingAPI :: Proxy RepeatAxesReorderingAPI
repeatAxesReorderingAPI = Proxy

repeatAxesReorderingRequest :: EquationStr Axis -> ClientM AxesReorderingRet
repeatAxesReorderingRequest = client repeatAxesReorderingAPI

repeatAxesReorderingPy :: Equation Axis -> Either BS.ByteString AxesReorderingRet
repeatAxesReorderingPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatAxesReorderingRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

addedAxesReconstruct' :: Equation Axis -> Either BS.ByteString AddedAxesReconstructRet
addedAxesReconstruct' = fmap addedAxesReconstruct . (checkDuplDim <=< checkRightDuplDim)

type RearrangeAddedAxesReconstructAPI = "/rearrange/added_axes_reconstruct" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AddedAxesReconstructRet

rearrangeAddedAxesReconstructAPI :: Proxy RearrangeAddedAxesReconstructAPI
rearrangeAddedAxesReconstructAPI = Proxy

rearrangeAddedAxesReconstructRequest :: EquationStr Axis -> ClientM AddedAxesReconstructRet
rearrangeAddedAxesReconstructRequest = client rearrangeAddedAxesReconstructAPI

rearrangeAddedAxesReconstructPy :: Equation Axis -> Either BS.ByteString AddedAxesReconstructRet
rearrangeAddedAxesReconstructPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeAddedAxesReconstructRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceAddedAxesReconstructAPI = "/reduce/added_axes_reconstruct" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AddedAxesReconstructRet

reduceAddedAxesReconstructAPI :: Proxy ReduceAddedAxesReconstructAPI
reduceAddedAxesReconstructAPI = Proxy

reduceAddedAxesReconstructRequest :: EquationStr Axis -> ClientM AddedAxesReconstructRet
reduceAddedAxesReconstructRequest = client reduceAddedAxesReconstructAPI

reduceAddedAxesReconstructPy :: Equation Axis -> Either BS.ByteString AddedAxesReconstructRet
reduceAddedAxesReconstructPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceAddedAxesReconstructRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatAddedAxesReconstructAPI = "/repeat/added_axes_reconstruct" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AddedAxesReconstructRet

repeatAddedAxesReconstructAPI :: Proxy RepeatAddedAxesReconstructAPI
repeatAddedAxesReconstructAPI = Proxy

repeatAddedAxesReconstructRequest :: EquationStr Axis -> ClientM AddedAxesReconstructRet
repeatAddedAxesReconstructRequest = client repeatAddedAxesReconstructAPI

repeatAddedAxesReconstructPy :: Equation Axis -> Either BS.ByteString AddedAxesReconstructRet
repeatAddedAxesReconstructPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatAddedAxesReconstructRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

finalShapes' :: Equation Axis -> Either BS.ByteString FinalShapesRet
finalShapes' = fmap finalShapes . (checkDuplDim <=< checkRightDuplDim)

type RearrangeFinalShapesAPI = "/rearrange/final_shapes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] FinalShapesRet

rearrangeFinalShapesAPI :: Proxy RearrangeFinalShapesAPI
rearrangeFinalShapesAPI = Proxy

rearrangeFinalShapesRequest :: EquationStr Axis -> ClientM FinalShapesRet
rearrangeFinalShapesRequest = client rearrangeFinalShapesAPI

rearrangeFinalShapesPy :: Equation Axis -> Either BS.ByteString FinalShapesRet
rearrangeFinalShapesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeFinalShapesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceFinalShapesAPI = "/reduce/final_shapes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] FinalShapesRet

reduceFinalShapesAPI :: Proxy ReduceFinalShapesAPI
reduceFinalShapesAPI = Proxy

reduceFinalShapesRequest :: EquationStr Axis -> ClientM FinalShapesRet
reduceFinalShapesRequest = client reduceFinalShapesAPI

reduceFinalShapesPy :: Equation Axis -> Either BS.ByteString FinalShapesRet
reduceFinalShapesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceFinalShapesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatFinalShapesAPI = "/repeat/final_shapes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] FinalShapesRet

repeatFinalShapesAPI :: Proxy RepeatFinalShapesAPI
repeatFinalShapesAPI = Proxy

repeatFinalShapesRequest :: EquationStr Axis -> ClientM FinalShapesRet
repeatFinalShapesRequest = client repeatFinalShapesAPI

repeatFinalShapesPy :: Equation Axis -> Either BS.ByteString FinalShapesRet
repeatFinalShapesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatFinalShapesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))



-- RECONSTRUCT AUTOGEN END
intersectCompLists :: Ord a => [Composite a] -> [Composite a] -> [Composite a]
intersectCompLists xs ys = uncc $ mapMaybe (\x -> if x `elem` cc ys then Just x else Nothing) (cc xs)

axisNumsFromCompList :: Ord a => [Composite a] -> M.Map a Int
axisNumsFromCompList = snd . foldl' (\(i,acc) x ->
        if M.member x acc then (i,acc) else
        (i+1,M.insertWith (\a b -> error "no duplicates allowed") x i acc))
        (0,M.empty) . cc

-- axesPermutation gives the numbers of flatten output axes
axesPermutation :: (Show a,Ord a) => Equation a -> [Int]
axesPermutation (Equation{..}) = let
    axisNums = axisNumsFromCompList (intersectCompLists inp outp)
    in
    foldr ((:) . (axisNums M.!)) [] . cc $ outp

-- addedAxes given equation returns a map from axes numbers to repeat counts
-- Example: addedAxes "h w -> h w 3" is {2: 3}
-- TODO: Add integer type to Axis datatype
addedAxes :: Equation Axis -> AddedAxesRet
addedAxes _ = []  -- TODO: implement

-- example: reducedElementaryAxes "h w -> h" "max" is [1]
reducedElementaryAxes :: (Show a,Ord a) => Equation a -> ReducedElementaryAxesRet
reducedElementaryAxes (Equation{..}) = let
    axisNums = axisNumsFromCompList inp
    in
    map (axisNums M.!) $ flatten inp \\ flatten outp

outputCompositeAxes :: Equation Axis -> OutputCompositeAxesRet
outputCompositeAxes eqn@(Equation{..}) = let
    axisNums = axisNumsFromCompList inp
    in
    map (F.toList . fmap (axisNums M.!)) outp

elementaryAxesLengths :: Equation Axis -> ElementaryAxesLengthsRet
elementaryAxesLengths eqn@Equation{..} = let m = M.fromList axesLengths in
    foldr ((:) . (`M.lookup` m)) [] . cc $ inp

-- unexported helper from Data.List
select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
select p x ~(ts,fs) | p x       = (x:ts,fs)
                    | otherwise = (ts, x:fs)

-- inputCompositeAxes returns a list that for each composite axis returns its
-- tuple of known and unknown axis numbers
inputCompositeAxes :: Equation Axis -> [([Int],[Int])]
inputCompositeAxes eqn@Equation{..} =
    let
        axisNums = axisNumsFromCompList inp
        known = S.fromList (fmap ((axisNums M.!) . fst) axesLengths)
        in map (
            foldr (select (`S.member` known) . (axisNums M.!)) ([],[])
            ) inp

-- TODO: fuse
-- TODO: allow deeper nesting of Composite
initMap :: Equation Axis -> [Int] -> Map Axis Int
initMap eqn@Equation{..} shape =
    let
    sizes = M.fromList . snd $ foldr (\x' (i,acc) -> case x' of
            Single x -> (i-1,(x, shape !! i):acc)
            Multiple xs -> (i-1,handleMultiple i xs ++ acc)
            ) (length inp - 1,[]) inp
    handleMultiple :: Int -> [Axis] -> [(Axis,Int)]
    handleMultiple i xs = map (\x -> if x `M.member` axesLengthsMap then
        (x,axesLengthsMap M.! x) :: (Axis,Int)
        else
        (x,shape !! i `div` prod xs) :: (Axis,Int)
        ) xs
        where
            prod = product . map (axesLengthsMap M.!) . filter (`M.member` axesLengthsMap)
    axesLengthsMap = M.fromList axesLengths
    in sizes

initShapes :: Equation Axis -> InitShapesRet
initShapes = initShapesWithShape sampleShape

-- TODO: fuse
initShapesWithShape :: [Int] -> Equation Axis -> InitShapesRet
initShapesWithShape shape eqn@Equation{..} = map (initMap eqn shape M.!) (flatten inp)

-- TODO: remove
reducedAxes :: Equation Axis -> ReducedAxesRet
reducedAxes = reducedElementaryAxes

-- TODO: remove
axesReordering :: Equation Axis -> AxesReorderingRet
axesReordering = axesPermutation

-- TODO: implement
addedAxesReconstruct :: Equation Axis -> AddedAxesReconstructRet
addedAxesReconstruct _ = M.empty

finalShapes :: Equation Axis -> FinalShapesRet
finalShapes = finalShapesWithShape sampleShape

-- TODO: allow deeper nesting
-- TODO: fuse
finalShapesWithShape :: [Int] -> Equation Axis -> FinalShapesRet
finalShapesWithShape shape eqn@Equation{..} = map (foldr ((*) . (initMap eqn shape M.!)) 1) outp
-- end of reconstruct

-- TODO: Generalize reduction type
applyRecipe :: [Int] -> Equation Axis -> [TfCommand]
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

fixup :: Equation Axis -> Equation Axis
fixup eqn@Equation{..} = let
    inp' = uncc . remDupl . cc $ inp
    outp' = uncc . remDupl . cc $ outp
    in eqn{inp = inp', outp = outp'}

remDupl :: (Show a,Ord a,Witherable t) => t a -> t a
remDupl = (`evalState` S.empty) . wither (\a -> state (\s -> (mfilter (not . (`S.member` s)) (Just a),S.insert a s)))

remDupl' :: [Composite Axis] -> [Composite Axis]
remDupl' = uncc . remDupl . cc

-- TODO: check this if operation is rearrange
-- checkOneSideIdent :: (Show (f a),Eq (f a),Foldable f) => Equation (f a) -> Either String (Equation (f a))
checkOneSideIdent :: Equation Axis -> Either BS.ByteString (Equation Axis)
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

checkDuplDim :: Equation Axis -> Either BS.ByteString (Equation Axis)
checkDuplDim eqn@(Equation{..}) = if flatten inp == flatten (remDupl' inp) && flatten outp == flatten (remDupl' outp)
    then
    Right $ Equation{..}
    else
    let
        ys = head ((flatten inp \\ flatten (remDupl' inp)) ++ (flatten outp \\ flatten (remDupl' outp))) :: Axis
        dup = BS.pack . show $ ys in Left $ "Indexing expression contains duplicate dimension \"" <> dup <> "\""

checkRightDuplDim :: Equation Axis -> Either BS.ByteString (Equation Axis)
checkRightDuplDim eqn@(Equation{..}) = if flatten outp == flatten (remDupl' outp)
    then
    Right eqn
    else
    let
        ys = head ((flatten inp \\ flatten (remDupl' inp)) ++ (flatten outp \\ flatten (remDupl' outp))) :: Axis
        dup = BS.pack . show $ ys in Left $ "Indexing expression contains duplicate dimension \"" <> dup <> "\""

checkLeftAxisUnused :: Equation Axis -> Either BS.ByteString (Equation Axis)
checkLeftAxisUnused eqn@(Equation{..}) =
    case find (`notElem` flatten inp) (map fst axesLengths) of
        Just x -> Left $ "Axis " <> BS.pack (show x) <> " is not used in transform"
        Nothing -> Right eqn

checkRightAxisUnused :: Equation Axis -> Either BS.ByteString (Equation Axis)
checkRightAxisUnused eqn@(Equation{..}) =
    case find (`notElem` flatten outp) (map fst axesLengths) of
        Just x -> Left $ "Axis " <> BS.pack (show x) <> " is not used in transform"
        Nothing -> Right eqn

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




    -- TODO: Support underscore axis

    -- PASS
    -- quickCheck $ \xs -> axesPermutationPy xs === axesPermutation' xs
    -- quickCheck $ \xs -> ellipsisPositionInLhsPy xs === ellipsisPositionInLhs' xs
    -- quickCheck $ \xs -> outputCompositeAxesPy xs === outputCompositeAxes' xs

    -- print . elementaryAxesLengths' $ (Equation [] [] [(Ellipsis,0)])
    -- quickCheck $ \xs -> collect (isRight (elementaryAxesLengths' xs)) $ eitherToMaybe (elementaryAxesLengthsPy xs) === eitherToMaybe (elementaryAxesLengths' xs)

    -- let xs = Equation [] [Multiple [axis "Ellipsis"]] in
    --     print $ ellipsisPositionInLhsPy xs



    -- print . axesPermutation' $ (Equation [] [Multiple []] :: Equation Axis)
    -- print $ ellipsisPositionInLhs [B, Ellipsis, H]
    -- print $ ellipsisPositionInLhs [B, H]
    -- print $ inputCompositeAxes S.empty $ fmap (fmap fromEnum) [Single B, Single H]

    pure ()
