{-# LANGUAGE RecordWildCards, DeriveTraversable, LambdaCase #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DataKinds, TypeOperators #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
import Control.Arrow
import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans.State
import Data.Aeson hiding ((.:))
import qualified Data.ByteString.Char8 as BS (isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Either (isRight, fromRight)
import Data.Either.Extra (eitherToMaybe)
import qualified Data.Foldable as F
import Data.Tensor
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
import Witherable hiding (filter)

data Axis = I
    | J
    | I0
    | I1
    | Ellipsis
    -- | Anon Int -- TODO: Add to Arbitrary instance and deal with Bounded
    deriving (Eq, Ord, Bounded, Enum, Generic)

-- TODO: Figure out how to use this in Servant
data Action =
    Rearrange
    | Reduce
    | Repeat
    deriving (Eq, Ord, Show)

instance Show Axis where
    show I = "i"
    show J = "j"
    show I0 = "i0"
    show I1 = "i1"
    -- show Ellipsis = "…"
    show Ellipsis = "..."
    -- show (Anon x) = show x

instance Arbitrary Axis where
    arbitrary = elements [I, J, Ellipsis]
    shrink = genericShrink

instance ToJSON Axis

data Composite a = Single a | Multiple [a] deriving (Functor, Foldable, Traversable, Show, Generic, Eq, Ord)

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
type EllipsisPositionInLhsRet = Maybe Int
type OutputCompositeAxesRet = [[Int]]
type ElementaryAxesLengthsRet = [Maybe Int]
type InputCompositeAxesRet = [([Int], [Int])]
type ReducedElementaryAxesRet = [Int]

type RearrangeAxesPermutationAPI = "/rearrange/axes_permutation" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AxesPermutationRet
type ReduceAxesPermutationAPI = "/reduce/axes_permutation" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AxesPermutationRet
type RepeatAxesPermutationAPI = "/repeat/axes_permutation" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AxesPermutationRet

rearrangeAxesPermutationAPI :: Proxy RearrangeAxesPermutationAPI
rearrangeAxesPermutationAPI = Proxy

reduceAxesPermutationAPI :: Proxy ReduceAxesPermutationAPI
reduceAxesPermutationAPI = Proxy

repeatAxesPermutationAPI :: Proxy RepeatAxesPermutationAPI
repeatAxesPermutationAPI = Proxy

rearrangeAxesPermutationRequest :: EquationStr Axis -> ClientM AxesPermutationRet
rearrangeAxesPermutationRequest = client rearrangeAxesPermutationAPI

rearrangeAxesPermutationPy :: Equation Axis -> Either BS.ByteString AxesPermutationRet
rearrangeAxesPermutationPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeAxesPermutationRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

rearrangeAxesPermutation' :: Equation Axis -> Either BS.ByteString AxesPermutationRet
rearrangeAxesPermutation' = fmap axesPermutation . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

reduceAxesPermutationRequest :: EquationStr Axis -> ClientM AxesPermutationRet
reduceAxesPermutationRequest = client reduceAxesPermutationAPI

reduceAxesPermutationPy :: Equation Axis -> Either BS.ByteString AxesPermutationRet
reduceAxesPermutationPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceAxesPermutationRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

reduceAxesPermutation' :: Equation Axis -> Either BS.ByteString AxesPermutationRet
reduceAxesPermutation' = fmap axesPermutation . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

repeatAxesPermutationRequest :: EquationStr Axis -> ClientM AxesPermutationRet
repeatAxesPermutationRequest = client repeatAxesPermutationAPI

repeatAxesPermutationPy :: Equation Axis -> Either BS.ByteString AxesPermutationRet
repeatAxesPermutationPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatAxesPermutationRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

repeatAxesPermutation' :: Equation Axis -> Either BS.ByteString AxesPermutationRet
repeatAxesPermutation' = fmap axesPermutation . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

type RearrangeAddedAxesAPI = "/rearrange/added_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AddedAxesRet
type ReduceAddedAxesAPI = "/reduce/added_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AddedAxesRet
type RepeatAddedAxesAPI = "/repeat/added_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] AddedAxesRet

rearrangeAddedAxesAPI :: Proxy RearrangeAddedAxesAPI
rearrangeAddedAxesAPI = Proxy

reduceAddedAxesAPI :: Proxy ReduceAddedAxesAPI
reduceAddedAxesAPI = Proxy

repeatAddedAxesAPI :: Proxy RepeatAddedAxesAPI
repeatAddedAxesAPI = Proxy

rearrangeAddedAxesRequest :: EquationStr Axis -> ClientM AddedAxesRet
rearrangeAddedAxesRequest = client rearrangeAddedAxesAPI

rearrangeAddedAxesPy :: Equation Axis -> Either BS.ByteString AddedAxesRet
rearrangeAddedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeAddedAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

rearrangeAddedAxes' :: Equation Axis -> Either BS.ByteString AddedAxesRet
rearrangeAddedAxes' = fmap addedAxes . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

reduceAddedAxesRequest :: EquationStr Axis -> ClientM AddedAxesRet
reduceAddedAxesRequest = client reduceAddedAxesAPI

reduceAddedAxesPy :: Equation Axis -> Either BS.ByteString AddedAxesRet
reduceAddedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceAddedAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

reduceAddedAxes' :: Equation Axis -> Either BS.ByteString AddedAxesRet
reduceAddedAxes' = fmap addedAxes . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

repeatAddedAxesRequest :: EquationStr Axis -> ClientM AddedAxesRet
repeatAddedAxesRequest = client repeatAddedAxesAPI

repeatAddedAxesPy :: Equation Axis -> Either BS.ByteString AddedAxesRet
repeatAddedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatAddedAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

repeatAddedAxes' :: Equation Axis -> Either BS.ByteString AddedAxesRet
repeatAddedAxes' = fmap addedAxes . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

type RearrangeEllipsisPositionInLhsAPI = "/rearrange/ellipsis_position_in_lhs" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] EllipsisPositionInLhsRet
type ReduceEllipsisPositionInLhsAPI = "/reduce/ellipsis_position_in_lhs" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] EllipsisPositionInLhsRet
type RepeatEllipsisPositionInLhsAPI = "/repeat/ellipsis_position_in_lhs" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] EllipsisPositionInLhsRet

rearrangeEllipsisPositionInLhsAPI :: Proxy RearrangeEllipsisPositionInLhsAPI
rearrangeEllipsisPositionInLhsAPI = Proxy

reduceEllipsisPositionInLhsAPI :: Proxy ReduceEllipsisPositionInLhsAPI
reduceEllipsisPositionInLhsAPI = Proxy

repeatEllipsisPositionInLhsAPI :: Proxy RepeatEllipsisPositionInLhsAPI
repeatEllipsisPositionInLhsAPI = Proxy

rearrangeEllipsisPositionInLhsRequest :: EquationStr Axis -> ClientM EllipsisPositionInLhsRet
rearrangeEllipsisPositionInLhsRequest = client rearrangeEllipsisPositionInLhsAPI

rearrangeEllipsisPositionInLhsPy :: Equation Axis -> Either BS.ByteString EllipsisPositionInLhsRet
rearrangeEllipsisPositionInLhsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeEllipsisPositionInLhsRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

rearrangeEllipsisPositionInLhs' :: Equation Axis -> Either BS.ByteString EllipsisPositionInLhsRet
rearrangeEllipsisPositionInLhs' = fmap ellipsisPositionInLhs . (checkLeftEllipsis <=< checkOneSideIdent <=< checkDuplDim <=< checkRightEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

reduceEllipsisPositionInLhsRequest :: EquationStr Axis -> ClientM EllipsisPositionInLhsRet
reduceEllipsisPositionInLhsRequest = client reduceEllipsisPositionInLhsAPI

reduceEllipsisPositionInLhsPy :: Equation Axis -> Either BS.ByteString EllipsisPositionInLhsRet
reduceEllipsisPositionInLhsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceEllipsisPositionInLhsRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

reduceEllipsisPositionInLhs' :: Equation Axis -> Either BS.ByteString EllipsisPositionInLhsRet
reduceEllipsisPositionInLhs' = fmap ellipsisPositionInLhs . (checkLeftEllipsis <=< checkOneSideIdent <=< checkDuplDim <=< checkRightEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

repeatEllipsisPositionInLhsRequest :: EquationStr Axis -> ClientM EllipsisPositionInLhsRet
repeatEllipsisPositionInLhsRequest = client repeatEllipsisPositionInLhsAPI

repeatEllipsisPositionInLhsPy :: Equation Axis -> Either BS.ByteString EllipsisPositionInLhsRet
repeatEllipsisPositionInLhsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatEllipsisPositionInLhsRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

repeatEllipsisPositionInLhs' :: Equation Axis -> Either BS.ByteString EllipsisPositionInLhsRet
repeatEllipsisPositionInLhs' = fmap ellipsisPositionInLhs . (checkLeftEllipsis <=< checkOneSideIdent <=< checkDuplDim <=< checkRightEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

type RearrangeOutputCompositeAxesAPI = "/rearrange/output_composite_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] OutputCompositeAxesRet
type ReduceOutputCompositeAxesAPI = "/reduce/output_composite_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] OutputCompositeAxesRet
type RepeatOutputCompositeAxesAPI = "/repeat/output_composite_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] OutputCompositeAxesRet

rearrangeOutputCompositeAxesAPI :: Proxy RearrangeOutputCompositeAxesAPI
rearrangeOutputCompositeAxesAPI = Proxy

reduceOutputCompositeAxesAPI :: Proxy ReduceOutputCompositeAxesAPI
reduceOutputCompositeAxesAPI = Proxy

repeatOutputCompositeAxesAPI :: Proxy RepeatOutputCompositeAxesAPI
repeatOutputCompositeAxesAPI = Proxy

rearrangeOutputCompositeAxesRequest :: EquationStr Axis -> ClientM OutputCompositeAxesRet
rearrangeOutputCompositeAxesRequest = client rearrangeOutputCompositeAxesAPI

rearrangeOutputCompositeAxesPy :: Equation Axis -> Either BS.ByteString OutputCompositeAxesRet
rearrangeOutputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeOutputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

rearrangeOutputCompositeAxes' :: Equation Axis -> Either BS.ByteString OutputCompositeAxesRet
rearrangeOutputCompositeAxes' = fmap outputCompositeAxes . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

reduceOutputCompositeAxesRequest :: EquationStr Axis -> ClientM OutputCompositeAxesRet
reduceOutputCompositeAxesRequest = client reduceOutputCompositeAxesAPI

reduceOutputCompositeAxesPy :: Equation Axis -> Either BS.ByteString OutputCompositeAxesRet
reduceOutputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceOutputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

reduceOutputCompositeAxes' :: Equation Axis -> Either BS.ByteString OutputCompositeAxesRet
reduceOutputCompositeAxes' = fmap outputCompositeAxes . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

repeatOutputCompositeAxesRequest :: EquationStr Axis -> ClientM OutputCompositeAxesRet
repeatOutputCompositeAxesRequest = client repeatOutputCompositeAxesAPI

repeatOutputCompositeAxesPy :: Equation Axis -> Either BS.ByteString OutputCompositeAxesRet
repeatOutputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatOutputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

repeatOutputCompositeAxes' :: Equation Axis -> Either BS.ByteString OutputCompositeAxesRet
repeatOutputCompositeAxes' = fmap outputCompositeAxes . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

type RearrangeElementaryAxesLengthsAPI = "/rearrange/elementary_axes_lengths" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] ElementaryAxesLengthsRet
type ReduceElementaryAxesLengthsAPI = "/reduce/elementary_axes_lengths" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] ElementaryAxesLengthsRet
type RepeatElementaryAxesLengthsAPI = "/repeat/elementary_axes_lengths" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] ElementaryAxesLengthsRet

rearrangeElementaryAxesLengthsAPI :: Proxy RearrangeElementaryAxesLengthsAPI
rearrangeElementaryAxesLengthsAPI = Proxy

reduceElementaryAxesLengthsAPI :: Proxy ReduceElementaryAxesLengthsAPI
reduceElementaryAxesLengthsAPI = Proxy

repeatElementaryAxesLengthsAPI :: Proxy RepeatElementaryAxesLengthsAPI
repeatElementaryAxesLengthsAPI = Proxy

rearrangeElementaryAxesLengthsRequest :: EquationStr Axis -> ClientM ElementaryAxesLengthsRet
rearrangeElementaryAxesLengthsRequest = client rearrangeElementaryAxesLengthsAPI

rearrangeElementaryAxesLengthsPy :: Equation Axis -> Either BS.ByteString ElementaryAxesLengthsRet
rearrangeElementaryAxesLengthsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeElementaryAxesLengthsRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

rearrangeElementaryAxesLengths' :: Equation Axis -> Either BS.ByteString ElementaryAxesLengthsRet
rearrangeElementaryAxesLengths' = fmap elementaryAxesLengths . (checkDuplDim <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis <=< checkLeftAxisUnused <=< checkLeftEllipsis <=< checkAxisInvalidName <=< checkRightAxisUnused <=< checkOneSideIdent)

reduceElementaryAxesLengthsRequest :: EquationStr Axis -> ClientM ElementaryAxesLengthsRet
reduceElementaryAxesLengthsRequest = client reduceElementaryAxesLengthsAPI

reduceElementaryAxesLengthsPy :: Equation Axis -> Either BS.ByteString ElementaryAxesLengthsRet
reduceElementaryAxesLengthsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceElementaryAxesLengthsRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

reduceElementaryAxesLengths' :: Equation Axis -> Either BS.ByteString ElementaryAxesLengthsRet
reduceElementaryAxesLengths' = fmap elementaryAxesLengths . (checkDuplDim <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis <=< checkLeftAxisUnused <=< checkLeftEllipsis <=< checkAxisInvalidName <=< checkRightAxisUnused <=< checkOneSideIdent)

repeatElementaryAxesLengthsRequest :: EquationStr Axis -> ClientM ElementaryAxesLengthsRet
repeatElementaryAxesLengthsRequest = client repeatElementaryAxesLengthsAPI

repeatElementaryAxesLengthsPy :: Equation Axis -> Either BS.ByteString ElementaryAxesLengthsRet
repeatElementaryAxesLengthsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatElementaryAxesLengthsRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

repeatElementaryAxesLengths' :: Equation Axis -> Either BS.ByteString ElementaryAxesLengthsRet
repeatElementaryAxesLengths' = fmap elementaryAxesLengths . (checkDuplDim <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis <=< checkLeftAxisUnused <=< checkLeftEllipsis <=< checkAxisInvalidName <=< checkRightAxisUnused <=< checkOneSideIdent)

type RearrangeInputCompositeAxesAPI = "/rearrange/input_composite_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] InputCompositeAxesRet
type ReduceInputCompositeAxesAPI = "/reduce/input_composite_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] InputCompositeAxesRet
type RepeatInputCompositeAxesAPI = "/repeat/input_composite_axes" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] InputCompositeAxesRet

rearrangeInputCompositeAxesAPI :: Proxy RearrangeInputCompositeAxesAPI
rearrangeInputCompositeAxesAPI = Proxy

reduceInputCompositeAxesAPI :: Proxy ReduceInputCompositeAxesAPI
reduceInputCompositeAxesAPI = Proxy

repeatInputCompositeAxesAPI :: Proxy RepeatInputCompositeAxesAPI
repeatInputCompositeAxesAPI = Proxy

rearrangeInputCompositeAxesRequest :: EquationStr Axis -> ClientM InputCompositeAxesRet
rearrangeInputCompositeAxesRequest = client rearrangeInputCompositeAxesAPI

rearrangeInputCompositeAxesPy :: Equation Axis -> Either BS.ByteString InputCompositeAxesRet
rearrangeInputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeInputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

rearrangeInputCompositeAxes' :: Equation Axis -> Either BS.ByteString InputCompositeAxesRet
rearrangeInputCompositeAxes' = fmap inputCompositeAxes . (checkDuplDim <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis <=< checkLeftAxisUnused <=< checkLeftEllipsis <=< checkAxisInvalidName <=< checkRightAxisUnused <=< checkOneSideIdent)

reduceInputCompositeAxesRequest :: EquationStr Axis -> ClientM InputCompositeAxesRet
reduceInputCompositeAxesRequest = client reduceInputCompositeAxesAPI

reduceInputCompositeAxesPy :: Equation Axis -> Either BS.ByteString InputCompositeAxesRet
reduceInputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceInputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

reduceInputCompositeAxes' :: Equation Axis -> Either BS.ByteString InputCompositeAxesRet
reduceInputCompositeAxes' = fmap inputCompositeAxes . (checkDuplDim <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis <=< checkLeftAxisUnused <=< checkLeftEllipsis <=< checkAxisInvalidName <=< checkRightAxisUnused <=< checkOneSideIdent)

repeatInputCompositeAxesRequest :: EquationStr Axis -> ClientM InputCompositeAxesRet
repeatInputCompositeAxesRequest = client repeatInputCompositeAxesAPI

repeatInputCompositeAxesPy :: Equation Axis -> Either BS.ByteString InputCompositeAxesRet
repeatInputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatInputCompositeAxesRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

repeatInputCompositeAxes' :: Equation Axis -> Either BS.ByteString InputCompositeAxesRet
repeatInputCompositeAxes' = fmap inputCompositeAxes . (checkDuplDim <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis <=< checkLeftAxisUnused <=< checkLeftEllipsis <=< checkAxisInvalidName <=< checkRightAxisUnused <=< checkOneSideIdent)

-- axesPermutation gives the numbers of flatten output axes
axesPermutation :: (Show a,Ord a) => Equation a -> [Int]
axesPermutation (Equation{..}) = let
    axisNums = M.fromList $ (`zip` [0..]) $ flatten inp
    in
    map (axisNums M.!) $ flatten outp

-- addedAxes given equation returns a map from axes numbers to repeat counts
-- Example: addedAxes "h w -> h w 3" is {2: 3}
-- TODO: Add integer type to Axis datatype
addedAxes :: Equation Axis -> AddedAxesRet
addedAxes _ = []  -- TODO: implement

-- example: reducedElementaryAxes "h w -> h" "max" is [1]
reducedElementaryAxes :: (Show a,Ord a) => Equation a -> ReducedElementaryAxesRet
reducedElementaryAxes (Equation{..}) = let
    axisNums = M.fromList $ (`zip` [0..]) $ flatten inp
    in
    map (axisNums M.!) $ flatten outp \\ flatten inp

outputCompositeAxes :: Equation Axis -> OutputCompositeAxesRet
outputCompositeAxes eqn@(Equation{..}) = let
    axisNums = M.fromList $ (`zip` [0..]) $ flatten inp
    in
    map (F.toList . fmap (axisNums M.!)) outp

elementaryAxesLengths :: Equation Axis -> ElementaryAxesLengthsRet
elementaryAxesLengths eqn@Equation{..} = let m = M.fromList axesLengths in
    map (`M.lookup` m) (flatten inp)

rebaseNums :: [Int] -> [Int]
rebaseNums xs = let
    h = M.fromList $ zip (sort xs) [0..]
    in map (h M.!) xs

-- ellipsisPositionInLhs (for now) gives the ellipsis position in the flattened
-- input axes
-- TODO: Error handling for two ellipses or ellipsis within composite axis
ellipsisPositionInLhs :: Equation Axis -> Maybe Int
ellipsisPositionInLhs = fmap fst . find (\(_,a) -> a == Ellipsis) . zip [0..] . flatten . inp

-- inputCompositeAxes returns a list that for each composite axis returns its
-- tuple of known and unknown axis numbers
inputCompositeAxes :: Equation Axis -> [([Int],[Int])]
inputCompositeAxes eqn@Equation{..} =
    let
        known = S.fromList (fmap ((axisNums M.!) . fst) axesLengths)
        axisNums = M.fromList $ (`zip` [0..]) $ flatten inp
        in map (
            partition (`S.member` known) . map (axisNums M.!) . F.toList
            ) inp

toLists :: [Composite a] -> [[a]]
toLists = map F.toList

newtype CC a = CC (Compose [] Composite a) deriving (Functor, Foldable, Traversable)

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

emptyAxis :: [Composite Axis]
emptyAxis = []

iAxis :: [Composite Axis]
iAxis = [Single I]

ijeAxis = [Multiple [I,J], Single Ellipsis]

errAxis = [Multiple [J, Ellipsis], Single J]

fixup :: Equation Axis -> Equation Axis
fixup eqn@Equation{..} = let
    inp' = uncc . remDupl . cc . remEllFromMult $ inp
    outp' = uncc . remDupl . cc . remEllFromMult $ outp
    in eqn{inp = inp', outp = outp'}

remDupl' :: [Composite Axis] -> [Composite Axis]
remDupl' = uncc . remDupl . cc

-- remEllFromMult removes ellipses from Multiples
remEllFromMult :: [Composite Axis] -> [Composite Axis]
remEllFromMult = map (\case
    Single x -> Single x
    Multiple xs -> Multiple $ filter (/= Ellipsis) xs
    )

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

checkLeftEllipsis :: Equation Axis -> Either BS.ByteString (Equation Axis)
checkLeftEllipsis eqn@(Equation{..}) | Ellipsis `elem` flatten inp && Ellipsis `notElem` flatten outp = Left $ "Ellipsis found in left side, but not right side of a pattern " <> BS.pack (eqnToStr eqn)
checkLeftEllipsis eqn = Right eqn

checkRightEllipsis :: Equation Axis -> Either BS.ByteString (Equation Axis)
-- TODO: Check if latest einops still has this bug
checkRightEllipsis eqn@(Equation{..}) | Ellipsis `elem` flatten outp && Ellipsis `notElem` flatten inp = Left $ "Ellipsis found in left side, but not right side of a pattern " <> BS.pack (eqnToStr eqn)
checkRightEllipsis eqn = Right eqn

checkEllipsisIsParen :: Equation Axis -> Either BS.ByteString (Equation Axis)
checkEllipsisIsParen eqn@(Equation{..}) | Ellipsis `elem` flatten (filter (\case (Multiple _) -> True; _ -> False) inp) = Left $ "Ellipsis is parenthesis in the left side is not allowed:  " <> BS.pack (eqnToStr eqn)
checkEllipsisIsParen eqn = Right eqn

checkDuplicateEllipsis :: Equation Axis -> Either BS.ByteString (Equation Axis)
checkDuplicateEllipsis eqn@(Equation{..}) | length (filter (==Ellipsis) (flatten inp)) > 1 = Left "Expression may contain dots only inside ellipsis (...); only one ellipsis for tensor "
checkDuplicateEllipsis eqn = Right eqn

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

checkAxisInvalidName :: Equation Axis -> Either BS.ByteString (Equation Axis)
checkAxisInvalidName eqn@(Equation{..}) | Ellipsis `elem` map fst axesLengths = Left "('Invalid name for an axis', '...')"
checkAxisInvalidName eqn = Right eqn

remDupl :: (Show a,Ord a,Witherable t) => t a -> t a
remDupl = (`evalState` S.empty) . wither (\a -> state (\s -> (mfilter (not . (`S.member` s)) (Just a),S.insert a s)))

findError :: ClientError -> BS.ByteString -- TODO: Make Unicode ellipsis (U+2026 display correctly
findError (UnsupportedContentType req resp@Response{..}) = responseBody
findError x = error (show x)

-- Observed order:
-- OneSideIdent < LeftEllipsis in EllipsisPositionInLhs
-- RightEllipsis < OneSideIdent in EllipsisPositionInLhs
-- DuplDim (on right side) < OneSideIdent in EllipsisPositionInLhs
-- LeftEllipsis < OneSideIdent in OutputCompositeAxes

main :: IO ()
main = do
    hspec $ do
        it "gets axes permutations for valid equation" $
            rearrangeAxesPermutation' (Equation {
                inp = [Single I, Single J]
                , outp = [Single J, Single I]
                , axesLengths = []
                })
            `shouldBe`
            rearrangeAxesPermutationPy (Equation {
                inp = [Single I, Single J]
                , outp = [Single J, Single I]
                , axesLengths = []
                })
    hspec $ do
        it "returns error for duplicate dimension" $
            rearrangeAxesPermutation' (Equation {
                inp = [Multiple [I,I]]
                , outp = [Multiple [I,I]]
                , axesLengths = []
                })
            `shouldBe`
            rearrangeAxesPermutationPy (Equation {
                inp = [Multiple [I,I]]
                , outp = [Multiple [I,I]]
                , axesLengths = []
                })
        it "returns error for one side ident" $
            rearrangeAxesPermutation' (Equation {
                inp = [Single I]
                , outp = []
                , axesLengths = []
                })
            `shouldBe`
            rearrangeAxesPermutationPy (Equation {
                inp = [Single I]
                , outp = []
                , axesLengths = []
                })

    hspec $ do
        it "calculates elementary axes lengths" $
            rearrangeElementaryAxesLengths' (Equation {
                inp = [Single I, Single J]
                , outp = [Single J, Single I]
                , axesLengths = [(I,2)]
                })
            `shouldBe`
            rearrangeElementaryAxesLengthsPy (Equation {
                inp = [Single I, Single J]
                , outp = [Single J, Single I]
                , axesLengths = [(I,2)]
                })
        it "calculates elementary axes lengths for multiples" $
            rearrangeElementaryAxesLengths' (Equation {
                inp = [Multiple [I0,I1], Single J]
                , outp = [Single I0, Single I1, Single J]
                , axesLengths = [(I1, 2)]
                })
            `shouldBe`
            rearrangeElementaryAxesLengthsPy (Equation {
                inp = [Multiple [I0,I1], Single J]
                , outp = [Single I0, Single I1, Single J]
                , axesLengths = [(I1, 2)]
                })

    hspec $ do
        it "calculates input composite axes" $
            rearrangeInputCompositeAxes' (Equation {
                inp = [Multiple [I0,I1], Single J]
                , outp = [Single I0, Single I1, Single J]
                , axesLengths = [(I1, 2)]
                })
            `shouldBe`
            rearrangeInputCompositeAxesPy (Equation {
                inp = [Multiple [I0,I1], Single J]
                , outp = [Single I0, Single I1, Single J]
                , axesLengths = [(I1, 2)]
                })
        it "calculates more composite axes" $
            rearrangeInputCompositeAxes' (Equation {
                inp = [Single J, Multiple [I0]]
                , outp = [Single I0, Single J]
                , axesLengths = [(I0, 2)]
                })
            `shouldBe`
            rearrangeInputCompositeAxesPy (Equation {
                inp = [Single J, Multiple [I0]]
                , outp = [Single I0, Single J]
                , axesLengths = [(I0, 2)]
                })
    hspec $ do
        it "calculates reduced elementary axes" $
            reduceReducedElementaryAxes' (Equation {
                inp = [Single I, Single J]
                , outp = [Single I]
                , axesLengths = []
                })
            `shouldBe`
            reduceReducedElementaryAxesPy (Equation {
                inp = [Single I, Single J]
                , outp = [Single I]
                , axesLengths = []
                })

    -- TODO: Create endpoints for rearrange, reduce and repeat
    -- TODO: Support underscore axis

    -- PASS
    -- quickCheck $ \xs -> axesPermutationPy xs === axesPermutation' xs
    -- quickCheck $ \xs -> ellipsisPositionInLhsPy xs === ellipsisPositionInLhs' xs
    -- quickCheck $ \xs -> outputCompositeAxesPy xs === outputCompositeAxes' xs

    -- print . elementaryAxesLengths' $ (Equation [] [] [(Ellipsis,0)])
    -- quickCheck $ \xs -> collect (isRight (elementaryAxesLengths' xs)) $ eitherToMaybe (elementaryAxesLengthsPy xs) === eitherToMaybe (elementaryAxesLengths' xs)

    -- let xs = Equation [] [Multiple [Ellipsis]] in
    --     print $ ellipsisPositionInLhsPy xs



    -- print . axesPermutation' $ (Equation [] [Multiple []] :: Equation Axis)
    -- print $ ellipsisPositionInLhs [I, Ellipsis, J]
    -- print $ ellipsisPositionInLhs [I, J]
    -- print $ inputCompositeAxes S.empty $ fmap (fmap fromEnum) [Single I, Single J]

    pure ()
