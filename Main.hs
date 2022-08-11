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

-- AUTOGEN BEGIN
addedAxes' :: Equation Axis -> Either BS.ByteString AddedAxesRet
addedAxes' = fmap addedAxes . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

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
axesPermutation' = fmap axesPermutation . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

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
elementaryAxesLengths' = fmap elementaryAxesLengths . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

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

ellipsisPositionInLhs' :: Equation Axis -> Either BS.ByteString EllipsisPositionInLhsRet
ellipsisPositionInLhs' = fmap ellipsisPositionInLhs . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

type RearrangeEllipsisPositionInLhsAPI = "/rearrange/ellipsis_position_in_lhs" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] EllipsisPositionInLhsRet

rearrangeEllipsisPositionInLhsAPI :: Proxy RearrangeEllipsisPositionInLhsAPI
rearrangeEllipsisPositionInLhsAPI = Proxy

rearrangeEllipsisPositionInLhsRequest :: EquationStr Axis -> ClientM EllipsisPositionInLhsRet
rearrangeEllipsisPositionInLhsRequest = client rearrangeEllipsisPositionInLhsAPI

rearrangeEllipsisPositionInLhsPy :: Equation Axis -> Either BS.ByteString EllipsisPositionInLhsRet
rearrangeEllipsisPositionInLhsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (rearrangeEllipsisPositionInLhsRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type ReduceEllipsisPositionInLhsAPI = "/reduce/ellipsis_position_in_lhs" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] EllipsisPositionInLhsRet

reduceEllipsisPositionInLhsAPI :: Proxy ReduceEllipsisPositionInLhsAPI
reduceEllipsisPositionInLhsAPI = Proxy

reduceEllipsisPositionInLhsRequest :: EquationStr Axis -> ClientM EllipsisPositionInLhsRet
reduceEllipsisPositionInLhsRequest = client reduceEllipsisPositionInLhsAPI

reduceEllipsisPositionInLhsPy :: Equation Axis -> Either BS.ByteString EllipsisPositionInLhsRet
reduceEllipsisPositionInLhsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (reduceEllipsisPositionInLhsRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

type RepeatEllipsisPositionInLhsAPI = "/repeat/ellipsis_position_in_lhs" :> ReqBody '[JSON] (EquationStr Axis) :> Post '[JSON] EllipsisPositionInLhsRet

repeatEllipsisPositionInLhsAPI :: Proxy RepeatEllipsisPositionInLhsAPI
repeatEllipsisPositionInLhsAPI = Proxy

repeatEllipsisPositionInLhsRequest :: EquationStr Axis -> ClientM EllipsisPositionInLhsRet
repeatEllipsisPositionInLhsRequest = client repeatEllipsisPositionInLhsAPI

repeatEllipsisPositionInLhsPy :: Equation Axis -> Either BS.ByteString EllipsisPositionInLhsRet
repeatEllipsisPositionInLhsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (repeatEllipsisPositionInLhsRequest . eqnToEqnStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

inputCompositeAxes' :: Equation Axis -> Either BS.ByteString InputCompositeAxesRet
inputCompositeAxes' = fmap inputCompositeAxes . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

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
outputCompositeAxes' = fmap outputCompositeAxes . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

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
reducedElementaryAxes' = fmap reducedElementaryAxes . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

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


axisNumsFromCompList :: Ord a => [Composite a] -> M.Map a Int
axisNumsFromCompList = snd . foldl' (\(i,acc) x ->
        if M.member x acc then (i,acc) else
        (i+1,M.insertWith (\a b -> error "no duplicates allowed") x i acc))
        (0,M.empty) . cc

-- axesPermutation gives the numbers of flatten output axes
axesPermutation :: (Show a,Ord a) => Equation a -> [Int]
axesPermutation (Equation{..}) = let
    axisNums = axisNumsFromCompList inp
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
    map (axisNums M.!) $ flatten outp \\ flatten inp

outputCompositeAxes :: Equation Axis -> OutputCompositeAxesRet
outputCompositeAxes eqn@(Equation{..}) = let
    axisNums = axisNumsFromCompList inp
    in
    map (F.toList . fmap (axisNums M.!)) outp

elementaryAxesLengths :: Equation Axis -> ElementaryAxesLengthsRet
elementaryAxesLengths eqn@Equation{..} = let m = M.fromList axesLengths in
    foldr ((:) . (`M.lookup` m)) [] . cc $ inp

-- ellipsisPositionInLhs (for now) gives the ellipsis position in the flattened
-- input axes
-- TODO: Error handling for two ellipses or ellipsis within composite axis
ellipsisPositionInLhs :: Equation Axis -> Maybe Int
ellipsisPositionInLhs xs = let inp' = inp xs in
    fmap fst . listToMaybe . snd .
    foldr (\x (i,acc) -> if x == Single Ellipsis then (i+1,(i,x):acc) else (i+1,acc))
        (length inp' - 1, []) $ inp'

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

remDupl :: (Show a,Ord a,Witherable t) => t a -> t a
remDupl = (`evalState` S.empty) . wither (\a -> state (\s -> (mfilter (not . (`S.member` s)) (Just a),S.insert a s)))

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
            axesPermutation' (Equation {
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
            axesPermutation' (Equation {
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
            axesPermutation' (Equation {
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
            elementaryAxesLengths' (Equation {
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
            elementaryAxesLengths' (Equation {
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
            inputCompositeAxes' (Equation {
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
            inputCompositeAxes' (Equation {
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
    -- hspec $ do
    --     it "calculates reduced elementary axes" $
    --         reduceReducedElementaryAxes' (Equation {
    --             inp = [Single I, Single J]
    --             , outp = [Single I]
    --             , axesLengths = []
    --             })
    --         `shouldBe`
    --         reduceReducedElementaryAxesPy (Equation {
    --             inp = [Single I, Single J]
    --             , outp = [Single I]
    --             , axesLengths = []
    --             })

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
