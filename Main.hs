{-# LANGUAGE RecordWildCards, DeriveTraversable, LambdaCase #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DataKinds, TypeOperators #-}
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.State
import Data.Aeson hiding ((.:))
import qualified Data.ByteString.Char8 as BS (isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Either (fromRight)
import Data.Either.Extra (eitherToMaybe)
import qualified Data.Foldable as F
import Data.Function.Pointless
import Data.Functor.Classes
import Data.Functor.Compose
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
    | Ellipsis
    -- | Anon Int -- TODO: Add to Arbitrary instance and deal with Bounded
    deriving (Eq, Ord, Bounded, Enum, Generic)

instance Show Axis where
    show I = "i"
    show J = "j"
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

newtype EquationStr = EquationStr { eqn :: String } deriving (Generic, Show)

instance ToJSON EquationStr

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
        pure $ Equation{..}
    shrink = genericShrink

instance ToJSON a => ToJSON (Equation a)

eqnToStr :: Show a => Equation a -> String
eqnToStr (Equation{..}) = compsToStr inp <> " -> " <> compsToStr outp

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

type AxesPermutationAPI = "/axes_permutation" :> ReqBody '[JSON] EquationStr :> Post '[JSON] AxesPermutationRet

axesPermutationAPI :: Proxy AxesPermutationAPI
axesPermutationAPI = Proxy

axesPermutationRequest :: EquationStr -> ClientM AxesPermutationRet
axesPermutationRequest = client axesPermutationAPI

axesPermutationPy :: Equation Axis -> Either BS.ByteString AxesPermutationRet
axesPermutationPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (axesPermutationRequest . EquationStr . eqnToStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

axesPermutation' :: Equation Axis -> Either BS.ByteString AxesPermutationRet
axesPermutation' = fmap axesPermutation . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

type AddedAxesAPI = "/added_axes" :> ReqBody '[JSON] EquationStr :> Post '[JSON] AddedAxesRet

addedAxesAPI :: Proxy AddedAxesAPI
addedAxesAPI = Proxy

addedAxesRequest :: EquationStr -> ClientM AddedAxesRet
addedAxesRequest = client addedAxesAPI

addedAxesPy :: Equation Axis -> Either BS.ByteString AddedAxesRet
addedAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (addedAxesRequest . EquationStr . eqnToStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

addedAxes' :: Equation Axis -> Either BS.ByteString AddedAxesRet
addedAxes' = fmap addedAxes . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

type EllipsisPositionInLhsAPI = "/ellipsis_position_in_lhs" :> ReqBody '[JSON] EquationStr :> Post '[JSON] EllipsisPositionInLhsRet

ellipsisPositionInLhsAPI :: Proxy EllipsisPositionInLhsAPI
ellipsisPositionInLhsAPI = Proxy

ellipsisPositionInLhsRequest :: EquationStr -> ClientM EllipsisPositionInLhsRet
ellipsisPositionInLhsRequest = client ellipsisPositionInLhsAPI

ellipsisPositionInLhsPy :: Equation Axis -> Either BS.ByteString EllipsisPositionInLhsRet
ellipsisPositionInLhsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (ellipsisPositionInLhsRequest . EquationStr . eqnToStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

ellipsisPositionInLhs' :: Equation Axis -> Either BS.ByteString EllipsisPositionInLhsRet
ellipsisPositionInLhs' = fmap ellipsisPositionInLhs . (checkLeftEllipsis <=< checkOneSideIdent <=< checkDuplDim <=< checkRightEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

type OutputCompositeAxesAPI = "/output_composite_axes" :> ReqBody '[JSON] EquationStr :> Post '[JSON] OutputCompositeAxesRet

outputCompositeAxesAPI :: Proxy OutputCompositeAxesAPI
outputCompositeAxesAPI = Proxy

outputCompositeAxesRequest :: EquationStr -> ClientM OutputCompositeAxesRet
outputCompositeAxesRequest = client outputCompositeAxesAPI

outputCompositeAxesPy :: Equation Axis -> Either BS.ByteString OutputCompositeAxesRet
outputCompositeAxesPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (outputCompositeAxesRequest . EquationStr . eqnToStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

outputCompositeAxes' :: Equation Axis -> Either BS.ByteString OutputCompositeAxesRet
outputCompositeAxes' = fmap outputCompositeAxes . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

type ElementaryAxesLengthsAPI = "/elementary_axes_lengths" :> ReqBody '[JSON] EquationStr :> Post '[JSON] ElementaryAxesLengthsRet

elementaryAxesLengthsAPI :: Proxy ElementaryAxesLengthsAPI
elementaryAxesLengthsAPI = Proxy

elementaryAxesLengthsRequest :: EquationStr -> ClientM ElementaryAxesLengthsRet
elementaryAxesLengthsRequest = client elementaryAxesLengthsAPI

elementaryAxesLengthsPy :: Equation Axis -> Either BS.ByteString ElementaryAxesLengthsRet
elementaryAxesLengthsPy xs = either (Left . findError) Right . unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (elementaryAxesLengthsRequest . EquationStr . eqnToStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

elementaryAxesLengths' :: Equation Axis -> Either BS.ByteString ElementaryAxesLengthsRet
elementaryAxesLengths' = fmap elementaryAxesLengths . (checkOneSideIdent <=< checkDuplDim <=< checkLeftEllipsis <=< checkEllipsisIsParen <=< checkRightDuplDim <=< checkDuplicateEllipsis)

-- axesPermutation gives the numbers of flatten output axes
axesPermutation :: (Show a,Ord a) => Equation a -> [Int]
axesPermutation (Equation{..}) = let
    axisNums = M.fromList $ (`zip` [0..]) $ flatten inp
    in
    map (axisNums M.!) $ flatten outp

-- added axes is apparently not relevant for "rearrange"
addedAxes :: Equation Axis -> AddedAxesRet
addedAxes _ = []  -- TODO: implement

outputCompositeAxes :: Equation Axis -> OutputCompositeAxesRet
outputCompositeAxes eqn@(Equation{..}) = let
    axisNums = M.fromList $ (`zip` [0..]) $ flatten inp
    in
    map (F.toList . fmap (axisNums M.!)) outp

elementaryAxesLengths :: Equation Axis -> ElementaryAxesLengthsRet
elementaryAxesLengths eqn = []

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
inputCompositeAxes :: Set Int -> [Composite Int] -> [([Int],[Int])]
inputCompositeAxes known = map (
            partition (`S.member` known) . F.toList
            )


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
fixup (Equation{..}) = let
    inp' = uncc . remDupl . cc . remEllFromMult $ inp
    outp' = uncc . remDupl . cc . remEllFromMult $ outp
    in Equation{inp = inp', outp = outp'}

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

remDupl :: (Show a,Ord a,Witherable t) => t a -> t a
remDupl = (`evalState` S.empty) . wither (\a -> state (\s -> (mfilter (not . (`S.member` s)) (Just a),S.insert a s)))

findError :: ClientError -> BS.ByteString -- TODO: Make Unicode ellipsis (U+2026 display correctly
findError (UnsupportedContentType req resp@Response{..}) = responseBody

-- Observed order:
-- OneSideIdent < LeftEllipsis in EllipsisPositionInLhs
-- RightEllipsis < OneSideIdent in EllipsisPositionInLhs
-- DuplDim (on right side) < OneSideIdent in EllipsisPositionInLhs
-- LeftEllipsis < OneSideIdent in OutputCompositeAxes

main :: IO ()
main = do
    hspec $ do
        it "gets axes permutations for valid equation" $
            axesPermutation' (Equation {inp = [Single I, Single J], outp = [Single J, Single I]})
            `shouldBe`
            axesPermutationPy (Equation {inp = [Single I, Single J], outp = [Single J, Single I]})
    hspec $ do
        it "returns error for duplicate dimension" $
            axesPermutation' (Equation {inp = [Multiple [I,I]], outp = [Multiple [I,I]]})
            `shouldBe`
            axesPermutationPy (Equation {inp = [Multiple [I,I]], outp = [Multiple [I,I]]})
        it "returns error for one side ident" $
            axesPermutation' (Equation {inp = [Single I], outp = []})
            `shouldBe`
            axesPermutationPy (Equation {inp = [Single I], outp = []})
    -- TODO: Create endpoints for rearrange, reduce and repeat
    -- TODO: Support underscore axis

    -- PASS
    -- quickCheck $ \xs -> axesPermutationPy xs === axesPermutation' xs
    -- quickCheck $ \xs -> ellipsisPositionInLhsPy xs === ellipsisPositionInLhs' xs
    -- quickCheck $ \xs -> outputCompositeAxesPy xs === outputCompositeAxes' xs

    quickCheck $ \xs -> elementaryAxesLengthsPy xs === elementaryAxesLengths' xs

    -- let xs = Equation [] [Multiple [Ellipsis]] in
    --     print $ ellipsisPositionInLhsPy xs



    -- print . axesPermutation' $ (Equation [] [Multiple []] :: Equation Axis)
    -- print $ ellipsisPositionInLhs [I, Ellipsis, J]
    -- print $ ellipsisPositionInLhs [I, J]
    -- print $ inputCompositeAxes S.empty $ fmap (fmap fromEnum) [Single I, Single J]

    pure ()
