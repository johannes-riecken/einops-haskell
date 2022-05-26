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
    show Ellipsis = "..."
    -- show (Anon x) = show x

instance Arbitrary Axis where
    arbitrary = elements [I, J, Ellipsis]

instance ToJSON Axis

data Composite a = Single a | Multiple [a] deriving (Functor, Foldable, Traversable, Show, Generic, Eq, Ord)

instance Arbitrary a => Arbitrary (Composite a) where
    arbitrary = oneof [Single <$> arbitrary, Multiple <$> arbitrary]

instance ToJSON a => ToJSON (Composite a)

type EinOpsAPI = "/" :> ReqBody '[JSON] EquationStr :> Post '[JSON] [Int]

newtype EquationStr = EquationStr { eqn :: String } deriving (Generic, Show)

instance ToJSON EquationStr

einOpsAPI :: Proxy EinOpsAPI
einOpsAPI = Proxy

einOpsRequest :: EquationStr -> ClientM [Int]
einOpsRequest = client einOpsAPI

-- einOps :: Equation Axis -> [Int]
einOps :: Equation Axis -> Either BS.ByteString [Int]
-- einOps :: Equation Axis -> Either ClientError [Int]
-- einOps xs = (fromRight [777]) . unsafePerformIO $ do
einOps xs = either (Left . findError) Right . unsafePerformIO $ do
-- einOps xs = unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    runClientM (einOpsRequest . EquationStr . eqnToStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))

flatten :: [Composite a] -> [a]
flatten = (=<<) F.toList

data Equation a = Equation {
    input :: [Composite a],
    output :: [Composite a]
    } deriving (Show, Generic)

instance Arbitrary a => Arbitrary (Equation a) where
    arbitrary = do
        inp <- arbitrary
        n <- choose (0,10)
        outp <- take n <$> shuffle inp
        pure $ Equation inp outp

instance ToJSON a => ToJSON (Equation a)

eqnToStr :: Show a => Equation a -> String
eqnToStr (Equation i o) = compsToStr i <> " -> " <> compsToStr o

compsToStr :: Show a => [Composite a] -> String
compsToStr = unwords . fmap compToStr

compToStr :: Show a => Composite a -> String
compToStr (Single x) = show x
compToStr (Multiple xs) = "(" <> unwords (fmap show xs) <> ")"

-- axesPermutation gives the numbers of flatten output axes
axesPermutation :: (Show a,Ord a) => Equation a -> [Int]
axesPermutation (Equation inp outp) = let
    axisNums = M.fromList $ (`zip` [0..]) $ flatten inp
    in
    trace (show axisNums) $ map (axisNums M.!) $ flatten outp

rebaseNums :: [Int] -> [Int]
rebaseNums xs = let
    h = M.fromList $ zip (sort xs) [0..]
    in map (h M.!) xs

-- ellipsisPositionInLhs (for now) gives the ellipsis position in the flattened
-- input axes
-- TODO: Error handling for two ellipses or ellipsis within composite axis
ellipsisPositionInLhs :: [Axis] -> Maybe Int
ellipsisPositionInLhs = fmap fst . find (\(_,a) -> a == Ellipsis) . zip [0..]

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
fixup (Equation inp outp) = let
    inp' = uncc . remDupl . cc . remEllFromMult $ inp
    outp' = remEllFromMult outp
    in Equation inp' outp'

-- remEllFromMult removes ellipses from Multiples
remEllFromMult :: [Composite Axis] -> [Composite Axis]
remEllFromMult = map (\case
    Single x -> Single x
    Multiple xs -> Multiple $ filter (/= Ellipsis) xs
    )


remDupl :: (Show a,Ord a,Witherable t) => t a -> t a
remDupl = (`evalState` S.empty) . wither (\a -> state (\s -> (mfilter (not . (`S.member` s)) (Just a),S.insert a s)))

findError :: ClientError -> BS.ByteString
findError (UnsupportedContentType req resp@Response{..}) = responseBody

main :: IO ()
main = do
    print $ einOps (Equation [Multiple [I,I]] [Multiple [I,I]])
    -- print $ einOps $ fixup (Equation [Multiple [I]] [Multiple [I]])
    -- TODO: Create endpoints for all the recipe fields and create unit tests
    -- for them
    -- TODO: Create endpoints for rearrange, reduce and repeat
    -- BS.putStrLn $ encode (EquationStr " -> ")
    -- putStrLn . eqnToStr $ Equation iAxis iAxis
    -- putStrLn . eqnToStr $ Equation iAxis ijeAxis
    -- print $ einOps (Equation iAxis iAxis)

--     quickCheck $ \xs -> let xs' = fixup xs in einOps xs' === Right (axesPermutation xs')

    -- print $ axesPermutation (fixup $ Equation errAxis errAxis)
    -- print $ (fixup $ Equation errAxis errAxis)
    -- quickCheck $ \xs -> let xs' = fixup xs in einOps xs' === Right (inputCompositeAxes S.empty (input $ xs'))
    -- print $ axesPermutation [J, I]
    -- print $ ellipsisPositionInLhs [I, Ellipsis, J]
    -- print $ ellipsisPositionInLhs [I, J]
    -- print $ inputCompositeAxes S.empty $ fmap (fmap fromEnum) [Single I, Single J]

    pure ()
