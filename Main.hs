{-# LANGUAGE DeriveTraversable, LambdaCase #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DataKinds, TypeOperators #-}
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set(..))
import Data.List
import Control.Arrow
import qualified Data.Foldable as F

import Data.Either (fromRight)
import Servant.API
import Servant.Client
import Servant.Client.Core.Request hiding (RequestBodyLBS, requestHeaders, requestBody)
import GHC.Generics
import Data.Proxy
import Data.Aeson hiding ((.:))
import Network.HTTP.Client hiding (Proxy(..), cookieJar)
import Network.HTTP.Simple hiding (Proxy(..))
import qualified Data.Sequence as S
import Data.Function.Pointless
import Data.Either.Extra (eitherToMaybe)
import Test.QuickCheck
import System.IO.Unsafe
import Safe
import qualified Data.ByteString.Lazy.Char8 as BS

data Axis = I | J | Ellipsis deriving (Eq, Ord, Bounded, Enum, Generic)

instance Show Axis where
    show I = "i"
    show J = "j"
    show Ellipsis = "..."

instance Arbitrary Axis where
    arbitrary = elements [I, J, Ellipsis]

instance ToJSON Axis

data Composite a = Single a | Multiple [a] deriving (Functor, Foldable, Traversable, Show, Generic)

instance Arbitrary a => Arbitrary (Composite a) where
    arbitrary = oneof [Single <$> arbitrary, Multiple <$> arbitrary]

instance ToJSON a => ToJSON (Composite a)

-- type EinOpsAPI = "/" :> ReqBody '[JSON] (Equation Axis) :> Post '[JSON] [Int]
type EinOpsAPI = "/" :> ReqBody '[JSON] EquationStr :> Post '[JSON] [Int]

newtype EquationStr = EquationStr { eqn :: String } deriving (Generic, Show)

instance ToJSON EquationStr

einOpsAPI :: Proxy EinOpsAPI
einOpsAPI = Proxy

einOpsRequest :: EquationStr -> ClientM [Int]
einOpsRequest = client einOpsAPI

einOps :: Equation Axis -> [Int]
-- einOps :: Equation Axis -> Either ClientError [Int]
einOps xs = (fromRight [777]) . unsafePerformIO $ do
-- einOps xs = unsafePerformIO $ do
    mngr <- newManager defaultManagerSettings
    res <- runClientM (einOpsRequest . EquationStr . eqnToStr $ xs) (
        mkClientEnv mngr (BaseUrl Http "127.0.0.1" 5000 ""))
    return res

flatten :: [Composite a] -> [a]
flatten = (=<<) F.toList

data Equation a = Equation {
    input :: [Composite a],
    output :: [Composite a]
    } deriving (Show, Generic)

instance Arbitrary a => Arbitrary (Equation a) where
    arbitrary = Equation <$> arbitrary <*> arbitrary

instance ToJSON a => ToJSON (Equation a)

eqnToStr :: Show a => Equation a -> String
eqnToStr (Equation i o) = compsToStr i <> " -> " <> compsToStr o

compsToStr :: Show a => [Composite a] -> String
compsToStr = intercalate " " . fmap compToStr

compToStr :: Show a => Composite a -> String
compToStr (Single x) = show x
compToStr (Multiple xs) = "(" <> intercalate " " (fmap show xs) <> ")"

-- axesPermutation gives the numbers of flatten output axes
axesPermutation :: [Axis] -> [Int]
axesPermutation = map fromEnum

-- ellipsisPositionInLhs (for now) gives the ellipsis position in the flattened
-- input axes
-- TODO: Error handling for two ellipses or ellipsis within composite axis
ellipsisPositionInLhs :: [Axis] -> Maybe Int
ellipsisPositionInLhs = fmap fst . listToMaybe . filter (\(_,a) -> a == Ellipsis) . zip [0..]

-- inputCompositeAxes returns a list that for each composite axis returns its
-- tuple of known and unknown axis numbers
inputCompositeAxes :: Set Int -> [Composite Int] -> [([Int],[Int])]
inputCompositeAxes known xs = map (
            partition ((`S.member` known)) . F.toList
            ) xs


toLists :: [Composite a] -> [[a]]
toLists = map F.toList

emptyAxis :: [Composite Axis]
emptyAxis = []

iAxis :: [Composite Axis]
iAxis = [Single I]

ijeAxis = [Multiple [I,J], Single Ellipsis]

main :: IO ()
main = do
    -- BS.putStrLn $ encode (EquationStr " -> ")
    -- putStrLn . eqnToStr $ Equation iAxis iAxis
    -- putStrLn . eqnToStr $ Equation iAxis ijeAxis
    -- print $ einOps (Equation iAxis iAxis)
    quickCheck $ \xs -> einOps xs === axesPermutation (flatten . input $ xs)
    -- print $ axesPermutation [J, I]
    -- print $ ellipsisPositionInLhs [I, Ellipsis, J]
    -- print $ ellipsisPositionInLhs [I, J]
    -- print $ inputCompositeAxes S.empty $ fmap (fmap fromEnum) [Single I, Single J]

    pure ()
