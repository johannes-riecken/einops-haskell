{-# LANGUAGE LambdaCase #-}
import Data.Maybe
import qualified Data.Set as S
import Data.Set (Set(..))
import Data.List
import Control.Arrow

data Axis = I | J | Ellipsis deriving (Eq, Ord, Show, Bounded, Enum)

data Composite a = Single a | Multiple [a] deriving (Show)

flatten :: [Composite a] -> [a]
flatten (Single x : xs) = x : flatten xs
flatten (Multiple xs : xs') = xs ++ flatten xs'

data Equation a = Equation {
    input :: [Composite a],
    output :: [Composite a]
    } deriving (Show)

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
inputCompositeAxes :: Set Axis -> [Composite Axis] -> [([Int],[Int])]
inputCompositeAxes known xs = map (map fromEnum *** map fromEnum) xs' where
        xs' = map (\case
            Single x -> case x `S.member` known of
                True -> ([x], [])
                False -> ([], [x])
            Multiple xs -> partition (`S.member` known) xs
            ) xs

main :: IO ()
main = do
    print $ axesPermutation [J, I]
    print $ ellipsisPositionInLhs [I, Ellipsis, J]
    print $ ellipsisPositionInLhs [I, J]
    print $ inputCompositeAxes S.empty [Single I, Single J]
    pure ()
