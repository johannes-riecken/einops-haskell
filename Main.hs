import Data.Maybe

data Axis = I | J | Ellipsis deriving (Eq, Ord, Show, Bounded, Enum)

data Composite = Single Axis | Multiple [Axis] deriving (Show)

flatten :: [Composite] -> [Axis]
flatten (Single x : xs) = x : flatten xs
flatten (Multiple xs : xs') = xs ++ flatten xs'

data Equation = Equation {
    input :: [Composite],
    output :: [Composite]
    } deriving (Show)

-- axesPermutation gives the numbers of flatten output axes
axesPermutation :: [Axis] -> [Int]
axesPermutation = map fromEnum

-- ellipsisPositionInLhs (for now) gives the ellipsis position in the flattened
-- input axes
-- TODO: Error handling for two ellipses or ellipsis within composite axis
ellipsisPositionInLhs :: [Axis] -> Maybe Int
ellipsisPositionInLhs = fmap fst . listToMaybe . filter (\(_,a) -> a == Ellipsis) . zip [0..]

-- inputCompositeAxes known xs = 

main :: IO ()
main = do
    print $ axesPermutation [J, I]
    print $ ellipsisPositionInLhs [I, Ellipsis, J]
    print $ ellipsisPositionInLhs [I, J]
    pure ()
