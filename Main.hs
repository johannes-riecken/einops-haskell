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
import Control.Monad
import Control.Monad.Trans.State
import Data.Aeson hiding ((.:))
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Foldable as F
-- import Data.Tensor hiding (select)
import Data.Functor.Compose
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe hiding (mapMaybe, catMaybes)
import qualified Data.Set as S
import Data.Tuple (swap)
import GHC.Generics
import Iso.Deriving
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

newtype Axis a = Axis { getAxis :: Maybe a } deriving (Arbitrary,Functor,Applicative,Monad,Foldable,Traversable,Filterable)

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

intersectCompLists :: Ord a => [Composite a] -> [Composite a] -> [Composite a]
intersectCompLists xs ys = uncc $ mapMaybe (\x -> if x `elem` cc ys then Just x else Nothing) (cc xs)

axisNumsFromCompList :: Ord a => [Composite (Axis a)] -> M.Map a Int
axisNumsFromCompList = snd . foldl' (\(i,acc) x ->
        if M.member x acc then (i,acc) else
        (i+1,M.insertWith (\a b -> error "no duplicates allowed") x i acc))
        (0,M.empty) . ccc

-- axesPermutation gives the numbers of flatten output axes
axesPermutation :: Ord a => Equation (Axis a) -> [Int]
axesPermutation (Equation{..}) = let
    axisNums = axisNumsFromCompList (intersectCompLists inp outp)
    in
    foldr ((:) . (axisNums M.!)) [] . ccc $ outp

-- addedAxes given equation returns a map from axes numbers to repeat counts
-- Example: addedAxes "h w -> h w 3" is {2: 3}
-- TODO: Add integer type to Axis datatype
addedAxes :: Equation (Axis a) -> AddedAxesRet
addedAxes _ = []  -- TODO: implement

catMaybes' :: [Axis a] -> [a]
catMaybes' = mapMaybe getAxis

-- example: reducedElementaryAxes "h w -> h" "max" is [1]
-- TODO: fuse
reducedElementaryAxes :: Ord a => Equation (Axis a) -> ReducedElementaryAxesRet
reducedElementaryAxes (Equation{..}) = let
    axisNums = axisNumsFromCompList inp
    catMaybes' :: [Axis a] -> [a]
    catMaybes' = mapMaybe getAxis
    flatten' = catMaybes' . flatten
    in
    map (axisNums M.!) $ flatten' inp \\ flatten' outp

-- TODO: fuse
-- TODO: allow deeper nesting of Composite
initMap :: Ord a => Equation (Axis a) -> [Int] -> Map a Int
initMap eqn@Equation{..} shape =
    let
    sizes = M.fromList . snd $ foldr (\x' (i,acc) -> case x' of
            Single (Axis (Just x)) -> (i-1,(x, shape !! i):acc)
            Single (Axis Nothing) -> (i-1,acc)
            Multiple xs -> (i-1,handleMultiple i (catMaybes' xs) ++ acc)
            ) (length inp - 1,[]) inp
    handleMultiple i xs = map (\x ->
        (x,fromJust $ (axesLengthsMap M.!? x) <|> Just (shape !! i `div` prod xs))
        ) xs
        where
            prod = foldr ((*) . (\k -> M.findWithDefault 1 k axesLengthsMap)) 1
    axesLengthsMap = M.fromList . map swap . mapMaybe (sequenceA . swap . first getAxis) $ axesLengths
    in sizes

-- TODO: fuse
initShapesWithShape :: Ord a => [Int] -> Equation (Axis a) -> InitShapesRet
initShapesWithShape shape eqn@Equation{..} = foldr ((:) . (initMap eqn shape M.!)) [] . ccc $ inp

-- TODO: remove
reducedAxes :: Ord a => Equation (Axis a) -> ReducedAxesRet
reducedAxes = reducedElementaryAxes

-- TODO: remove
axesReordering :: Ord a => Equation (Axis a) -> AxesReorderingRet
axesReordering = axesPermutation

-- TODO: implement
addedAxesReconstruct :: Equation (Axis a) -> AddedAxesReconstructRet
addedAxesReconstruct _ = M.empty

-- TODO: allow deeper nesting
-- TODO: fuse
finalShapesWithShape :: Ord a => [Int] -> Equation (Axis a) -> FinalShapesRet
finalShapesWithShape shape eqn@Equation{..} = foldr ((:) . foldr ((*) . (initMap eqn shape M.!)) 1 . cci) [] outp
-- end of reconstruct

-- TODO: Generalize reduction type
applyRecipe :: Ord a => [Int] -> Equation (Axis a) -> Maybe String -> [TfCommand]
applyRecipe shape eqn@Equation{..} reductionName = let
    x = initShapesWithShape shape eqn
    y = reducedAxes eqn
    z = axesReordering eqn
    w = addedAxesReconstruct eqn
    v = finalShapesWithShape shape eqn
    reshapeCmd = Reshape x
    reduceCmds = if null y then [] else getReduceCmds (fromJust reductionName) y
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

-- inner composition
newtype CCI a = CCI (Compose Composite Axis a) deriving (Functor, Foldable, Traversable, Applicative)
cci :: Composite (Axis a) -> CCI a
cci = CCI . Compose

-- list of axes
newtype CLA a = CLA (Compose [] Axis a) deriving (Functor, Foldable, Traversable, Applicative)

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

remDupl :: (Show a,Ord a,Witherable t) => t a -> t a
remDupl = (`evalState` S.empty) . wither (\a -> state (\s -> (mfilter (not . (`S.member` s)) (Just a),S.insert a s)))

remDupl' :: (Show a,Ord a) => [Composite (Axis a)] -> [Composite (Axis a)]
remDupl' = uncc . remDupl . cc

-- TODO: check this if operation is rearrange
-- TODO: cover
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

-- TODO: cover
checkDuplDim :: (Eq a,Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString (Equation (Axis a))
checkDuplDim eqn@(Equation{..}) = if flatten inp == flatten (remDupl' inp) && flatten outp == flatten (remDupl' outp)
    then
    Right $ Equation{..}
    else
    let
        ys = head ((flatten inp \\ flatten (remDupl' inp)) ++ (flatten outp \\ flatten (remDupl' outp)))
        dup = BS.pack . show $ ys in Left $ "Indexing expression contains duplicate dimension \"" <> dup <> "\""

-- TODO: cover
checkRightDuplDim :: (Eq a,Ord a,Show a) => Equation (Axis a) -> Either BS.ByteString (Equation (Axis a))
checkRightDuplDim eqn@(Equation{..}) = if flatten outp == flatten (remDupl' outp)
    then
    Right eqn
    else
    let
        ys = head ((flatten inp \\ flatten (remDupl' inp)) ++ (flatten outp \\ flatten (remDupl' outp)))
        dup = BS.pack . show $ ys in Left $ "Indexing expression contains duplicate dimension \"" <> dup <> "\""

main :: IO ()
main = do
    hspec $ do
        it "generates tf commands for rearrange" $
            applyRecipe sampleShape (Equation {
                inp = [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "H"), Multiple [axis "B", axis "W"], Single (axis "C")]
                , axesLengths = []
                }) Nothing
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
                }) (Just "max")
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
                }) Nothing
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
                }) Nothing
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
                }) Nothing
            `shouldBe`
            [
            Reshape [4, 4, 3]
            , Transpose [0, 1, 2]
            , Reshape [16, 3]
            ]
        it "generates tf commands for anonymous axis" $
            applyRecipe (1 : tail sampleShape) (Equation {
                inp = [Single anon, Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = [Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , axesLengths = []
                }) Nothing
            `shouldBe`
            [
            Reshape [4, 4, 3]
            , Transpose [0, 1, 2]
            , Reshape [4, 4, 3]
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



    -- print . axesPermutation' $ (Equation [] [Multiple []] :: Equation (Axis a))
    -- print $ ellipsisPositionInLhs [B, Ellipsis, H]
    -- print $ ellipsisPositionInLhs [B, H]
    -- print $ inputCompositeAxes S.empty $ fmap (fmap fromEnum) [Single B, Single H]

    pure ()
