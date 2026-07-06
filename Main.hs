{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

import Control.Applicative
import Control.Applicative.Lift
import Control.Arrow
import Data.Aeson hiding ((.:))
import qualified Data.Foldable as F
-- import Data.Tensor hiding (select)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Kind
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe hiding (mapMaybe, catMaybes)
import Data.Tuple (swap)
import GHC.Generics
import Iso.Deriving
import Test.Hspec
import Test.QuickCheck
import Text.Printf
import Witherable hiding (filter)

sampleShape :: [Int]
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

-- axis :: String -> Axis String
-- axis x = Axis (Just x)

axis :: String -> Axis (Identity String)
axis x = (Axis (Just . Identity $ x))

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
data Composite f a = Single (f a) | Multiple [f a] deriving (Functor, Foldable, Traversable, Show, Generic, Eq, Ord)

class HFunctor (t :: (Type -> Type) -> (Type -> Type)) where
  hmap :: (forall x. f x -> g x) -> (t f a -> t g a)

instance HFunctor Composite where
    hmap f (Single xs) = Single (f xs)
    hmap f (Multiple xs) = Multiple (map f xs)

foo :: Composite [] Int
foo = fmap (*2) (Single [1,2,3])

instance Filterable f => Filterable (Composite f) where
    mapMaybe g (Single xs) = Single (mapMaybe g xs)
    mapMaybe g (Multiple xs) = Multiple (map (mapMaybe g) xs)

instance Inject (Compose (Lift []) f a) (Composite f a) where
    inj (Compose (Pure x)) = Single x
    inj (Compose (Other xs)) = Multiple xs

instance Project (Compose (Lift []) f a) (Composite f a) where
    prj ((Single x)) = Compose $ Pure x
    prj ((Multiple xs)) = Compose $ Other xs

instance Isomorphic (Compose (Lift []) f a) (Composite f a)

deriving via As1 @Type (Compose (Lift []) f) (Composite f) instance Applicative f => Applicative (Composite f)

instance Arbitrary (f a) => Arbitrary (Composite f a) where
    arbitrary = oneof [Single <$> arbitrary, Multiple <$> arbitrary]
    shrink = genericShrink

instance ToJSON (f a) => ToJSON (Composite f a)

data EquationStr a = EquationStr {
    eqn :: String
    , axes_lengths :: [(a,Int)]
    } deriving (Generic)

deriving instance Show a => Show (EquationStr a)

instance (Show a, ToJSON a) => ToJSON (EquationStr a) where
    toJSON (EquationStr{..}) =
        object ["eqn" .= eqn, "axes_lengths" .= toJSON (map (first show) axes_lengths)]

data Equation g a = Equation {
    inp :: [Composite g a]
    , outp :: [Composite g a]
    , axesLengths :: [(g a, Int)]
    } deriving (Show, Generic)

instance (Arbitrary (g a)) => Arbitrary (Equation g a) where
    arbitrary = do
        inp <- arbitrary
        n <- choose (0,10)
        outp <- take n <$> shuffle inp
        axesLengths <- arbitrary
        pure $ Equation{..}
    shrink = genericShrink

instance (ToJSON (g a)) => ToJSON (Equation g a)

flatten :: (Foldable f) => [Composite f a] -> [f a]
flatten = (=<<) extract where
    extract (Single x)    = [x]
    extract (Multiple xs) = xs

axisNumsFromCompList :: Ord a => [Composite Axis a] -> M.Map a Int
axisNumsFromCompList = snd . foldl' (\(i,acc) x ->
        (i+1,M.insertWith (\_ _ -> error "no duplicates allowed") x i acc))
        (0,M.empty) . cc

newtype CC f a = CC (Compose [] (Composite f) a) deriving (Functor, Foldable, Traversable, Applicative, Alternative)

deriving instance Filterable f => Filterable (CC f)

-- smart constructor
cc :: [Composite f a] -> CC f a
cc = CC . Compose

uncc :: CC f a -> [Composite f a]
uncc (CC (Compose x)) = x

-- list of axes
newtype CLA a = CLA (Compose [] Axis a) deriving (Functor, Foldable, Traversable, Applicative)

intersectCompLists :: (Ord a, Filterable f, Foldable f) => [Composite f a] -> [Composite f a] -> [Composite f a]
intersectCompLists xs ys = uncc $ mapMaybe (\x -> if x `elem` cc ys then Just x else Nothing) (cc xs)

-- axesPermutation gives the numbers of flattened output axes
axesPermutation :: Ord a => Equation Axis a -> [Int]
axesPermutation (Equation{..}) = let
    -- intersectCompList is necessary for the correct number ordering here
    axisNums = axisNumsFromCompList (intersectCompLists inp outp)
    in
    foldr ((:) . (axisNums M.!)) [] . cc $ outp

catMaybes' :: [Axis a] -> [a]
catMaybes' = mapMaybe getAxis

catMaybes'' :: [Identity (Axis a)] -> [a]
catMaybes'' = mapMaybe (getAxis . runIdentity)

-- example: reducedElementaryAxes "h w -> h" "max" is [1]
-- TODO: fuse
reducedElementaryAxes :: forall a. Ord a => Equation Axis a -> [Int]
reducedElementaryAxes (Equation{..}) = let
    axisNums :: M.Map (a) Int
    axisNums = axisNumsFromCompList inp
    flatten' :: [Composite Axis (a)] -> [(a)]
    flatten' = catMaybes' . flatten
    diff = flatten' inp \\ flatten' outp :: [(a)]
    in
    map (axisNums M.!) $ flatten' inp \\ flatten' outp

-- TODO: fuse
-- TODO: allow deeper nesting of Composite
initMap :: forall a. Ord a => Equation Axis a -> [Int] -> Map a Int
initMap Equation{..} shape =
    let
    sizes = M.fromList . snd $ foldr (\x' (i,acc) -> case x' of
            Single ((Axis (Just x))) -> (i-1,(x, shape !! i):acc)
            Single ((Axis Nothing)) -> (i-1,acc)
            Multiple xs -> (i-1,handleMultiple i (catMaybes' xs) ++ acc)
            ) (length inp - 1,[]) inp
    handleMultiple i xs = map (\x ->
        (x,fromJust $ (axesLengthsMap M.!? x) <|> Just (shape !! i `div` prod xs))
        ) xs
        where
            prod = foldr ((*) . (\k -> M.findWithDefault 1 k axesLengthsMap)) 1
    axesLengthsMap :: Map a Int
    axesLengthsMap = M.fromList . map swap . mapMaybe (sequenceA . swap . first (getAxis)) $ axesLengths
    in sizes

runIdentity' :: [Composite Identity (Axis a)] -> [Composite Axis a]
runIdentity' = map (\c -> case c of
    Single (Identity x) -> Single x
    Multiple xs -> Multiple (map runIdentity xs)
    )

-- TODO: fuse
initShapesWithShape :: Ord a => [Int] -> Equation Axis a -> [Int]
initShapesWithShape shape eqn@Equation{..} = foldr ((:) . (initMap eqn shape M.!)) [] . cc $ inp

-- TODO: allow deeper nesting
-- TODO: fuse
finalShapesWithShape :: Ord a => [Int] -> Equation Axis a -> [Int]
finalShapesWithShape shape eqn@Equation{..} = foldr ((:) . foldr ((*) . (initMap eqn shape M.!)) 1) [] outp
-- end of reconstruct

-- swizzleComposite :: Composite Axis (Identity a) -> Composite Identity (Axis a)
-- swizzleComposite (Single (Axis (Just (Identity x)))) = Single (Identity (Axis (Just x)))
-- swizzleComposite (Single (Axis Nothing)) = Single (Identity (Axis Nothing))
-- swizzleComposite (Multiple xs) = Multiple (map (\case
--     (Axis (Just (Identity x))) -> Identity (Axis (Just x))
--     (Axis Nothing) -> Identity (Axis Nothing)) xs
--     )

-- swizzle :: Equation Identity Axis a -> Equation Axis Identity a
-- swizzle eqn@Equation{..} = Equation {
--     inp = map swizzleComposite inp
--     , outp = map swizzleComposite outp
--     , axesLengths = [] -- sorry
--     }

-- TODO: Implement
getReduceCmds :: String -> [Int] -> [TfCommand]
getReduceCmds op xs = [Reduce op xs]

-- TODO: Generalize reduction type
applyRecipe :: Ord a => [Int] -> Equation Axis a -> Maybe String -> [TfCommand]
applyRecipe shape eqn reductionName = let
    x = initShapesWithShape shape (eqn)
    y = reducedElementaryAxes eqn
    z = axesPermutation eqn
    v = finalShapesWithShape shape (eqn)
    reshapeCmd = Reshape x
    reduceCmds = if null y then [] else getReduceCmds (fromJust reductionName) y
    transposeCmd = Transpose z
    finalReshapeCmd = Reshape v
    in
    [reshapeCmd] ++ reduceCmds ++ [transposeCmd] ++ [finalReshapeCmd]

-- -- --     if len(added_axes) > 0:
-- -- --         tensor = backend.add_axes(tensor, n_axes=len(axes_reordering) + len(added_axes), pos2len=added_axes)

-- -- -- def _apply_recipe(recipe: TransformRecipe, tensor: Tensor, reduction_type: Reduction) -> Tensor:
-- -- --     # this method works for all backends but not compilable with
-- -- --     backend = get_backend(tensor)
-- -- --     init_shapes, reduced_axes, axes_reordering, added_axes, final_shapes = \
-- -- --         _reconstruct_from_shape(recipe, backend.shape(tensor))
-- -- --     tensor = backend.reshape(tensor, init_shapes)
-- -- --     tensor = _reduce_axes(tensor, reduction_type=reduction_type, reduced_axes=reduced_axes, backend=backend)
-- -- --     tensor = backend.transpose(tensor, axes_reordering)
-- -- --     if len(added_axes) > 0:
-- -- --         tensor = backend.add_axes(tensor, n_axes=len(axes_reordering) + len(added_axes), pos2len=added_axes)
-- -- --     return backend.reshape(tensor, final_shapes)

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
            applyRecipe (drop 1 sampleShape) (Equation {
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
            applyRecipe (1 : drop 1 sampleShape) (Equation {
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





--     -- TODO: Support underscore axis

--     -- PASS
--     -- quickCheck $ \xs -> axesPermutationPy xs === axesPermutation' xs
--     -- quickCheck $ \xs -> ellipsisPositionInLhsPy xs === ellipsisPositionInLhs' xs
--     -- quickCheck $ \xs -> outputCompositeAxesPy xs === outputCompositeAxes' xs

--     -- print . elementaryAxesLengths' $ (Equation [] [] [(Ellipsis,0)])
--     -- quickCheck $ \xs -> collect (isRight (elementaryAxesLengths' xs)) $ eitherToMaybe (elementaryAxesLengthsPy xs) === eitherToMaybe (elementaryAxesLengths' xs)

--     -- let xs = Equation [] [Multiple [axis "Ellipsis"]] in
--     --     print $ ellipsisPositionInLhsPy xs



--     -- print . axesPermutation' $ (Equation [] [Multiple []] :: Equation Axis a)
--     -- print $ ellipsisPositionInLhs [B, Ellipsis, H]
--     -- print $ ellipsisPositionInLhs [B, H]
--     -- print $ inputCompositeAxes S.empty $ fmap (fmap fromEnum) [Single B, Single H]

--     pure ()
