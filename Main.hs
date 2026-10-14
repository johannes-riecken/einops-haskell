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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Applicative
import Control.Applicative.Lift
import Control.Arrow
import Control.Monad (mfilter)
import Data.Aeson hiding ((.:), Key)
import qualified Data.Foldable as F
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

instance Keyed Maybe where
    mapWithKey f = fmap (f ())

newtype Axis a = Axis { getAxis :: Maybe a } deriving (Arbitrary,Functor,Applicative,Monad,Foldable,Traversable,Filterable,Keyed)

axis' :: Int -> Axis Int
axis' x = Axis (Just x)

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
data Composite f a = Single { getSingle :: (f a) } | Multiple { getMultiple :: [f a] } deriving (Functor, Foldable, Traversable, Show, Generic, Eq, Ord)

class HFunctor (t :: (Type -> Type) -> (Type -> Type)) where
  hmap :: (forall x. f x -> g x) -> (t f a -> t g a)

instance HFunctor Composite where
    hmap f (Single xs) = Single (f xs)
    hmap f (Multiple xs) = Multiple (map f xs)

-- from Data.Key, which doesn't build for some reason
type family Key (f :: Type -> Type)
type instance Key (g :.: f) = (Key g, Key f)
type instance Key (f :*: g) = Either (Key f) (Key g)
type instance Key (f :+: g) = Either (Key f) (Key g)
type instance Key (Rec1 f) = Key f
type instance Key (M1 i c f) = Key f
type instance Key (Composite f) = (Maybe Int, Key f)
type instance Key (CompList f) = (Int, Maybe Int, Key f)
type instance Key (Map k) = k
type instance Key Maybe = ()
type instance Key Axis = ()

class Functor f => Keyed f where
  mapWithKey :: (Key f -> a -> b) -> f a -> f b

instance (Functor f, Keyed f) => Keyed (Composite f) where
  mapWithKey g (Single fa) =
    Single (mapWithKey (\idx a -> g (Nothing, idx) a) fa)
  mapWithKey g (Multiple fas) =
    Multiple (zipWith (\pos fa -> mapWithKey (\idx a -> g (Just pos, idx) a) fa) [0..] fas)

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

newtype CompList f a = MkCompList { getCompList :: [Composite f a] }
    deriving (Eq, Ord, Show, Functor, Foldable, Arbitrary, ToJSON)

deriving via (Compose [] (Composite f) :: Type -> Type) instance Filterable f => Filterable (CompList f)

instance HFunctor CompList where
    hmap f (MkCompList xs) = MkCompList $ map (hmap f) xs

instance Keyed f => Keyed (CompList f) where
    mapWithKey f (MkCompList xs) = MkCompList (zipWith (\pos comp -> mapWithKey (\(mi, k) a -> f (pos, mi, k) a) comp) [0..] xs)

data Equation g a = Equation {
    inp :: CompList g a
    , outp :: CompList g a
    , axisLengths :: [(g a, Int)]
    } deriving (Show, Generic)

instance (Arbitrary (g a)) => Arbitrary (Equation g a) where
    arbitrary = do
        inp <- arbitrary
        n <- choose (0,10)
        outp <- MkCompList . take n <$> shuffle (getCompList inp)
        axisLengths <- arbitrary
        pure $ Equation{..}
    shrink = genericShrink

instance (ToJSON (g a)) => ToJSON (Equation g a)

newtype CC f a = CC (Compose [] (Composite f) a) deriving (Functor, Foldable, Traversable, Applicative, Alternative)

-- smart constructor
cc :: [Composite f a] -> CC f a
cc = CC . Compose

axisNumsFromCompList :: (Ord a, Foldable f) => CompList f a -> M.Map a Int
axisNumsFromCompList = M.fromList . (`zip` [0..]) . F.toList

deriving instance Filterable f => Filterable (CC f)

intersectCompLists :: (Ord a, Filterable f, Foldable f) => CompList f a -> CompList f a -> CompList f a
intersectCompLists xs ys = mapMaybe (\x -> if x `elem` ys then Just x else Nothing) (xs)

-- axesPermutation gives the numbers of flattened output axes
axesPermutation :: Ord a => Equation Axis a -> [Int]
axesPermutation (Equation{..}) = let
    -- intersectCompList is necessary for the correct number ordering here
    axisNums = axisNumsFromCompList (intersectCompLists inp (outp))
    in
    foldr ((:) . (axisNums M.!)) [] $ outp

-- example: reducedElementaryAxes "h w -> h" "max" is [1]
reducedElementaryAxes :: forall a. Ord a => Equation Axis a -> [Int]
reducedElementaryAxes (Equation{..}) = let
    axisNums :: M.Map (a) Int
    axisNums = axisNumsFromCompList inp
    diff = F.toList inp \\ F.toList (outp) :: [(a)]
    in
    map (axisNums M.!) diff

-- inferAxisLengths returns the lengths for all axis names, where a Multiple
-- axis may have at most one unknown lengths, which is then the length from the
-- corresponding shape item divided by all the known lengths of axes in the
-- Multiple
inferAxisLengths :: forall a. Ord a => Equation Axis a -> [Int] -> Map a Int
inferAxisLengths Equation{..} shape =
    M.fromList . F.toList $ mapWithKey (\(i, mj, ()) nm -> case mj of
        Nothing {- Single -} -> (nm, shape !! i)
        _ -> case axisLengthsMap M.!? nm of
            Just n -> (nm, n)
            Nothing -> (nm, shape !! i `div` prodKnownForMultiple) where
                prodKnownForMultiple = foldr ((*)) 1 knownLengthsForMultiple
                -- I guess there must be a generic operator that can index a container by key type?
                knownLengthsForMultiple = catMaybes (map (axisLengthsMap M.!?) (catMaybes (map getAxis . getMultiple . (!! i) . getCompList $ inp)))
        ) inp where
            axisLengthsMap = M.fromList -- seems like axisLengths should always be this map
                . map swap -- swap back to normal order
                . mapMaybe ( -- get rid of the Justs
                    sequenceA -- get the Just on the outside
                    . swap -- get the Just on the right to make it functorial
                    . first (getAxis)
                    ) $ axisLengths

-- TODO: fuse
initShapesWithShape :: Ord a => [Int] -> Equation Axis a -> [Int]
initShapesWithShape shape eqn@Equation{..} = foldr ((:) . (inferAxisLengths eqn shape M.!)) [] $ inp

finalShapesWithShape :: Ord a => [Int] -> Equation Axis a -> [Int]
finalShapesWithShape shape eqn@Equation{..} = foldr ((:) . foldr ((*) . (inferAxisLengths eqn shape M.!)) 1) [] . getCompList $ outp
-- end of reconstruct

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

main :: IO ()
main = do
    hspec $ do
        it "has deep Filterable instance" $
            mapMaybe (mfilter even . Just) (MkCompList [Single (axis' 1), Multiple [axis' 2, axis' 3, axis' 4]])
            `shouldBe`
            MkCompList [Single anon, Multiple [axis' 2, anon, axis' 4]]
        it "generates tf commands for rearrange" $
            applyRecipe sampleShape (Equation {
                inp = MkCompList [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = MkCompList [Single (axis "H"), Multiple [axis "B", axis "W"], Single (axis "C")]
                , axisLengths = []
                }) Nothing
            `shouldBe`
            [
            Reshape [6,4,4,3] -- TODO: remove redundant command
            , Transpose [1,0,2,3]
            , Reshape [4,24,3]
            ]
        it "generates tf commands for reduce" $
            applyRecipe sampleShape (Equation {
                inp = MkCompList [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = MkCompList [Single (axis "B"), Single (axis "C")]
                , axisLengths = []
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
                inp = MkCompList [Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = MkCompList [Multiple [axis "B", axis "H", axis "W", axis "C"]]
                , axisLengths = []
                }) Nothing
            `shouldBe`
            [
            Reshape [6, 4, 4, 3]
            , Transpose [0, 1, 2, 3]
            , Reshape [288]
            ]
        it "generates tf commands for split initial axis" $
            applyRecipe sampleShape (Equation {
                inp = MkCompList [Single (axis "B"), Single (axis "H"), Multiple [axis "I", axis "W"], Single (axis "C")]
                , outp = MkCompList [Single (axis "I"), Single (axis "B"), Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , axisLengths = [(axis "I",2)]
                }) Nothing
            `shouldBe`
            [
            Reshape [6, 4, 2, 2, 3]
            , Transpose [2, 0, 1, 3, 4]
            , Reshape [2, 6, 4, 2, 3]
            ]
        it "generates tf commands for different shape" $
            applyRecipe (drop 1 sampleShape) (Equation {
                inp = MkCompList [Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = MkCompList [Multiple [axis "H", axis "W"], Single (axis "C")]
                , axisLengths = []
                }) Nothing
            `shouldBe`
            [
            Reshape [4, 4, 3]
            , Transpose [0, 1, 2]
            , Reshape [16, 3]
            ]
        it "generates tf commands for anonymous axis" $
            applyRecipe (1 : drop 1 sampleShape) (Equation {
                inp = MkCompList [Single anon, Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , outp = MkCompList [Single (axis "H"), Single (axis "W"), Single (axis "C")]
                , axisLengths = []
                }) Nothing
            `shouldBe`
            [
            Reshape [4, 4, 3]
            , Transpose [0, 1, 2]
            , Reshape [4, 4, 3]
            ]





-- -- --     -- TODO: Support underscore axis

-- -- --     -- PASS
-- -- --     -- quickCheck $ \xs -> axesPermutationPy xs === axesPermutation' xs
-- -- --     -- quickCheck $ \xs -> ellipsisPositionInLhsPy xs === ellipsisPositionInLhs' xs
-- -- --     -- quickCheck $ \xs -> outputCompositeAxesPy xs === outputCompositeAxes' xs

-- -- --     -- print . elementaryAxesLengths' $ (Equation [] [] [(Ellipsis,0)])
-- -- --     -- quickCheck $ \xs -> collect (isRight (elementaryAxesLengths' xs)) $ eitherToMaybe (elementaryAxesLengthsPy xs) === eitherToMaybe (elementaryAxesLengths' xs)

-- -- --     -- let xs = Equation [] [Multiple [axis "Ellipsis"]] in
-- -- --     --     print $ ellipsisPositionInLhsPy xs



-- -- --     -- print . axesPermutation' $ (Equation [] [Multiple []] :: Equation Axis a)
-- -- --     -- print $ ellipsisPositionInLhs [B, Ellipsis, H]
-- -- --     -- print $ ellipsisPositionInLhs [B, H]
-- -- --     -- print $ inputCompositeAxes S.empty $ fmap (fmap fromEnum) [Single B, Single H]

-- -- --     pure ()
