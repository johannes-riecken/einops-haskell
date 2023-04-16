{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.Free
import GHC.Types
import Iso.Deriving

-- Shape is a LeafTree
-- TODO: Make dependent type to enforce tensor invariant
data Shape a = Atom a | Nested [Shape a]
    deriving (Functor, Applicative, Monad) via (Free [] `As1` Shape)

-- data Vect (n :: Nat) a where
--   Nil  :: Vect Nil a
--   Cons :: a -> Vect n a -> Vect (Cons () n) a

-- data Tensor :: forall n. Vect n Nat -> Type -> Type where
--     Scalar :: a -> Tensor Nil a
--     Dimension :: Vect n (Tensor d a) -> Tensor (Cons n d) a

type Shape' = Free []

instance Project (Shape' a) (Shape a) where
    prj (Atom x) = Pure x
    prj (Nested xs) = Free $ fmap prj xs

instance Inject (Shape' a) (Shape a) where
    inj (Pure x) = Atom x
    inj (Free xs) = Nested $ fmap inj xs

instance Isomorphic (Shape' a) (Shape a)

deriving instance Show a => Show (Shape a)
deriving instance Eq a => Eq (Shape a)
deriving instance Ord a => Ord (Shape a)

-- Scalar has a unit, a zero-dimensional one
-- Vect has as additional info a list of units (a one-dimensional vector), giving the length
-- Tensor has as additional info a list of lists of units, giving the length per dimension
-- Hypertensor has as additional info a list of lists of lists of units

main :: IO ()
main = pure ()
