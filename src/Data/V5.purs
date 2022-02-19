
module Data.V5 where

import Prelude
import Data.Traversable
import Control.Apply

newtype V5 a = V5
    { x1 :: a
    , x2 :: a
    , x3 :: a
    , x4 :: a
    , x5 :: a
    }

derive instance Functor V5
derive instance Eq a => Eq (V5 a)

mkV5 :: forall a. a -> a -> a -> a -> a -> V5 a
mkV5 x1 x2 x3 x4 x5 = V5 {x1, x2, x3, x4, x5}

unV5 :: forall a. V5 a -> { x1 :: a, x2 :: a, x3 :: a, x4 :: a, x5 :: a }
unV5 (V5 x) = x

instance Apply V5 where
    apply (V5 f) (V5 x) = V5
      { x1: f.x1 x.x1
      , x2: f.x2 x.x2
      , x3: f.x3 x.x3
      , x4: f.x4 x.x4
      , x5: f.x5 x.x5
      }

instance Applicative V5 where
    pure x = V5 { x1: x, x2: x, x3: x, x4: x, x5: x }

instance Foldable V5 where
    foldr f z (V5 x) = f x.x1 (f x.x2 (f x.x3 (f x.x4 (f x.x5 z))))
    foldl f z (V5 x) = f (f (f (f (f z x.x1) x.x2) x.x3) x.x4) x.x5
    foldMap f (V5 x) = f x.x1 <> f x.x2 <> f x.x3 <> f x.x4 <> f x.x5

instance Traversable V5 where
    traverse f (V5 x) = mkV5 <$> f x.x1 <*> f x.x2 <*> f x.x3 <*> f x.x4 <*> f x.x5
    sequence (V5 x) = mkV5 <$> x.x1 <*> x.x2 <*> x.x3 <*> x.x4 <*> x.x5

instance Semigroup a => Semigroup (V5 a) where
    append = lift2 append

instance Monoid a => Monoid (V5 a) where
    mempty = pure mempty

data Ix5 = Ix5_0 | Ix5_1 | Ix5_2 | Ix5_3 | Ix5_4

