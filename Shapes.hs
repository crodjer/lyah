{-# LANGUAGE GADTs #-}

import Data.Monoid

newtype Rectangle a = Rectangle (a, a) deriving Show
newtype Circle a = Circle a deriving Show
data AnyShape a where
  AnyShape :: (Shape s, Floating a, Show (s a)) => s a -> AnyShape a
newtype Shapes a = Shapes [AnyShape a] deriving Show

class Shape shape where
  area :: (Floating a) => shape a -> a
  perimeter :: (Floating a) => shape a -> a

instance Shape Rectangle where
  area (Rectangle (l, b)) = l * b
  perimeter (Rectangle (l,  b)) = 2 * (l + b)

instance Shape Circle where
  area (Circle r) = pi * r * r
  perimeter (Circle r) = 2 * pi * r

instance Show (AnyShape a) where
  show (AnyShape sa) = "Shape " ++ show sa

instance Shape AnyShape where
  area (AnyShape sa) = area sa
  perimeter (AnyShape sa) = area sa

instance Shape Shapes where
  area (Shapes ss) = sum(map area ss)
  perimeter (Shapes ss) = sum(map perimeter ss)

shapes :: (Floating a, Show a) => Shapes a
shapes = Shapes [AnyShape (Circle 3), AnyShape (Rectangle (2, 2))]
