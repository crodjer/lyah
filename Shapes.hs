{-# LANGUAGE ExistentialQuantification #-}

import Data.Monoid

newtype Rectangle a = Rectangle (a, a) deriving Show
newtype Circle a = Circle a deriving Show
data AnyShape a = forall s a. (Shape s, Floating a, Show (s a)) => AnyShape (s a)

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

-- anyShapeArea :: (Shape s, Num a, Show (s a)) => AnyShape a -> s a
anyShapeArea (AnyShape sa) =  (area sa)

-- For AnyShape to be of any use, ^ should work. I can't extract anything
-- useful out of it.

-- instance Shape (AnyShape) where
--   area (AnyShape sa) = area sa
--   perimeter (AnyShape sa) = area sa

shapes :: Floating a => [AnyShape a]
shapes = [AnyShape (Circle 3), AnyShape (Rectangle (2, 2))]
