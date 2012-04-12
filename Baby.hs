module Baby where

import qualified Data.Map as Map
import System.Random


doubleMe :: Num a => a -> a
doubleMe x = x * 2
doubleUs :: (Num a) => a -> a -> a
doubleUs x y = doubleMe (x + y)

doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x > 100
                        then x
                        else x*2

doubleSmallNumber' :: Int -> Int
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1


boomBangs :: [Int] -> [String]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

lucky :: Int -> String
lucky 7 = "Lucky No. Seven!"
lucky _ = "Sorry, you're out of luck, pal!"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n-1)

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital string@(x:_) = "The first letter of " ++ string ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pfft, I bet you're ugly!"
    | bmi <= fat = "You're a fat! Lose some weight, fatty!"
    | otherwise                   = "You're a whale, congratulations!"
    where bmi = weight / (height * height)
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ "." ++ [l] ++ "."
initials _ _ = ""

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / (h * h)]

head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

{-
 -elem' :: (Eq a) => a -> [a] -> Bool
 -elem' a [] = False
 -elem' a(x:xs)
 -    | a == x    = True
 -    | otherwise = a `elem'` xs
 -}

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x ]
    in smallerSorted ++ [x] ++ biggerSorted

zipWith' :: (a -> b-> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)
chain _ = [0]

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1::Int ..100]))

sum' :: (Num a) => [a] -> a
sum' = sum
{-sum' = foldl (+) 0-}

elem' :: (Eq a) => a -> [a] -> Bool
elem' y = foldl (\acc x -> (x == y) || acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x :acc) []

{-
 -phoneBook =
 -    [("betty","555-2938")
 -    ,("bonnie","452-2928")
 -    ,("patsy","493-2928")
 -    ,("lucille","205-2928")
 -    ,("wendy","939-8282")
 -    ,("penny","853-2492")
 -    ]
 -}

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

formList' :: (Ord k) => [(k,v)] -> Map.Map k v
formList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty


phoneBookMult :: [(String, String)]
phoneBookMult =
    [("betty","555-2938")
    ,("patsy","943-2929")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("betty","342-2492")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")
    ]

phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs

{-
 -data Person = Person String String Int Float String String deriving (Show)
 -}

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     {-
                      -, height :: Float
                      -, phoneNumber :: String
                      -, flavor :: String
                      -}
                     } deriving (Eq, Show, Read)

data Car = Car { company :: String
                , model :: String
                , year :: Int
                } deriving (Show)
tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

{-
 -data (Show a, Show b, Show c) => Car a b c = Car { company :: a
 -                                       , model :: b
 -                                       , year :: c
 -                                       } deriving (Show)
 -
 -tellCar :: Car -> String
 -tellCar (Car {company = c, model = m, year = y}) = "This " ++ show c ++ " " ++ show m ++ " was made in " ++ show y
 -
 -}

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

phoneBook :: [(String,String)]
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type AssocList k v = [(k,v)]
type IntMap = Map.Map Int

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber m =
    case Map.lookup lockerNumber m of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " does't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker number " ++ show lockerNumber ++ " is already taken!"

{-
 -data List a = Empty | Cons {listHEAD :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
 -}
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infix 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | TreeNode a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = TreeNode x EmptyTree EmptyTree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (TreeNode a left right)
    | x == a = TreeNode x left right
    | x < a = TreeNode a (treeInsert x left) right
    | x > a = TreeNode a left (treeInsert x right)
treeInsert _ _ = EmptyTree

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (TreeNode a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right
treeElem _ _ = False

{-
 -class Eq a where
 -    (==) :: a -> a -> Bool
 -    (/=) :: a -> a -> Bool
 -    x == y = not (x /= y)
 -    x /= y = not (x == y)
 -}

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = True

instance Show TrafficLight where
    show Red = "Red Light"
    show Yellow = "Yellow Light"
    show Green = "Green Light"

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap f (TreeNode x leftsub rightsub) = TreeNode (f x) (fmap f leftsub) (fmap f rightsub)

class Tofu t where
    tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
    tofu = Frank

data Barry t k p = Barry { yabba :: p, dabba :: t k } deriving (Show)

instance Functor (Barry a b) where
    fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, _) = random newGen'
    in (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms newGen

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n - 1) newGen
    in (value:restOfList, finalGen)

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction (x:y:ys) "/" = (y / x):ys
            foldingFunction (x:y:ys) "^" = (y ** x):ys
            foldingFunction (x:ys) "ln" = log x:ys
            foldingFunction xs "sum" = [sum xs]
            foldingFunction xs numberString = read numberString:xs

--Counter Maybe. Not a functor
data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
    fmap _ CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)
