
--BOOL
--True || False
--True && False
--not True
--odd 7
--2 == 3
--2 /= 3
-- ^:integer **:real
--(mod 10 3 )== 1
--min 8 9
--max 5 6
--succ 8
--(div (-5) (-2))==2
--(div (-5) (2))==-3
doubleMe x = x+x
doublesmall x = if (>) x 2
    then x
    else (*) x 2
doubleSmallNumber x = (if x > 100 then x else x*2) + 1 
abs  x = if (<) x 0
    then -x
    else x
-- <Only in GHCi
--let a = 1>
---[1,2,3]++[34,5]
-- 2 : [3,4,5,6]
--[0,1,2,3,4] !! 2 == 2    ####Index
--[3,2,1] > [2,1,0]    ####lexicographical order
--[3,4,2] > [3,4]
--head last init tail
--length [1,2,3]
-- null reverse
--maximum minimum
--drop take sum product
--elem 'a' ['a'..'z']
--generating infinite lists: cycle for list, repeat for element
--finite: replicate
--replicate 3 10
--list comprehension
--[x+2|x<-[1..100],mod x 2 ==1]
--let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
-- [ [ x | x <- xs, even x ] | xs <- xxs] 
--Tuple
--fst snd
-- zip ['a'..'z'] [1..]
--Typeclass && Type Variable
--(==) :: (Eq a) => a -> a -> Bool
--Show Read Eq Ord Enum Bounded Num Integral Floating

--Pattern Matching  Guards Where!?
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          (skinny, normal, fat) = (18.5, 25.0, 30.0)  

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  
--Let it be
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea
--let <bindings> in <expression>
--let bindings are expressions themselves. where bindings are just syntactic constructs
--ghci> 4 * (if 10 > 5 then 10 else 0) + 2  
--42
--ghci> [let square x = x * x in (square 5, square 3, square 2)]  
--[(25,9,4)] 
--ghci> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)  
--(6000000,"Hey there!")  
--ghci> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)  
--(6000000,"Hey there!")  
calcletBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcletBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
--We omitted the in part of the let binding when we used them in list comprehensions 
--because the visibility of the names is already predefined there. 
--Since let bindings are expressions and are fairly local in their scope,
--they can't be used across guards.
--Case expressions
headPM' :: [a] -> a  
headPM' [] = error "No head for empty lists!"  
headPM' (x:_) = x

headCE' :: [a] -> a  
headCE' xs = case xs of [] -> error "No head for empty lists!"  
                        (x:_) -> x

describeListCE :: [a] -> String  
describeListCE xs = "The list is " ++ case xs of [] -> "empty."  
                                                 [x] -> "a singleton list."   
                                                 xs -> "a longer list."  

describeListPM :: [a] -> String  
describeListPM xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."           
--recursion
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x
--Quicksort
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

quicksort' :: (Ord a) => [a] -> [a]    
quicksort' [] = []    
quicksort' (x:xs) =     
    let smallerSorted = quicksort' (filter (<=x) xs)  
        biggerSorted = quicksort' (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted   
--Curried Function
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]  
map' _ [] = []  
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]  
filter' _ [] = []  
filter' p (x:xs)   
    | p x       = x : filter' p xs  
    | otherwise = filter' p xs

flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f = g  
    where g x y = f y x
--lambda
--ghci> zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]  
--[153.0,61.5,31.0,15.75,6.6]  
addThree :: (Num a) => a -> a -> a -> a  
addThree x y z = x + y + z

addThree' :: (Num a) => a -> a -> a -> a  
addThree' = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c  
flip'' f = \x y -> f y x
--
--foldl foldr foldl1 foldr1
--scanl scanr scanl1 scanr1
--
--f (g (z x)) is equal to f $ g $ z x
--
--ghci> map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]  
--[-14,-15,-27]
--ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]]  
--[-14,-15,-27] 
--
--ADT
data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)  
--ghci> :t Circle  
--Circle :: Float -> Float -> Float -> Shape
--If you want to export all the value constructors for a given type, just write ..

--Record syntax
data Person = Person String String Int Float String String deriving (Show)
firstName :: Person -> String  
firstName (Person firstname _ _ _ _ _) = firstname  
  
lastName :: Person -> String  
lastName (Person _ lastname _ _ _ _) = lastname  
  
age :: Person -> Int  
age (Person _ _ age _ _ _) = age  
  
height :: Person -> Float  
height (Person _ _ _ height _ _) = height  
  
phoneNumber :: Person -> String  
phoneNumber (Person _ _ _ _ number _) = number  
  
flavor :: Person -> String  
flavor (Person _ _ _ _ _ flavor) = flavor
--
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq, Show, Read)

--Type parameters
--data Maybe a = Nothing | Just a 
--If we were defining a mapping type, we could add a typeclass constraint in the data declaration:
--data (Ord k) => Map k v = ...
--However, it's a very strong convention in Haskell to never add typeclass constraints in data declarations.
--When declaring a data type, the part before the = is the type constructor
-- and the constructors after it (possibly separated by |'s) are value constructors.
data Vector a = Vector a a a deriving (Show)
--ghci> Vector " " "sdfs" "f35sdf3"
--Vector " " "sdfs" "f35sdf3"
--it :: Vector [Char]
--Derived instances
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
--typedef
type String = [Char] 
--Type synonyms can also be parameterized. 
--type AssocList k v = [(k,v)] 
--AssocList is a type constructor that takes two types and produces a concrete type
type IntMap v = Map.Map Int v
type IntMap = Map.Map Int 
--Recursive data structures
infixr 5  ++++ 
(++++) :: [a] -> [a] -> [a]  
[]     ++++ ys = ys  
(x:xs) ++++ ys = x : (xs ++ ys)
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right
--Typeclass
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"

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
--Functor
--class Functor f where  
--    fmap :: (a -> b) -> f a -> f b  
--fmap :: (a -> b) -> f a -> f b
--fmap :: (a -> b) -> (f a -> f b)
--lifting
--ghci> :t fmap (*2)  
--fmap (*2) :: (Num a, Functor f) => f a -> f a
--You can think of fmap as either a function that takes a function and a functor 
--and then maps that function over the functor, 
--or you can think of it as a function that takes a function and lifts that function 
--so that it operates on functors. 
--Both views are correct and in Haskell, equivalent.
--functor laws
--The first functor law states that 
--if we map the id function over a functor, 
--the functor that we get back should be the same as the original functor.
--The second law says that 
--composing two functions and then mapping the resulting function over a functor 
--should be the same as first mapping one function over the functor 
--and then mapping the other one. 
--fmap (f . g) == fmap f . fmap g.
--fmap (f . g) F == fmap f (fmap g F).
data CMaybe a = CNothing | CJust Int a deriving (Show)
--Kinds and some type-foo
--A kind is more or less the type of a type
--ghci> :k Int  
--Int :: *
--ghci> :k Maybe  
--Maybe :: * -> * 
--    ghci> :k Maybe Int  
--Maybe Int :: *  
--ghci> :k Either  
--Either :: * -> * -> *
class Tofu t where  
    tofu :: j a -> t a j
data Frank a b  = Frank {frankField :: b a} deriving (Show)
--Applicative functors
 
--Monoid
--mempty `mappend` x = x
--x `mappend` mempty = x
--(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

--
--
--