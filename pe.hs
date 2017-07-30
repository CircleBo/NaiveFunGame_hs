
import Prelude hiding (even, odd, div, compare, Num, Int, Integer, Float, Double, Rational, Word)

data Peano = Zero | Succ Peano deriving (Eq, Show)

add, sub, mul, div :: Peano -> Peano -> Peano
-- Addition
add Zero x = x
add x Zero = x
add x (Succ Zero) = Succ x
add (Succ Zero) x = Succ x
add x (Succ y) = Succ (add x y)
-- Subtraction

sub x Zero = x
sub Zero _ = error "negative number"
sub (Succ x) (Succ y) = sub x y
-- Multiplication
mul Zero _ = Zero
mul _ Zero = Zero
mul x (Succ Zero) = x
mul (Succ Zero) x = x
mul x (Succ y) = add x (mul x y)
-- Integer division
div _ Zero = error "divide by 0"
div Zero _ = Zero
div x (Succ Zero) = x
div (Succ Zero) _ = Zero
div x y = case (compare x y) of LT -> Zero
                                EQ -> Succ Zero
                                GT ->  Succ (div (sub x y) y)
even, odd :: Peano -> Bool
-- Even
even Zero = True
even (Succ x)= not (even x)
-- Odd
odd x = not (even x)

compare :: Peano -> Peano -> Ordering
-- Compare
compare Zero Zero = EQ
compare Zero _ = LT
compare _ Zero = GT
compare (Succ x) (Succ y) = compare x y



