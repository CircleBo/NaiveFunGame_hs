import Data.List
import Data.Char
import Control.Applicative
data Alphabet a = Alphabet [a] deriving (Show,Eq) 
thisAlphabet = Alphabet  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-*/\\ \n\r\t!@#$%^&|(){}<>[]="

data RE a = RE [a] | Alt [RE a] deriving (Show,Eq)

alternation :: [RE a]->RE a
alternation x = Alt x

concatenation :: RE a->RE a->RE a
concatenation (RE a) (RE b) = RE (a++b)
concatenation (RE a) (Alt b) = Alt (fmap (concatenation (RE a)) b )
concatenation (Alt a) (RE b) = Alt (fmap (flip concatenation (RE b)) a )
concatenation (Alt a) (Alt b)= Alt (liftA2 concatenation a b)

kleene :: RE a->RE a
kleene re = Alt (Data.List.concat (fmap (flip Data.List.replicate re) [1..]))
-- a(b|c)*
x = concatenation (RE "a")  (kleene (alternation [RE "b",RE "c"]))
----------------------------------------------------------------------------------
data State a = InitialState | AcceptedState | Concat (State a) a | Eclosure (State a) Int
data NFA a =  NFA { nfastates::[State a],
                    nfatransition::State a->a->State a,
                    nfastart :: State a ,
                    nfaaccept :: State a,
                    nfaalphabet::Alphabet a }

data DFA a =  DFA { dfastates::[State a],
                    dfatransition::State a->a->State a,
                    dfastart :: State a ,
                    dfaaccept :: State a,
                    dfaalphabet::Alphabet a }

thompsonstates :: RE a -> [State a]
thompsonstates (RE [])      = [InitialState,AcceptedState]
thompsonstates (RE [a])     = [InitialState,Concat InitialState a, AcceptedState]
thompsonstates (RE (x:xs))  = scanl Concat (Concat InitialState x) xs ++ [InitialState,AcceptedState]
thompsonstates (Alt y) =  e ++ [InitialState,AcceptedState] ++ concat (fmap thompsonstates y)
    where e= fmap (Eclosure InitialState) [1..length y]
--thompsontransition :: RE a -> nfatransition


--thompson :: RE a -> NFA a
--thompson (RE [])=NFA{nfastart=InitialState,nfaalphabet=thisAlphabet,nfaaccept=AcceptedState,}
