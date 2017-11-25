import Data.List
import Control.Applicative
data Alphabet a = Alphabet [a] deriving (Show,Eq) 
thisAlphabet = Alphabet  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-*/\\ \n\r\t!@#$%^&|(){}<>[]="

data RE a = RE [a] | Alt [RE a] deriving (Show)

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

data NFAState a = InitialState | AcceptedState | Concat (NFAState a) a | Eclosure (NFAState a) Int deriving (Show)
data NFA a =  NFA { nfastates::[NFAState a],
                    nfatransition::NFAState a->a->NFAState a,
                    nfastart :: NFAState a ,
                    nfaaccept :: NFAState a,
                    nfaalphabet::Alphabet a }


thompsonstates :: RE a -> [NFAState a]
thompsonstates (RE [])      = [InitialState,AcceptedState]
thompsonstates (RE [a])     = [InitialState,Concat InitialState a, AcceptedState]

thompsonstates (RE (x:xs))  = scanl Concat (Concat InitialState x) xs ++ [InitialState,AcceptedState]

thompsonstates (Alt y) =  e ++ [InitialState,AcceptedState] ++ concat (fmap thompsonstates y)
    where e= fmap (Eclosure InitialState) [1..length y]