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

data State a = InitialState | AcceptedState | Concat (State a) a
data NFA a =  DFA { states::[State a],
                    transition::State a->a->State a,
                    start :: State a ,
                    accept :: State a,
                    alphabet::Alphabet a }

data DFA a =  DFA { states::[State a],
                    transition::State a->a->State a,
                    start :: State a ,
                    accept :: State a,
                    alphabet::Alphabet a }

thompson :: RE a -> NFA a







