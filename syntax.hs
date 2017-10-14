--record syntax
data CAR = Car {company :: String, model :: String, year :: Int} deriving (Show,Eq)   
--ghci> Car "x" "z" 3 == Car {company = "x", model = "z", year = 3}
--True
--it :: Bool