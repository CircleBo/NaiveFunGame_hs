--record syntax
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)  
--ghci> Car {company="Ford", model="Mustang", year=1967}  
--Car {company = "Ford", model = "Mustang", year = 1967}  