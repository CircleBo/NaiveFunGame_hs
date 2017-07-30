
import Data.List
isempty :: ([Char],[Char],[Char])->Bool
isempty (_,[],[]) = False
isempty _  = True

firstf :: (String,String,String)->String
firstf (a,b,c) = a

transbit :: Char -> Char
transbit '0' = '1'
transbit '1' = '0'
transbit c = c

mainf :: ([Char],[Char],[Char]) -> Char -> ([Char],[Char],[Char])
mainf  (xs,[],[]) _ = (xs,[],[])
mainf  (xs,[x],ys) '1' = (xs,[transbit x],ys)
mainf  (xs,[x],[]) '0' = (xs ++ [x],[],[])
mainf  (xs,[x],y:ys) '0' = (xs ++ [x],[y],ys)

flowf :: Bool -> [Char] -> [Char] ->[([Char],[Char],[Char])]
flowf False _ _ = error"undefined"
flowf True l "" = [([],[],[])]
flowf True l [x] = scanl mainf ([],[x],[])  (cycle l)
flowf True l (x:xs) = scanl mainf ([],[x],xs)  (cycle l)


extractf :: [([Char],[Char],[Char])] -> String
extractf xs= firstf $ head (dropWhile isempty xs)

interpreter :: String -> String -> String
interpreter tape array  = extractf ( flowf (elem '0' tape) tape array )


