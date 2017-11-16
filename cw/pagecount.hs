module Codewars.Kata.Pagination where
    
    type Collection a = [a]
    type ItemsPerPage = Int
    
    itemCount :: Collection a -> Int
    itemCount = length
    
    pageCount :: Collection a -> ItemsPerPage -> Int
    pageCount xs n = (l - l `mod` n) `div` n + if l`mod`n==0 then 0 else 1
        where l=itemCount xs
    
    pageItemCount :: Collection a -> ItemsPerPage -> Int -> Maybe Int
    pageItemCount xs n page = if page<0 then Nothing
                              else if length xs <= n*(page) then Nothing
                              else if length xs >= (page+1)*n then Just n
                              else Just (length xs `mod` n)
    
    pageIndex :: Collection a -> ItemsPerPage -> Int -> Maybe Int
    pageIndex xs n item = if n<0 || length xs == 0 then Nothing
                          else if item==0 then Just 0
                          else if item<0  then Nothing
                          else if item>length xs then Nothing
                          else Just (item `div` n)