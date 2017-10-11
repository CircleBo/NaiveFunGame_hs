class Functor f where  
    fmap :: (a -> b) -> f a -> f b 

instance Functor [] where  
    fmap = map  

class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something

class Monad m where  
    return :: a -> m a  
    
    (>>=) :: m a -> (a -> m b) -> m b  
    
    (>>) :: m a -> m b -> m b  
    x >> y = x >>= \_ -> y  
    
    fail :: String -> m a  
    fail msg = error msg  

instance Monad Maybe where  
    return x = Just x  
    Nothing >>= f = Nothing  
    Just x >>= f  = f x  
    fail _ = Nothing

