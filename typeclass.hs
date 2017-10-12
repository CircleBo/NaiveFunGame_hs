class Functor f where  
    fmap :: (a -> b) -> f a -> f b 

(<$>) :: Functor f => (a -> b) -> f a -> f b
infixl 4 <$>

--------------------------------
--fmap id == id
--fmap (f.g) == fmap f . fmap g
--------------------------------

instance Functor [] where  
    fmap = map  
-------------------------------------------------------------------------------------------
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
--A minimal complete definition must include implementations of pure and of either <*> or liftA2. 
--If it defines both, then they must behave the same as their default definitions:
--(<*>) == liftA2 id liftA2 f x y == f <$> x <*> y

--------------------------------------------
--identity
--pure id <*> v == v    
--composition
--pure (.) <*> u <*> v <*> == u <*> (v <*>)
--homomorphism
--pure f <*> pure x = pure (f x)
--interchange
--u <*> pure y == pure ($y) <*> u
--------------------------------------------



(<*>) :: Applicative f => f (a -> b) -> f a -> f b
infixl 4 <*>

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
infixl 4 <**>
    
(<*) :: Applicative f => f a -> f b -> f a
infixl 4 <*

(*>) :: Applicative f => f a -> f b -> f b
infixl 4 <*
instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something
-------------------------------------------------------------------------------------------
class Monad m where  
    return :: a -> m a  
    
    (>>=) :: m a -> (a -> m b) -> m b  
    
    (>>) :: m a -> m b -> m b  
    x >> y = x >>= \_ -> y  
    
    fail :: String -> m a  
    fail msg = error msg  

(<$!>) :: Monad m => (a -> b) -> m a -> m b
infixl 4 <$!>

(>>=) :: Monad m => m a -> (a -> m b) -> m b
infixl 1 >>=

(>>) :: Monad m => m a -> m b -> m b
infixl 1 >>
---------------------------------------------
--return a >>= k  =  k a
--m >>= return  =  m
--m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
---------------------------------------------
instance Monad Maybe where  
    return x = Just x  
    Nothing >>= f = Nothing  
    Just x >>= f  = f x  
    fail _ = Nothing
