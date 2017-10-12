
1. 把"class Functor f where  
    fmap :: (a -> b) -> f a -> f b "
   "class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b"成为Functor，Applicative的定义？
2. "u *> v = (id <$ u) <*> v" , 当 u==Nothing？？？
    u *> v = v 难道不是更省事？
3.  