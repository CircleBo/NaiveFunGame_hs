class Functor f where  
    fmap :: (a -> b) -> f a -> f b 

(<$>) :: Functor f => (a -> b) -> f a -> f b
infixl 4 <$>

(<$) :: Functor f => a -> f b -> f a
infixl 4 <$
-- The default definition is fmap . const, 
--but this may be overridden with a more efficient version.
--------------------------------
--Minimal complete definition
--fmap
--------------------------------
--fmap id == id
--fmap (f.g) == fmap f . fmap g
--------------------------------

instance Functor [] where  
    fmap = map  

instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing

-- A phantom type is a parametrised type whose parameters
--do not all appear on the right-hand side of its definition.
newtype Const a b = Const { getConst :: a }

instance Functor (Const a)where
    fmap f c = c
-- Verify   
    --fmap id == id
    --fmap (f.g) == fmap f . fmap g

-------------------------------------------------------------------------------------------
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
infixl 4 <*>
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
--------------------------------------------
--Minimal complete definition
--pure, ((<*>) | liftA2)
--A minimal complete definition must include implementations of pure and of either <*> or liftA2. 
--If it defines both, then they must behave the same as their default definitions:
--(<*>) == liftA2 id 
--liftA2 f x y == f <$> x <*> y

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

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
infixl 4 <**>

--Sequence actions, discarding the value of the first argument:
(*>) :: Applicative f => f a -> f b -> f b
infixl 4 *>

--Sequence actions, discarding the value of the second argument:
(<*) :: Applicative f => f a -> f b -> f a
infixl 4 <*

--default definitions, which may be overridden with equivalent specialized implementations:
u *> v = (id <$ u) <*> v
u <* v = liftA2 const u v

--------------------------------------------
instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs] 
    liftA2 f xs ys = [f x y | x <- xs, y <- ys]
    xs *> ys  = [y | _ <- xs, y <- ys]
--------------------------------------------
instance Applicative Maybe where
    pure = Just

    Just f  <*> m       = fmap f m
    Nothing <*> _m      = Nothing

    liftA2 f (Just x) (Just y) = Just (f x y)
    liftA2 _ _ _ = Nothing

    Just _m1 *> m2      = m2
    Nothing  *> _m2     = Nothing

-------------------------------------------------------------------------------------------
class Monad m where  
    return :: a -> m a  
    
    (>>=) :: m a -> (a -> m b) -> m b  
    
    (>>) :: m a -> m b -> m b  
    x >> y = x >>= \_ -> y  
    
    fail :: String -> m a  
    fail msg = error msg  
-------------------------------------------------------------------------------------------
--Minimal complete definition
--(>>=)
--------------------------------------------
{- | The 'Monad' class defines the basic operations over a /monad/,
a concept from a branch of mathematics known as /category theory/.
From the perspective of a Haskell programmer, however, it is best to
think of a monad as an /abstract datatype/ of actions.
Haskell's @do@ expressions provide a convenient syntax for writing
monadic expressions.

Instances of 'Monad' should satisfy the following laws:

* @'return' a '>>=' k  =  k a@
* @m '>>=' 'return'  =  m@
* @m '>>=' (\\x -> k x '>>=' h)  =  (m '>>=' k) '>>=' h@

Furthermore, the 'Monad' and 'Applicative' operations should relate as follows:

* @'pure' = 'return'@
* @('<*>') = 'ap'@

The above laws imply:

* @'fmap' f xs  =  xs '>>=' 'return' . f@
* @('>>') = ('*>')@

and that 'pure' and ('<*>') satisfy the applicative functor laws.

The instances of 'Monad' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
defined in the "Prelude" satisfy these laws.
-}
--------------------------------------------
class Applicative m => Monad m where
    -- | Sequentially compose two actions, passing any value produced
    -- by the first as an argument to the second.
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b

    -- | Sequentially compose two actions, discarding any value produced
    -- by the first, like sequencing operators (such as the semicolon)
    -- in imperative languages.
    (>>)        :: forall a b. m a -> m b -> m b
    m >> k = m >>= \_ -> k -- See Note [Recursive bindings for Applicative/Monad]
    {-# INLINE (>>) #-}

    -- | Inject a value into the monadic type.
    return      :: a -> m a
    return      = pure

    -- | Fail with a message.  This operation is not part of the
    -- mathematical definition of a monad, but is invoked on pattern-match
    -- failure in a @do@ expression.
    --
    -- As part of the MonadFail proposal (MFP), this function is moved
    -- to its own class 'MonadFail' (see "Control.Monad.Fail" for more
    -- details). The definition here will be removed in a future
    -- release.
    fail        :: String -> m a
    fail s      = errorWithoutStackTrace s

--------------------------------------------
join              :: (Monad m) => m (m a) -> m a
join x            =  x >>= id

ap                :: (Monad m) => m (a -> b) -> m a -> m b
ap m1 m2          = do { x1 <- m1; x2 <- m2; return (x1 x2) }
-- Since many Applicative instances define (<*>) = ap, we
-- cannot define ap = (<*>)

(<$!>) :: Monad m => (a -> b) -> m a -> m b
infixl 4 <$!>

(>>=) :: Monad m => m a -> (a -> m b) -> m b
infixl 1 >>=

--Same as '>>=', but with the arguments interchanged    
(=<<) :: Monad m => (a -> m b) -> m a -> m b
infixr 1 =<<
f =<< x         = x >>= f

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
---------------------------------------------
instance Monad []  where
    xs >>= f             = [y | x <- xs, y <- f x]
    (>>) = (*>)
    fail _              = []
---------------------------------------------
instance Functor ((->) r) where
    fmap = (.)

--const :: a -> b -> a
--const x is a unary function which evaluates to x for all inputs.
instance Applicative ((->) a) where
pure = const
(<*>) f g x = f x (g x)
liftA2 q f g x = q (f x) (g x)

instance Monad ((->) r) where
    f >>= k = \ r -> k (f r) r

--do notation
six = sum $ do 
    x<-[1,2,3] 
    return x


-- | The class of monoids (types with an associative binary operation that
-- has an identity).  Instances should satisfy the following laws:
--
--  * @mappend mempty x = x@
--
--  * @mappend x mempty = x@
--
--  * @mappend x (mappend y z) = mappend (mappend x y) z@
--
--  * @mconcat = 'foldr' mappend mempty@
--
-- The method names refer to the monoid of lists under concatenation,
-- but there are many other instances.
--
-- Some types can be viewed as a monoid in more than one way,
-- e.g. both addition and multiplication on numbers.
-- In such cases we often define @newtype@s and make those instances
-- of 'Monoid', e.g. 'Sum' and 'Product'.

class Monoid a where
    mempty  :: a
    -- ^ Identity of 'mappend'
    mappend :: a -> a -> a
    -- ^ An associative operation
    mconcat :: [a] -> a

    -- ^ Fold a list using the monoid.
    -- For most types, the default definition for 'mconcat' will be
    -- used, but the function is included in the class definition so
    -- that an optimized version can be provided for specific types.

    mconcat = foldr mappend mempty


instance Monoid [a] where
    {-# INLINE mempty #-}
    mempty  = []
    {-# INLINE mappend #-}
    mappend = (++)
    {-# INLINE mconcat #-}
    mconcat xss = [x | xs <- xss, x <- xs]
---------------------------------------------
instance Monoid b => Monoid (a -> b) where
    mempty _ = mempty
    mappend f g x = f x `mappend` g x

-- -----------------------------------------------------------------------------
-- The Alternative class definition

infixl 3 <|>
(<|>) :: Alternative f => f a -> f a -> f a

--------------------------------------------
---- | A monoid on applicative functors.----
--------------------------------------------


--
-- If defined, 'some' and 'many' should be the least solutions
-- of the equations:
--
-- * @some v = (:) '<$>' v '<*>' many v@
--
-- * @many v = some v '<|>' 'pure' []@
class Applicative f => Alternative f where
    -- | The identity of '<|>'
    empty :: f a
    -- | An associative binary operation
    (<|>) :: f a -> f a -> f a

    -- | One or more.
    some :: f a -> f [a]
    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = liftA2 (:) v many_v

    -- | Zero or more.
    many :: f a -> f [a]
    many v = many_v
      where
        many_v = some_v <|> pure []
        some_v = liftA2 (:) v many_v


instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> _ = l

instance Alternative [] where
    empty = []
    (<|>) = (++)

-- -----------------------------------------------------------------------------
-- The MonadPlus class definition

-- | Monads that also support choice and failure.
class (Alternative m, Monad m) => MonadPlus m where
    -- | the identity of 'mplus'.  It should also satisfy the equations
    --
    -- > mzero >>= f  ==  mzero
    -- > v >> mzero   ==  mzero
    --
    --mzero `mplus` ma == ma `mplus` mzero == ma
    --ma `mplus` (mb `mplus` mc) == (ma `mplus` mb) `mplus` mc
    mzero :: m a
    mzero = empty
 
    -- | an associative operation
    mplus :: m a -> m a -> m a
    mplus = (<|>)

instance MonadPlus []  where
    mzero = []
    mplus xs ys = xs ++ ys 
 