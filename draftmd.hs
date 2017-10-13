(++) <$> ["ha","heh","hmm"] <*> ["?","!","."]  

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show) 

<> <$> <+> <*> <|> >>=
--
infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
--
infixl 4 <*>
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
--
infixl 1 >>=
(>>=) :: Monad m => m a -> (a -> m b) -> m b
--
infixl 1 >>
(>>) :: Monad m => m a -> m b -> m b
--