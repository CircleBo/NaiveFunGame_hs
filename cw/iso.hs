module ISO where
    
    
    import Data.Void
    -- A type of `Void` have no value.
    -- So it is impossible to construct `Void`,
    -- unless using undefined, error, unsafeCoerce, infinite recursion, etc
    -- And there is a function
    -- absurd :: Void -> a
    -- That get any value out of `Void`
    -- We can do this becuase we can never have void in the zeroth place.
    
    -- so, when are two type, `a` and `b`, considered equal?
    -- a definition might be, it is possible to go from `a` to `b`,
    -- and from `b` to `a`.
    -- Going a roundway trip should leave you the same value.
    -- Unfortunately it is virtually impossible to test this in Haskell.
    -- This is called Isomorphism.
    
    type ISO a b = (a -> b, b -> a)
    
    
    
    -- given ISO a b, we can go from a to b
    substL :: ISO a b -> (a -> b)
    substL = fst
    
    -- and vice versa
    substR :: ISO a b -> (b -> a)
    substR = snd
    
    -- There can be more than one ISO a b
    isoBool :: ISO Bool Bool
    isoBool = (id, id)
    
    isoBoolNot :: ISO Bool Bool
    isoBoolNot = (not, not)
    
    -- isomorphism is reflexive
    refl :: ISO a a
    refl = (id,id)
    -- isomorphism is symmetric
    symm :: ISO a b -> ISO b a
    symm x= (snd x,fst x)
    
    -- isomorphism is transitive
    trans :: ISO a b -> ISO b c -> ISO a c
    trans ab bc =  ((fst bc).(fst ab),(snd ab).(snd bc))
    
    -- We can combine isomorphism:
    isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
    isoTuple (ab, ba) (cd, dc) = 
      (   \(a, c) -> (ab a, cd c), \(b,d) -> (ba b,dc d)   )
    
    
    fab :: (a->b)->[a]->[b] 
    fab foo [] = []
    fab foo (x:xs) = (foo x):(fab foo xs)
    isoList :: ISO a b -> ISO [a] [b]
    isoList (ab,ba) = (fab ab,fab ba) 
    
    mab :: (a->b)->(Maybe a)->(Maybe b)
    mab foo Nothing = Nothing
    mab foo (Just a) = Just (foo a)
    isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
    isoMaybe (ab, ba) = (mab ab,mab ba)
    -------------------------------------------------------------------------------
    isoei :: (a->b)-> (c->d)-> (Either a c) -> (Either b d)
    isoei ab cd (Left a)  = Left  (ab a)
    isoei ab cd (Right c) = Right (cd c) 
    isoie :: (b->a)-> (d->c)-> (Either b d) -> (Either a c)
    isoie ba dc (Left b)  = Left  (ba b)
    isoie ba dc (Right d) = Right (dc d)
    
    isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
    isoEither (ab,ba) (cd,dc) = 
        (   isoei  ab cd  ,  isoie ba dc    )
    -----------------------------------------------------------------------------
    isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
    isoFunc (ab,ba) (cd,dc) =
        ( \ac -> cd.ac.ba , \bd -> dc.bd.ab )             
    ---------------------------------------------------------------
    unJust::Maybe a->a
    unJust (Just x) = x

    unab :: (Maybe x -> Maybe y)-> x-> y
    unab mab a= case (mab (Just a) )of 
        Just b -> unJust (mab (Just a))

    unba :: (Maybe x -> Maybe y)-> x-> y
    unba mba b= case (mba (Just b) )of 
        Just a -> unJust (mba (Just b))
    -- Going another way is hard (and is generally impossible)
    isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
    isoUnMaybe (mab,mba) = (unab mab,unba mba)
    -- Remember, for all valid ISO, converting and converting back
    -- Is the same as the original value.
    -- You need this to prove some case are impossible.
    -- We cannot have
    -- isoUnEither :: ISO (Either a b) (Either c d) -> ISO a c -> ISO b d.

    -- Note that we have
    ff :: (Either [()] () ) -> (Either [()] Void)
    ff (Left x) = Left x
    ff (Right x)= Right undefined
    gg :: (Either [()] Void)-> (Either [()] () )
    gg (Right x) = Right ()
    gg (Left x) = Left x
    isoEU :: ISO (Either [()] ()) (Either [()] Void)
    isoEU = (ff,gg)
    -- where (), the empty tuple, has 1 value, and Void has 0 value
    -- If we have isoUnEither,
    -- We have ISO () Void by calling isoUnEither isoEU
    -- That is impossible, since we can get a Void by substL on ISO () Void
    -- So it is impossible to have isoUnEither
    
    -- And we have isomorphism on isomorphism!
    
    isoSymm :: ISO (ISO a b) (ISO b a)
    isoSymm = ( \abba -> (snd abba, fst abba)  , \baab ->(snd baab,fst baab) )
    
    
