mean::(Fractional p)=>[p]->Maybe p
mean []=Nothing
mean a = Just ((sum a) / (fromIntegral (length a) ) )