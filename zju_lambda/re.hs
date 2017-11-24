data Regular a = RE a|Alt (Regular a) (Regular a)|Conc (Regular a) (Regular a)| Kleene (Regular a)
data Alphabet a = Alphabet [a]



