mm a b           = foldr (:) b a

mylast l         = foldl1 (\a x -> x) l 
mylast2 l        = foldl1 ( curry snd ) l 
mylast3 l        = foldr1 ( curry snd ) l 

mylength l       = foldr (\x a-> a+1) 0 l
mylength3 l      = foldl ( curry fst . (+1) ) 0 l

mymap f l        = foldr ( (:) . f ) [] l

myreverse l      = foldl ( flip (:) ) [] l

myconcat l       = foldl1 (++) l 
myconcat4 l      = foldr  ( foldr (:) ) [] l
myconcat5 l      = foldl  ( foldr (:) ) [] l

myconcatMap f l  = foldr ( (++) . f ) [] l
myconcatMap3 f l = foldr ( foldr (:) . f ) [] l

myany f l        = foldr ( (||) . f ) False l
myall f l        = foldr ( (&&) . f ) True l

mysum l          = foldr1 (+) l
mysum3 l         = foldr (+) 0 l

myproduct l      = foldr1 (*) l
myproduct2 l     = foldr (*) 1 l

mymaximum l      = foldr1 max l
mymaximum2 (h:t) = foldr max h t

myminimum l      = foldr1 min l
myminimum2 (h:t) = foldr min h t
