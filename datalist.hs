-- * Basic functions

-- (++) 
-- (++) :: [a] -> [a] -> [a]
-- concatena listas
mm a b = foldr (:) b a

mm2 [] l = l
mm2 l [] = l
mm2 (h:t) l2 = h : mm2 t l2

-- head
-- head :: [a] -> a
-- cabeça de lista
myhead [] = error "empty list"
myhead (h:t) = h

-- last 
-- last :: [a] -> a
-- ultimo elemento
mylast l = foldl1 (\a x -> x) l 
mylast2 l = foldl1 ( curry snd ) l 

mylast3 [] = error "empty list"
mylast3 [x] = x
mylast3 (h:t) = mylast3 t

-- tail 
-- tail :: [a] -> [a]
-- cauda 
mytail [] = error "empty list"
mytail (h:t) = t 

-- init
-- init :: [a] -> [a]
-- todos menos o último
myinit [] = error "empty list"
myinit [x] = []
myinit (h:t) = h:myinit t

-- uncons
-- uncons :: [a] -> Maybe (a,[a])
-- decompoe uma lista em Just (cabeaça,cauda) ou Nothing
uncons [] = Nothing
uncons (h:t) = Just (h,t)

-- singleton 
-- singleton :: a -> [a]
-- faz lista singuar
singleton x = [x]

-- null 
-- null :: [a] -> Bool
-- ve se uma lista é nula  
mynull [] = True
mynull  _ = False

-- length 
-- length :: [a] -> Int 
-- tamanho de uma lista 
mylength l = foldr (\x a-> a+1) 0 l

mylength2 [] = 0
mylength2 (h:t) = 1 + mylength2 t


-- * List transformations

-- map 
-- map :: (a -> b) -> [a] -> [b]
-- aplica uma funçao a todos os elementos de uma lista
mymap f l = foldr ( (:) . f ) [] l

mymap2 f [] = []
mymap2 f (h:t) = f h : mymap2 f t

mymap3 f l = [ f x | x <- l ]
   
-- reverse 
-- reverse :: [a] -> [a]
-- inverte uma lista
myreverse l = foldl ( flip (:) ) [] l

myreverse2 l = revaux l []

revaux :: [a] -> [a] -> [a]
revaux [] l = l
revaux (h:t) l = revaux t (h:l)

myreverse3 [] = []
myreverse3 (h:t) = myreverse3 t ++ [h]

-- intersperse 
-- intersperse :: a -> [a] -> [a]
-- coloca um elemento no meio de todos de uma lista 
-- FIXME
-- intersperse a l = foldr () [] l
intersperse2 a []    = []
intersperse2 a [x]   = [x]
intersperse2 a (h:t) = h:a:intersperse2 a t

intersperse3 a l = tail $ concat [ a:[x] | x <- l ]
   
-- intercalate 
-- intercalate :: [a] -> [[a]] -> [a] 
-- une uma lista de listas com outra lista
intercalate a []    = []
intercalate a [l] = l 
intercalate a (h:t) = h ++ a ++ intercalate a t

intercalate3 a (h:t) = concat $ h : [ a ++ x | x <- t ]

-- traspose 
-- transpose :: [[a]] -> [[a]] 
-- matriz transposta 
--transpose l = [ map (!! k) l | k <- [0..(length $ head l) - 1] ]
transpose l = [[ linha !! k | linha <- l, k < length linha ] | k <- [0.. t-1]]
    where t = maximum $ map length l

-- subsequences
-- subsequences :: [a] -> [[a]]
-- subsequencias 
subsequences [] = [[]]
subsequences (h:t)  = [ h:x | x <- l ]  ++ l
    where l = subsequences t

-- permutations 
-- permutations :: [a] -> [[a]]
-- permuta uma lista
permutations [] = [[]]
permutations l = [ (l!!k):x 
                  | k <- [0..length l-1]
                  , x <- (permutations $ delete2 k l) ]

permutations2 [] = [[]]
----permutations l = [ map  (: l!!k ) (permutations (delete (l!!k) l)) | k <- [0..length l -1] ]  
permutations2 l = concat [ map ( (:)(l!!k) ) (permutations $ delete2 k l ) | k <- [0..length l-1] ]  

 -- * Reducing lists (folds)

-- foldl 
-- foldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl f a [] = a
myfoldl f a (h:t) = myfoldl f (f a h) t 

-- foldl'
-- FIXME

-- foldl1
-- foldl1 :: (a -> a -> a ) -> [a] -> a
-- nao leva acumulador (é o primeiro elemntos)
myfoldl1 f [x] = x 
myfoldl1 f (h:t)  = foldl f h t

-- foldl1'

-- foldr
-- foldr :: (a -> b -> b) -> b ->[a] -> b
myfoldr f a [] = a
myfoldr f a (h:t) = f h (myfoldr f a t)
-- foldr'

-- foldr1
-- foldr1 :: (a -> a -> a) -> [a] -> a
-- foldr1 é igual, nao muda a ordem
myfoldr1 f [x] = x
myfoldr1 f (h:t) = f h (myfoldr1 f t)

-- foldr1'


-- * Special folds

-- concat 
-- concat :: [[a]] -> [a]
-- concatena listas de listas
myconcat l = foldl1 (++) l 

myconcat2 l  = [ x | f <- l, x <- f]
-- myconcat2 l = [ x | x <- f ,f <- l] nao funciona
--ordem importa la dentro

myconcat3 [] = [] 
myconcat3 (h:t) = h ++ myconcat3 t

-- concatMap 
-- concatMap :: (a -> [b]) -> [a] -> [b]
-- equivalente a concat $ map
myconcatMap f l = foldr ( (++) . f ) [] l

myconcatMap3 f l = foldr ( foldr (:) . f ) [] l

myconcatMap2 f [] = []
myconcatMap2 f (h:t) = f h ++ myconcatMap2 f t

-- and
-- and :: Bool -> Bool -> Bool
myand True True = True
myand _    _    = False

-- or
-- or :: Bool -> Bool -> Bool
myor False False = False
myor _    _     = True

-- any
-- any :: (a -> Bool) -> [a] -> Bool
-- verifica se existe algum elemento se cumpre a condiçao numa lista 
myany f l = foldr ( (||) . f ) False l

myany2 f [] = False
myany2 f (h:t) = f h || myany2 f t    

-- all 
-- all :: (a -> Bool) -> [a] -> Bool
-- todos os elementos cumprem a condiçao
myall f l = foldr ( (&&) . f ) True l

myall2 f [] = True
myall2 f (h:t) = f h && myall2 f t

-- sum 
-- sum :: Num a => [a] -> a
-- soma uma lista
mysum l = foldr1 (+) l

mysum3 l = foldr (+) 0 l

mysum2 [] = 0
mysum2 (h:t) = h + mysum2 t

-- product 
-- product :: Num a => [a] -> a
-- produto de uma lista
myproduct l = foldr1 (*) l

myproduct2 l = foldr (*) 1 l

myproduct3 [] = 1
myproduct3 (h:t) = h * myproduct3 t

-- maximum  
-- maximum :: Ord a => [a] -> a
-- máximo de lista
mymaximum l = foldr1 max l

mymaximum2 (h:t) = foldr max h t

mymaximum3 [] = error "empty list"
mymaximum3 [x] = x
mymaximum3 (h:t) = if (h > k) then h else k
    where k = mymaximum3 t

-- minimum 
-- minimum :: Ord a => [a] -> a
-- minimo de lista 
myminimum l = foldr1 min l

myminimum2 (h:t) = foldr min h t

myminimum3 [] = error "empty list"
myminimum3 [x] = x
myminimum3 (h:t) = if (h < k) then h else k
    where k = myminimum3 t

-- * Building lists
-- ** Scans

-- scanl
-- scanl :: (b -> a -> b) -> b -> [a] -> [b] 
-- é um fold mas guarda o acuulador na várias etapas
myscanl f a [] = [a]
myscanl f a (h:t) = a : myscanl f (f a h) t 

-- scanl'
-- scanl1

-- scanr
-- scanr :: (a -> b -> b) -> b -> [a] -> [b]
myscanr f a [] = [a]
myscanr f a (h:t) = f h h2 : (h2:t2)  
    where (h2:t2) = myscanr f a t

-- scanr1
-- scanr1'

-- FIXME

-- ** Accumulating maps
-- mapAccumL
-- mapAccumR

-- ** Infinite lists

-- iterate


-- iterate'

-- repeat
-- repeat :: a -> [a]
-- lista infinita de xs
myrepeat x = k
    where k = x:k

myrepeat2 x = x:myrepeat2 x

-- replicate
-- replicate :: Int -> a -> [a]
-- replica um x infinitamente
myreplicate n x = [x | k <- [1..n] ]

myreplicate2 n x = take n $ repeat x

myreplicate3 0  _ = []
myreplicate3 n  x = x: myreplicate3 (n-1) x

-- cycle   
-- cycle :: [a] -> [a]
-- repete uma lista infinitamente
mycycle l = k
    where k = l ++ k

mycycle2 l = l ++ mycycle2 l

-- ** Unfolding
-- unfoldr





---------------------------------------------------------------------------

-- delete apaga a primeira ocorrencia 
delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (h:t)
    | x == h = t
    | otherwise = h: delete x t
------------------------------
-- apaga um indice
delete2 :: Int -> [a] -> [a]
delete2 _ [] = []
delete2 0 (h:t) = t
delete2 n (h:t) = h: delete2 (n-1) t 
-------------------

-- (!!) seleciona um indice numa lista
pp :: Eq a => [a] -> Int -> a
pp [] x = error "empty list"
pp (h:t) x | x == 0 = h
           | otherwise = pp t (x-1) 


