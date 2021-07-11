-- * Basic functions

-- (++) concatena listas
mm a b = foldr (:) b a

mm2 :: [a] -> [a] -> [a]
mm2 [] l = l
mm2 l [] = l
mm2 (h:t) l2 = h : mm2 t l2

-- head cabeça de lista
myhead :: [a] -> a
myhead [] = error "empty list"
myhead (h:t) = h

-- last ultimo elemento
--mylast (h:t) = foldr (id) h t 
--FIXME

mylast2 :: [a] -> a
mylast2 [] = error "empty list"
mylast2 [x] = x
mylast2 (h:t) = mylast2 t

-- tail cauda 
mytail :: [a] -> [a]
mytail [] = error "empty list"
mytail (h:t) = t 

-- init
myinit :: [a] -> [a]
myinit [] = error "empty list"
myinit [x] = []
myinit (h:t) = h:myinit t

-- uncons , decompoe uma lista em Just (cabeaça,cauda) ou Nothing
uncons :: [a] -> Maybe (a,[a])
uncons [] = Nothing
uncons (h:t) = Just (h,t)

-- singleton , faz lista singuar
singleton :: a -> [a]
singleton x = [x]

-- null ve se uma lista é nula  
mynull :: [a] -> Bool
mynull [] = True
mynull  _ = False

-- length tamanho de uma lista 
mylength l = foldr (\x a-> a+1) 0 l

mylength2 [] = 0
mylength2 (h:t) = 1 + mylength2 t


-- * List transformations

-- map aplica uma funçao a todos os elementos de uma lista

mymap f l = foldr ( (:) . f ) [] l

mymap2 :: (a -> b) -> [a] -> [b]
mymap2 f [] = []
mymap2 f (h:t) = f h : mymap2 f t

mymap3 f l = [ f x | x <- l ]
   
-- reverse inverte uma lista
myreverse l = foldl ( flip (:) ) [] l

myreverse2 :: [a] -> [a]
myreverse2 l = revaux l []

revaux :: [a] -> [a] -> [a]
revaux [] l = l
revaux (h:t) l = revaux t (h:l)

myreverse3 [] = []
myreverse3 (h:t) = myreverse3 t ++ [h]

-- intersperse coloca um elemento no meio de todos de uma lista 

intersperse2 :: a -> [a] -> [a]
intersperse2 a []    = []
intersperse2 a [x]   = [x]
intersperse2 a (h:t) = h:a:intersperse2 a t

intersperse2 a l = tail $ concat [ a:[x] | x <- l ]
   
-- intercalate une uma lista de listas com outra lista
intercalate :: [a] -> [[a]] -> [a] 
intercalate a []    = []
intercalate a [l] = l 
intercalate a (h:t) = h ++ a ++ intercalate a t

intercalate3 a (h:t) = concat $ h : [ a ++ x | x <- t ]

-- traspose matriz transposta 
transpose :: [[a]] -> [[a]] --transpose l = [ map (!! k) l | k <- [0..(length $ head l) - 1] ]
transpose l = [[ linha !! k | linha <- l, k < length linha ] | k <- [0.. t-1]]
    where t = maximum $ map length l

--  subsequences
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (h:t)  = [ h:x | x <- l ]  ++ l
    where l = subsequences t

-- permutations permuta uma lista

permutations [] = [[]]
permutations l = [ (l!!k):x 
                  | k <- [0..length l-1]
                  , x <- (permutations $ delete2 k l) ]

permutations2 :: [a] -> [[a]]
permutations2 [] = [[]]
----permutations l = [ map  (: l!!k ) (permutations (delete (l!!k) l)) | k <- [0..length l -1] ]  
permutations2 l = concat [ map ( (:)(l!!k) ) (permutations $ delete2 k l ) | k <- [0..length l-1] ]  

 -- * Reducing lists (folds)

-- foldl 
myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl f a [] = a
myfoldl f a (h:t) = myfoldl f (f a h) t 

--   , foldl'
-- foldl1 nao leva acumulador (é o primeiro elemntos)
--   , foldl1'

-- foldr
myfoldr :: (a -> b -> b) -> b ->[a] -> b
myfoldr f a [] = a
myfoldr f a (h:t) = f h (myfoldr f a t)
-- FIXME
--   , foldr1


-- * Special folds

-- concat concatena listas de listas
myconcat :: [[a]] -> [a]
myconcat [] = [] 
myconcat (h:t) = h ++ myconcat t

-- concatMap equivalente a concat $ map
-- especie de fold com map? fold para subtituir o concat
myconcatMap :: (a -> [b]) -> [a] -> [b]
myconcatMap f [] = []
myconcatMap f (h:t) = f h ++ myconcatMap f t

-- and
myand :: Bool -> Bool -> Bool
myand True True = True
myand _    _    = False

-- or
myor :: Bool -> Bool -> Bool
myor False False = False
myor _    _     = True

-- any verifica se existe algum elemento se cumpre a condiçao numa lista 
myany c l = foldr (\x a -> c x || a) False l
--myany3 c l = foldr ( c . || ) False l
--FIXME
myany2 :: (a -> Bool) -> [a] -> Bool
myany2 c [] = False
myany2 c (h:t) = c h || myany2 c t    

-- all todos os elementos cumprem a condiçao
myall c l = foldr (\x a -> c x && a) True l

myall2 :: (a -> Bool) -> [a] -> Bool
myall2 c [] = True
myall2 c (h:t) = c h && myall2 c t

-- sum soma uma lista
mysum l = foldr (+) 0 l

mysum2 :: Num a => [a] -> a
mysum2 [] = 0
mysum2 (h:t) = h + mysum2 t

-- product produto de uma lista
myproduct l = foldr (*) 1 l

myproduct2 :: Num a => [a] -> a
myproduct2 [] = 1
myproduct2 (h:t) = h * myproduct2 t

-- maximum  máximo de lista
mymaximum (h:t) = foldr max h t

mymaximum2 :: Ord a => [a] -> a
mymaximum2 [] = error "empty list"
mymaximum2 [x] = x
mymaximum2 (h:t) = if (h > k) then h else k
    where k = mymaximum2 t

-- minimum minimo de lista 
myminimum (h:t) = foldr min h t

myminimum2 :: Ord a => [a] -> a
myminimum2 [] = error "empty list"
myminimum2 [x] = x
myminimum2 (h:t) = if (h < k) then h else k
    where k = myminimum2 t

-- * Building lists
-- ** Scans
-- scanl
myscanl :: (b -> a -> b) -> b -> [a] -> [b] 
myscanl f a [] = [a]
myscanl f a (h:t) = a : myscanl f (f a h) t 
-- scanl'
-- scanl1
-- scanr
myscanr :: (a -> b -> b) -> b -> [a] -> [b]
myscanr f a [] = [a]
myscanr f a (h:t) = f h h2 : (h2:t2)  
    where (h2:t2) = myscanr f a t
-- scanr1
-- FIXME






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


