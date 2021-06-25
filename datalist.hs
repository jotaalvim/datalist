-- * Basic functions

-- (++) concatena listas
mm :: [a] -> [a] -> [a]
mm [] l = l
mm l [] = l
mm (h:t) l2 = h : mm t l2

-- head cabeça de lista
myhead :: [a] -> a
myhead [] = error "empty list"
myhead (h:t) = h

-- last ultimo elemento
mylast :: [a] -> a
mylast [] = error "empty list"
mylast [x] = x
mylast (h:t) = mylast t

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
mylength [] = 0
mylength (h:t) = 1 + mylength t


-- * List transformations

-- map aplica uma funçao a todos os elementos de uma lista
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (h:t) = f h : mymap f t

mymap2 f l = [ f x | x <- l ]
   
-- reverse inverte uma lista
myreverse :: [a] -> [a]
myreverse l = revaux l []

revaux :: [a] -> [a] -> [a]
revaux [] l = l
revaux (h:t) l = revaux t (h:l)

myreverse2 [] = []
myreverse2 (h:t) = myreverse2 t ++ [h]

-- intersperse coloca um elemento no meio de todos de uma lista 
intersperse :: a -> [a] -> [a]
intersperse a []    = []
intersperse a [x]   = [x]
intersperse a (h:t) = h:a:intersperse a t

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
--   , foldl1
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
myany :: (a -> Bool) -> [a] -> Bool
myany c [] = False
myany c (h:t) = c h || myany c t    

-- all todos os elementos cumprem a condiçao
myall :: (a -> Bool) -> [a] -> Bool
myall c [] = True
myall c (h:t) = c h && myall c t

-- sum soma uma lista
mysum :: Num a => [a] -> a
mysum [] = 0
mysum (h:t) = h + mysum t

-- product produto de uma lista
myproduct :: Num a => [a] -> a
myproduct [] = 1
myproduct (h:t) = h * myproduct t

-- maximum  máximo de lista
mymaximum :: Ord a => [a] -> a
mymaximum [] = error "empty list"
mymaximum [x] = x
mymaximum (h:t) = if (h > k) then h else k
    where k = mymaximum t

-- minimum minimo de lista 
myminimum :: Ord a => [a] -> a
myminimum [] = error "empty list"
myminimum [x] = x
myminimum (h:t) = if (h < k) then h else k
    where k = myminimum t

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
myscanr f a (h:t) = f h (myscanr f a t)


-- scanr1







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


