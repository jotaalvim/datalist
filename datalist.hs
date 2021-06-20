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




-- permutations permuta uma lista
permutations :: [a] -> [[a]]
permutations [] = []
--permutations l = [ map  (: l!!k ) (permutations (delete (l!!k) l)) | k <- [0..length l -1] ]  

permutations (h:t) = [ map (:h) (permutations t) | k <- [0..length t] ]  
-------------------------------------------------------------------------
-------------------------------------------------------------------------
-------------------------------------------------------------------------




















-- subsequences lista de subsequencias


--sub l = []: continuousSubSeqs l
--continuousSubSeqs = filter (not . null) . concatMap inits . tails
--continuousSubSeqs ls = [t | i <- inits ls, t <- tails i, not $ null t]
--
------------------------------------------
--subsequences            :: [a] -> [[a]]
--subsequences xs         =  [] : nonEmptySubsequences xs
--
--nonEmptySubsequences         :: [a] -> [[a]]
--nonEmptySubsequences []      =  []
--nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
--  where f ys r = ys : (x : ys) : r


-- delete apaga a primeira ocorrencia 
delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (h:t)
    | x == h = t
    | otherwise = h: delete x t


-- (!!) seleciona um indice numa lista
pp :: Eq a => [a] -> Int -> a
pp [] x = error "empty list"
pp (h:t) x | x == 0 = h
           | otherwise = pp t (x-1) 


