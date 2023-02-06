-- * Basic functions

-- (++) 
-- (++) :: [a] -> [a] -> [a]
-- concatenate 2 list
mm a b = foldr (:) b a

mm2 [] l = l 
mm2 l [] = l 
mm2 (h:t) l2 = h : mm2 t l2

-- head
-- head :: [a] -> a
myhead [] = error "empty list"
myhead (h:t) = h

-- last 
-- last :: [a] -> a
mylast = foldl1 (\a x -> x) 
mylast2 = foldl1 ( curry snd ) 
mylast3 = foldr1 ( curry snd )

mylast4 [] = error "empty list"
mylast4 [x] = x
mylast4 (h:t) = mylast4 t

-- tail 
-- tail :: [a] -> [a]
mytail [] = error "empty list"
mytail (h:t) = t 

-- init
-- init :: [a] -> [a]
-- all elems but not the last
myinit [] = error "empty list"
myinit [x] = []
myinit (h:t) = h:myinit t

-- uncons
-- uncons :: [a] -> Maybe (a,[a])
-- decomposes a lista in Just (head,tail) or Nothing
-- uncons [1,2,3,4] = Just (1,[2,3,4])
uncons [] = Nothing
uncons (h:t) = Just (h,t)

-- singleton 
-- singleton :: a -> [a]
-- singular list
singleton x = [x]

-- null 
-- null :: [a] -> Bool
mynull [] = True
mynull  _ = False

-- length 
-- length :: [a] -> Int 
mylength = foldr (\x a-> a+1) 0
mylength3 = foldl ( curry fst . (+1) ) 0

mylength2 [] = 0
mylength2 (h:t) = 1 + mylength2 t


-- * List transformations

-- map 
-- map :: (a -> b) -> [a] -> [b]
-- aplies a function to every element in a list
mymap f = foldr ( (:) . f ) []

mymap2 f [] = []
mymap2 f (h:t) = f h : mymap2 f t

mymap3 f l = [ f x | x <- l ]
   
-- reverse 
-- reverse :: [a] -> [a]
myreverse = foldl ( flip (:) ) []
-- fast reverse ↓
myreverse2 l = revaux l []

revaux :: [a] -> [a] -> [a]
revaux [] l = l
revaux (h:t) l = revaux t (h:l)

myreverse3 [] = []
myreverse3 (h:t) = myreverse3 t ++ [h]

-- intersperse 
-- intersperse :: a -> [a] -> [a]
-- puts an x inbetween all elements in a list 
myintersperse a []    = []
myintersperse a [x]   = [x]
myintersperse a (h:t) = h:a: myintersperse2 a t

myintersperse2 a l = tail $ concat [ a:[x] | x <- l ]
   
-- intercalate 
-- intercalate :: [a] -> [[a]] -> [a] 
-- concats all elements in a list with another list in between them
myintercalate a []    = []
myintercalate a [l] = l 
myintercalate a (h:t) = h ++ a ++ myintercalate a t

myintercalate3 a (h:t) = concat $ h : [ a ++ x | x <- t ]

-- traspose 
-- transpose :: [[a]] -> [[a]] 
-- transposed matrix
--transpose l = [ map (!! k) l | k <- [0..(length $ head l) - 1] ]
mytranspose l = [[ linha !! k | linha <- l, k < length linha ] | k <- [0.. t-1]]
    where t = maximum $ map length l

-- subsequences
-- subsequences :: [a] -> [[a]]
-- subsequences of a list 
mysubsequences [] = [[]]
mysubsequences (h:t)  = [ h:x | x <- l ]  ++ l
    where l = mysubsequences t

-- permutations 
-- permutations :: [a] -> [[a]]
-- permutations of a list
mypermutations [] = [[]]
mypermutations l = [ (l!!k):x 
                  | k <- [0..length l-1]
                  , x <- (mypermutations $ delete2 k l) ]

mypermutations2 [] = [[]]
----permutations l = [ map  (: l!!k ) (permutations (delete (l!!k) l)) | k <- [0..length l -1] ]  
mypermutations2 l = concat [ map ( (:) (l!!k) ) (mypermutations2 $ delete2 k l ) | k <- [0..length l-1] ]  

 -- * Reducing lists (folds)

-- foldl 
-- foldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl f a [] = a
myfoldl f a (h:t) = myfoldl f (f a h) t 


-- foldl1
-- foldl1 :: (a -> a -> a ) -> [a] -> a
-- uses the first element as the accumutalor 
myfoldl1 f [x] = x 
myfoldl1 f (h:t)  = foldl f h t


-- foldr
-- foldr :: (a -> b -> b) -> b ->[a] -> b
myfoldr f a [] = a
myfoldr f a (h:t) = f h (myfoldr f a t)

-- foldr1
-- foldr1 :: (a -> a -> a) -> [a] -> a
-- it dosen't change order 
myfoldr1 f [x] = x
myfoldr1 f (h:t) = f h (myfoldr1 f t)

-- * Special folds

-- concat 
-- concat :: [[a]] -> [a]
-- concatenate all ellements in a list
myconcat = foldl1 (++)

myconcat2 l  = [ x | f <- l, x <- f ]
-- myconcat2 l = [ x | x <- f ,f <- l] dosen't work, order matters

myconcat3 [] = [] 
myconcat3 (h:t) = h ++ myconcat3 t

myconcat4 = foldr  ( foldr (:) ) [] 
myconcat5 = foldl  ( foldr (:) ) []

-- concatMap 
-- concatMap :: (a -> [b]) -> [a] -> [b]
-- concatenates and applies a function to all elements
myconcatMap f = foldr ( (++) . f ) []

myconcatMap3 f = foldr ( foldr (:) . f ) []

myconcatMap2 f [] = []
myconcatMap2 f (h:t) = f h ++ myconcatMap2 f t

-- and
-- and :: Bool -> Bool -> Bool
myand True True = True
myand _    _    = False

-- or
-- or :: Bool -> Bool -> Bool
myor False False = False
myor _     _     = True

-- any
-- any :: (a -> Bool) -> [a] -> Bool
myany f = foldr ( (||) . f ) False

myany2 f [] = False
myany2 f (h:t) = f h || myany2 f t    

-- all 
-- all :: (a -> Bool) -> [a] -> Bool
myall f l = foldr ( (&&) . f ) True l

myall2 f [] = True
myall2 f (h:t) = f h && myall2 f t

-- sum 
-- sum :: Num a => [a] -> a
mysum  = foldr1 (+) 

mysum3  = foldr (+) 0 

mysum2 [] = 0
mysum2 (h:t) = h + mysum2 t

-- product 
-- product :: Num a => [a] -> a
myproduct = foldr1 (*) 

myproduct2 = foldr (*) 1

myproduct3 [] = 1
myproduct3 (h:t) = h * myproduct3 t

-- maximum  
-- maximum :: Ord a => [a] -> a
-- biggest element of the list
mymaximum = foldr1 max

mymaximum2 (h:t) = foldr max h t

mymaximum3 [] = error "empty list"
mymaximum3 [x] = x
mymaximum3 (h:t) = if (h > k) then h else k
    where k = mymaximum3 t

-- minimum 
-- minimum :: Ord a => [a] -> a
-- lowest element in a list
myminimum = foldr1 min

myminimum2 (h:t) = foldr min h t

myminimum3 [] = error "empty list"
myminimum3 [x] = x
myminimum3 (h:t) = if (h < k) then h else k
    where k = myminimum3 t

-- * Building lists
-- ** Scans

-- scanl
-- scanl :: (b -> a -> b) -> b -> [a] -> [b] 
-- it's a fold but makes a list with all the accumulators 
myscanl f a [] = [a]
myscanl f a (h:t) = a : myscanl f (f a h) t 

-- scanl1
-- scanl1 :: (a -> a -> a) -> [a] -> [a]
myscanl1 f [] = []
myscanl1 f (h:t) = myscanl f h t

-- scanr
-- scanr :: (a -> b -> b) -> b -> [a] -> [b]
myscanr f a [] = [a]
myscanr f a (h:t) = f h h2 : (h2:t2)  
    where (h2:t2) = myscanr f a t

-- scanr1
-- scanr :: (a -> b -> b) -> [a] -> [b]
myscanr1 f [] = []
myscanr1 f (h:t) = myscanr f h t

-- ** Accumulating maps

-- mapAccumL
-- mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)

-- mapAccumR

-- ** Infinite lists

-- iterate
-- iterate :: (a -> a) -> a -> [a]  
myiterate f a = a : myiterate f (f a)

-- repeat
-- repeat :: a -> [a]
-- constroi uma lista com xs indefenidamente
myrepeat x = k
    where k = x:k

myrepeat2 x = x:myrepeat2 x

-- replicate
-- replicate :: Int -> a -> [a]
myreplicate n x = [x | k <- [1..n] ]

myreplicate2 n x = take n $ repeat x
myreplicate20 n = (take n) . repeat
myreplicate21 =  (. repeat ) . take

myreplicate3 0  _ = []
myreplicate3 n  x = x: myreplicate3 (n-1) x

-- cycle   
-- cycle :: [a] -> [a]
mycycle l = k
    where k = l ++ k

mycycle2 l = l ++ mycycle2 l

-- ** Unfolding
-- unfoldr
-- anamorfismo

--  * Sublists
--  ** Extracting sublists

-- take
-- take :: Int -> [a] -> [a]
mytake 0 _ = []
mytake n (h:t) = h: mytake (n-1) t

-- drop
-- drop :: Int -> [a] -> [a]
mydrop 0 l  = l
mydrop x [] = []
mydrop n (h:t) = mydrop (n-1) t

-- splitAt
-- splitAt :: Int -> [a] -> ([a],[a])
mysplitAt 0 l = ([],l) 
mysplitAt x [] = ([],[]) 
mysplitAt x (h:t) = (h:p,q)
    where (p,q) = mysplitAt x t

-- takeWhile
-- takeWhile :: (a -> Bool) -> [a] -> [a] 
mytakeWhile f [] = []
mytakeWhile f (h:t)
    | f h = h: mytakeWhile f t
    | otherwise = [] 

-- dropWhile
-- dropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile f [] = []
mydropWhile f (h:t)
    | f h = mydropWhile f t
    | otherwise = h:t

-- dropWhileEnd
-- FIXME

-- span
-- span :: (a -> Bool) -> [a] -> ([a],[a]) 
-- divide em dois por um take while?
myspan f [] = ([],[])
myspan f (h:t)
    | f h = (h:p,q)
    | otherwise = (p,h:t)
    where (p,q) = myspan f t

-- break
-- QUAL È A DIFERENÇA ENTRE O BREA E O SPAN
--
--
--
-- stripPrefix
--
-- group
-- group :: Eq a => [a] -> [[a]]
-- agrupa os elementos iguais
mygroup [] = []
mygroup [x] = [[x]]
mygroup (h:t)
    | h == h2 = (h:h2:t2):tf
    | otherwise = [h]:(h2:t2):tf
    where ((h2:t2):tf) = mygroup t

-- inits
-- inits :: [a] -> [[a]]
-- inits "abcd" = ["","a","ab","abc","abcd"]
myinits [] = [[]]
myinits l =  myinits (init l) ++ [l]

myinits2 [] = [[]]
myinits2 l = l: (h2:t2)
    where (h2:t2) = myinits2 ( init l ) 

-- tails
-- tails :: [a] -> [[a]]
-- mytails "abcd" = ["abcd","bcd","cd","d",""]
mytails [] = [[]]
mytails l = l : mytails (tail l)

--  ** Predicates
-- isPrefixOf
-- isPrefixOf :: Eq a => [a] -> [a] -> Bool
myisPrefixOf [] _ = True
myisPrefixOf (h:t) (h2:t2) = h == h2 && myisPrefixOf t t2

-- isSuffixOf
-- isSuffixOf :: Eq a => [a] -> [a] -> Bool
-- FIXME
--myisSuffixOf [] _ = True
--myisSuffixOf l l2 = f == f2 && myisSuffixOf FIXEM  
--    where f  = last l
--          f2 = last l2 
-- isInfixOf
-- isSubsequenceOf

--  * Searching lists

--  ** Searching by equality

-- elem
-- elem :: Eq a => [a] -> Bool
myelem _ [] = False
myelem x (h:t) 
    | x == h = True
    | otherwise = myelem x t

--myelem x l = foldl ( (== x) . (||) ) False l
--FIXME

-- notElem
-- notElem :: (Foldable t, Eq a) => a -> t a -> Bool

-- lookup
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
mylookup a [] = Nothing
mylookup a ((c,b):cs)
    | a == c = Just b
    | otherwise = mylookup a cs

--  ** Searching with a predicate
-- find
-- find :: Foldable t => (a -> Bool) -> t a -> Maybe a

-- filter
-- filter :: (a -> Bool) -> [a] -> [a]
myfilter p l = [ x | x <- l, p x]

-- partition
-- partition :: (a -> Bool) -> [a] -> ([a], [a])


--  * Indexing lists
--  | These functions treat a list @xs@ as a indexed collection,
--  with indices ranging from 0 to @'length' xs - 1@.

-- (!!)
-- !! :: Eq a => [a] -> Int -> a
-- seleciona um indice numa lista
pp [] x = error "empty list"
pp (h:t) x | x == 0 = h
           | otherwise = pp t (x-1) 

-- elemIndex
-- elemIndices
--
-- findIndex
-- findIndices
--
--  * Zipping and unzipping lists

-- zip
-- zip :: [a] -> [b] -> [(a,b)]
myzip [] _ = [] 
myzip _ [] = [] 
myzip (h:t) (h2:t2) = (h,h2):myzip t t2

myzip2 l1 l2 = [(x,y)  |  x <- l1, y <- l2 ]

-- zip3
myzip3 l1 l2 l3 = [(x,y,z)  |  x <- l1, y <- l2, z <- l3 ]

--zip 4
myzip4 l1 l2 l3 l4 = [(x,y,z,a)  |  x <- l1, y <- l2, z <- l3, a <- l4]

--zip 5
myzip5 l1 l2 l3 l4 l5 = [(x,y,z,a,b)  |  x <- l1, y <- l2, z <- l3, a <- l4, b <- l5]

--zip 6
myzip6 l1 l2 l3 l4 l5 l6 = [(x,y,z,a,b,c)  |  x <- l1, y <- l2, z <- l3, a <- l4, b <- l5, c <- l6]

--zip 7
myzip7 l1 l2 l3 l4 l5 l6 l7 = [(x,y,z,a,b,c,d)  |  x <- l1, y <- l2, z <- l3, a <- l4, b <- l5, c <- l6, d <- l7]

-- zipWith
-- zipWith3
-- zipWith4, zipWith5, zipWith6, zipWith7
--
-- unzip
-- unzip3
-- unzip4, unzip5, unzip6, unzip7
--
--  * Special lists
--
--  ** Functions on strings

-- lines
-- lines :: String -> [String]
mylines "" = []
mylines (a:t) = charin a  $ mylines t

mylines2 = foldr charin []

charin :: Char -> [String] -> [String]  
charin a [] = ["a"]
charin '\n' l = "":l
charin a (x:y) = (a:x): y

-- words
mywords :: String -> [String]
mywords = foldr breakW []

breakW  :: Char -> [String] -> [String]
breakW  a []   = ["a"]
breakW ' ' l   = "":l
breakW c (a:b) = (c:a):b

-- unlines
-- unlines :: [String] -> String
-- unlines . lines /= id

myunlines = (++"\n") . myintercalate "\n"

myunlines1 l = myintercalate "\n" l ++ "\n"

myunlines2 [] = ""
myunlines2 (a:as) = a++ "\n" ++ myunlines2 as

-- unwords
myunwords = myintersperse ' '

--  ** \"Set\" operations

-- nub
-- nub :: Eq a => [a] -> [a]
-- retira elemtos repetidos
mynub [] = []
mynub (h:t) 
    | elem h t = mynub t
    | otherwise = h:mynub t

-- delete
-- delete :: Eq a => a -> [a] -> [a]
-- delete apaga a primeira ocorrencia 
delete x [] = []
delete x (h:t)
    | x == h = t
    | otherwise = h: delete x t

-- (\\)
--
-- union
-- intersect
--
--  ** Ordered lists
-- sort
-- sortOn
-- insert
--
--  * Generalized functions
--
--  ** The \"@By@\" operations
--  | By convention, overloaded functions have a non-overloaded
--  counterpart whose name is suffixed with \`@By@\'.
-- 
--  It is often convenient to use these functions together with
--  'Data.Function.on', for instance @'sortBy' ('Prelude.compare'
--  ``Data.Function.on`` 'Prelude.fst')@.
--
--  *** User-supplied equality (replacing an @Eq@ context)
--  | The predicate is assumed to define an equivalence.
-- nubBy
-- deleteBy
-- deleteFirstsBy
-- unionBy
-- intersectBy
-- groupBy
--
--  *** User-supplied comparison (replacing an @Ord@ context)
--  | The function is assumed to define a total ordering.
-- sortBy
-- insertBy
-- maximumBy
-- minimumBy
--
--
-- genericLength
-- genericTake
-- genericDrop
-- genericSplitAt
-- genericIndex
-- genericReplicate



---------------------------------------------------------------------------
-- apaga um indice
delete2 :: Int -> [a] -> [a]
delete2 _ [] = []
delete2 0 (h:t) = t
delete2 n (h:t) = h: delete2 (n-1) t 
-------------------



