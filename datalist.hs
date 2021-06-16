-- (++) concatena listas
mm :: [a] -> [a] -> [a]
mm [] l = l
mm l [] = l
mm (h:t) l2 = h : mm t l2

-- head primeiro elemento da lista
myhead :: [a] -> a
myhead [] = error "empty list"
myhead (h:t) = h

-- last último elemento da lista
mylast :: [a] -> a
mylast [] = error "empty list"
mylast [x] = x
mylast (h:t) = mylast t

-- tail cauda de uma lista
mytail :: [a] -> [a]
mytail [] = error "empty list"
mytail [x] = []
mytail (h:t) = t

-- init todos menos a ultimo
myinit :: [a] -> [a]
myinit [] = error "empty list"
myinit [x] = []
myinit (h:t) = h : myinit t

-- uncons , decompoe uma lista em Just (cabeaça,cauda) ou Nothing
uncons :: [a] -> Maybe (a,[a])
uncons [] = Nothing
uncons (h:t) = Just (h,t)

-- singleton , faz lista singuar
singleton :: a -> [a]
singleton x = [x]

-- null ve se uma lista é nula  
-- null :: Foldable t => t a -> Bool
mynull :: [a] -> Bool
mynull [] = True
mynull  _ = False

-- length tamanho de uma lista 
-- length :: Foldable t => t a -> Int
mylength [] = 0
mylength (h:t) = 1 + mylength t

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

intersperse2 a l = tail [ a:x | x <- l ]

-- intercalate é tipo a intersperse mas para lista
intercalate :: [a] -> [[a]] -> [a] 
intercalate a []   = []
intercalate a [[x]]  = [x]
intercalate a (h:t)= h ++ a ++ intercalate a t

intercalate2 a l =  tail [ a ++ x | x <- l ]
