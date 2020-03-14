{-
 def mystery(lst):
    if len(lst) < 2:
        return lst
    return [lst[1]] + [lst[0]] + mystery(lst[2:])   
This is not a tail recursion call. It is due to the fact the call relies on the end call of the recursion.

-}
data Stream a = Chunk a (Stream a)
data Tree a = Node a (Tree a) (Tree a)
            | Leaf
            deriving Show 


streamToList :: Int -> Stream a -> [a]
streamToList 1 (Chunk val next) = [val]
streamToList n (Chunk val next) = [val] ++ streamToList (n-1) next

streamRepeat :: a -> Stream a
streamRepeat a = Chunk a (streamRepeat a)

instance Functor(Stream) where
    fmap f (Chunk val next) = Chunk (f val)  (fmap f next)

streamFromSeed :: (a -> a)-> a -> Stream a

streamFromSeed f a = Chunk a (streamFromSeed f (f a))

bigTree :: Tree Integer 

bigTree (Node val left right) = Node val (--streamFromSeed (+1) val) (streamFromSeed (+1) val)

{-
treeStream :: (a -> a)-> Tree a -> Tree a

treeStream f (Node val left right) = Node (f val) (treeStream f left) (treeStream f right)
-}