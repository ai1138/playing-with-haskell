import qualified Data.Map as Map
import Data.Map (Map)
frequency :: Ord a => [a] -> Map a Int


data Tree a = Node a (Tree a) (Tree a)
    | Leaf
    deriving Show   


frequency [] = Map.empty
frequency (h:t) =
    case Map.lookup h (frequency t) of 
         Just n -> Map.adjust (+1) h (frequency t) 
         Nothing -> Map.insert h 1 (frequency t)


instance Functor(Tree) where
    fmap f Leaf = Leaf
    fmap f (Node value left right) = Node (f value) (fmap f left) (fmap f right)



instance Eq a => Eq (Tree a) where
    Leaf == Leaf = True 
    (Node val left right) == Leaf = False
    Leaf == (Node val left right) = False
    (Node val1 left1 right1) == (Node val2 left2 right2) = ((val1 == val2) && ((left1) == (left2)) && ((right1) == (right2)))


bstInsert :: Ord a => Tree (a, Int) -> a -> Tree (a, Int)


bstInsert Leaf a = Node (a,1) Leaf Leaf
bstInsert (Node (b,n) left right) a 
        | b < a = Node (b,n) left (bstInsert right a)
        | b == a = Node (b,n+1) left right
        | b > a = Node (b,n) (bstInsert left a) right

bstLookup :: Ord a => Tree (a, Int)-> a -> Int

bstLookup Leaf a = 0 
bstLookup (Node (b,n) left right) a 
        | b < a = bstLookup right a
        | b == a = n
        | b > a = bstLookup left a

bstRemove :: Ord a => Tree (a, Int) -> a -> Maybe (Tree (a, Int))

bstRemove (Node (b,n) left right) a = 
    |bstLookup (Node (b,n) left right) a > 1 = (Node (b,n-1) left right)
    |
    |
