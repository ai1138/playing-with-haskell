data Tree a = Node a (Tree a) (Tree a)
            |Leaf
            deriving Show

----mapTree 
someTree = Node "t1"
             (Node "t2a"
                (Node "t3a" Leaf Leaf)
                (Node "t3b" Leaf Leaf))
             (Node "t2b"
                (Node "t3c" Leaf Leaf)
                (Node "t3d" Leaf Leaf))
mapTree :: (a -> b) -> Tree a -> Tree b

mapTree func (Node value left right) = Node func value (mapTree func left) (mapTree func right)
maptree func Leaf = Leaf 


--flipTree
{-flipTree :: Tree a -> Tree a

flipTree (Node val left right) = flipTree(Node val right) flipTree(Node val left)
flipTree leaf = leaf 
-}
--inorderTraversal
inorderTraversal :: Tree a -> [a]

inorderTraversal (Node val left right) = inorderTraversal left ++ [val] ++  inorderTraversal right
inorderTraversal leaf = []
-- getLevel 
getLevel :: Int -> Tree a -> [a]

getLevel 0 (Node val left right) = [val]

getLevel n (Node val left right) = [val] ++ getLevel (n-1) left ++ getLevel (n-1) right
--myLast
mylast :: [a] -> a

mylast [] = 0
mylast [x] = x
mylast(_:t) = mylast t


--graphs
--data Graph = GraphNode Int [Graph]



