module BinaryTree where
    data BTree a = EmptyTree | Node (BTree a) a (BTree a) deriving (Ord, Eq, Show)

    {- compares value of new node x to root
    if same, don't insert (no duplicates); if smaller, insert into left subtree lSub; if larger, insert into right subtree rSub -}
    treeInsert :: (Ord a) => a -> BTree a -> BTree a
    treeInsert x EmptyTree = Node EmptyTree x EmptyTree
    treeInsert x (Node lSub root rSub)
        | x == root = Node lSub x rSub
        | x < root = Node (treeInsert x lSub) root rSub
        | x > root = Node lSub root (treeInsert x rSub)