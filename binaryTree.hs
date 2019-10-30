module BinaryTree where
    --type definition for binary trees
    data BTree a = EmptyTree | Node (BTree a) a (BTree a) deriving (Ord, Eq)

    {- need to create instances for ord, show etc
    add type constraint to type definition?
    add instances for Ord, Eq
    -}
    
    instance Show a => Show (BTree a) where
        show EmptyTree = ""
        show (Node a b c) = show a ++ " " ++ show b ++ " " ++ show c

    {- compares value of new node x to root
    if same, don't insert (no duplicates); if smaller, insert into left subtree lSub; if larger, insert into right subtree rSub -}
    treeInsert :: (Ord a) => a -> BTree a -> BTree a
    treeInsert x EmptyTree = Node EmptyTree x EmptyTree
    treeInsert x (Node lSub root rSub)
        | x == root = Node lSub x rSub
        | x < root = Node (treeInsert x lSub) root rSub
        | x > root = Node lSub root (treeInsert x rSub)

    --recurses over list of nodes to add
    treeFromList :: (Ord a) => [a] -> BTree a -> BTree a
    treeFromList [] tree = tree
    treeFromList (node:nodes) tree = treeFromList nodes (treeInsert node tree)