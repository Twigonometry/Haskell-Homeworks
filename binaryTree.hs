module BinaryTree where
    data BTree a = EmptyTree | Node (BTree a) a (BTree a) deriving (Ord, Eq, Show)

    {- need to create instances for ord, show etc
    how to show a BTree?
    need to instance Read?

    instance Show BTree where
        show (BTree a) = show a
    
    instance Show EmptyTree where
        show (EmptyTree) = ""

    instance Show Node where
        show (Node (BTree a) b (BTree c)) = show BTree a ++ " " ++ b ++ " " ++ show BTree c
    -}
    

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