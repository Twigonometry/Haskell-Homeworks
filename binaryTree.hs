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
    --in current form, inserted in reverse order (sort of)
    --e.g [2,1] -> insert 2 into 1 (even though 2 should be root element)

    {- builds tree from list, inserting nodes in order they appear
    need to reverse list for items to be inserted in correct order -}
    treeFromList :: (Ord a) => [a] -> BTree a
    treeFromList [] = EmptyTree
    treeFromList nodes = treeInsert (head (reverse nodes)) (treeFromList (reverse nodes))

    complete :: (Ord a) => (BTree a) -> Bool
    complete EmptyTree = True
    complete (Node lSub x rSub) = complete lSub && complete rSub && 
                                  treeHeight lSub == treeHeight rSub

    {-
    treeFromList :: (Ord a) => [a] -> BTree a
    treeFromList [] = EmptyTree
    treeFromList (node:nodes) = treeInsert node (treeFromList nodes)
    -}

    --taking definition from slides "A binary tree is complete if the two sub-trees of every node are of equal size"
    {-
    complete :: (Ord a) => BTree a -> Bool
    complete EmptyTree = False
    complete (Node lSub x rSub) = treeSize lSub == treeSize rSub
    -}

    {-
    complete Node EmptyTree x EmptyTree = True
    complete Node lSub root rSub = complete
    -}

    --recursively totals number of nodes in a tree
    treeSize :: (Ord a) => BTree a -> Int
    treeSize EmptyTree = 0
    treeSize (Node lSub x rSub) = 1 + treeSize lSub + treeSize rSub

    treeHeight :: (Ord a) => BTree a -> Int
    treeHeight EmptyTree = 0
    treeHeight (Node lSub x rSub) = 1 + max (treeHeight lSub) (treeHeight rSub)

    --helper functions for testing functions
    getRoot :: BTree a -> a
    getRoot EmptyTree = error "No elements"
    getRoot (Node lSub x rSub) = x

    getLSub :: BTree a -> BTree a
    getLSub EmptyTree = error "No elements"
    getLSub (Node lSub x rSub) = lSub

    getRSub :: BTree a -> BTree a
    getRSub EmptyTree = error "No elements"
    getRSub (Node lSub x rSub) = rSub