module Bfs where
data Tree a = Empty | Node {left::(Tree a), value:: a, right:: (Tree a)}

newtype BFSTree a = BFSTree(Tree a)

instance Foldable BFSTree where
	foldMap _ (BFSTree Empty) = mempty
	foldMap f (BFSTree t) = bfs' mempty [t] where
		bfs' acc [] = acc
		bfs' acc (t:ts) = let acc' = acc `mappend` (f $ value t) 
				in bfs' acc' $ ts ++ subtrees t
		subtrees t = [ sub | sub@(Node _ _ _) <- [left t, right t] ]

newtype DFSTreePost a = DFSTreePost(Tree a)
instance Foldable DFSTreePost where
	foldr f z (DFSTreePost Empty) = z
	foldr f z (DFSTreePost(Node l k r)) = f k (foldr f (foldr f z (DFSTreePost r)) (DFSTreePost l))

newtype DFSTreePre a = DFSTreePre(Tree a)
instance Foldable DFSTreePre where
	foldr f z (DFSTreePre Empty) = z
	foldr f z (DFSTreePre(Node l k r)) = foldr f (foldr f (f k z) (DFSTreePre r)) (DFSTreePre l)

newtype DFSTreeIn a = DFSTreeIn(Tree a)
instance Foldable DFSTreeIn where
	foldr f z (DFSTreeIn Empty) = z
	foldr f z (DFSTreeIn(Node l k r)) = foldr f (f k(foldr f z (DFSTreeIn r))) (DFSTreeIn l)



tree = Node (Node (Node Empty 2 Empty) 2 Empty) 1 (Node Empty 3 (Node Empty 4 Empty))
wrap a = [a]
