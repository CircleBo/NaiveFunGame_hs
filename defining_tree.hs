data BinaryTree a = BinaryNode{
	a,
	lefttree :: BinaryTree a,
	righttree :: BinaryTree a
} 
	| NullTree
	deriving (Show)
