data Tree = Leaf | Node Int Tree Tree

walkInOrder :: Tree -> [Int]
walkInOrder Leaf = []
walkInOrder (Node x l r) = [x] ++ walkInOrder l ++ walkInOrder r

walkPostOrder :: Tree -> [Int]
walkPostOrder Leaf = []
walkPostOrder (Node x l r) = walkPostOrder l ++ walkPostOrder r ++ [x]

newNode :: Int -> [Tree] -> [Tree]
newNode n []  = [Node n Leaf Leaf]
newNode n [x] = [Node n    x Leaf]
newNode n (a : b : xs) = (Node n b a) : xs

restore :: [Tree] -> [Int] -> [Int] -> [Int] -> Tree
restore nodeStack [] [] [] = head nodeStack -- XXX: The only item!
restore nodeStack stL (x: xs) (y: ys) | x == y = restore ((Node y Leaf Leaf) : nodeStack) stL xs ys
restore nodeStack (l: ls) stX (y: ys) | l == y = restore (newNode y nodeStack) ls stX ys
restore nodeStack stL (x: xs) stY = restore nodeStack (x: stL) xs stY
  
main = 
  let
    tree = Node 1 (Node 2 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 5 Leaf Leaf)
    inOrder = walkInOrder tree
    postOrder = walkPostOrder tree
  in do
    print inOrder
    print postOrder
    print "----------------"
    let
      t2 = restore [] [] inOrder postOrder
    in do
      print $ walkInOrder $ t2
      print $ walkPostOrder $ t2
