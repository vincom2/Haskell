import Control.Monad.Writer

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

test1 = Node 1 Empty Empty --AVL
test2 = Node 1 (Node 2 (Node 3 Empty Empty) Empty) Empty --not AVL

checkAVL' :: Tree a -> Writer [String] (Bool, Int)
checkAVL' Empty = do
  tell ["At a leaf"]
  return (True, 0)
checkAVL' (Node _ left right) = do
  (lb, lh) <- checkAVL' left
  (rb, rh) <- checkAVL' right
  tell ["Left: (" ++ show lb ++ "," ++ show lh ++ ")   " ++ "Right: (" ++ show rb ++ "," ++ show rh ++ ")" ++ "lh-rh=" ++ show (lh-rh) ++ "   rh-lh=" ++ show (rh-lh)]
  return (lb && rb && abs(lh-rh) <= 1, (max lh rh) + 1)
  --where (lb, lh) = checkAVL' left
  --      (rb, rh) = checkAVL' right
