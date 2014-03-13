module Tree (preorder, inorder, postorder, Tree(..), test)
where
  import qualified Data.Foldable as F
  import Data.Monoid
  import DiffList

  data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

  test = Node 5
              (Node 3
                  (Node 1 Empty Empty)
                  (Node 6 Empty Empty)
              )
              (Node 9
                  (Node 8 Empty Empty)
                  (Node 10 Empty Empty)
              )
  test2 = Node 7 Empty (Node 10 (Node 4 Empty Empty) (Node 3 Empty (Node 1 Empty (Node 2 Empty (Node 8 (Node 11 Empty Empty) Empty)))))

  test3 = Node 'F' (Node 'B' (Node 'A' Empty Empty) (Node 'D' (Node 'C' Empty Empty) (Node 'E' Empty Empty))) (Node 'G' Empty (Node 'I' (Node 'H' Empty Empty) Empty))

  instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

  instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x `mappend`
                             F.foldMap f r

  -- I believe the foldr version should be O(n)
  -- not actually sure about the other 2. it might be a good exercise to try and figure those out.
  inorder :: Tree a -> [a]
  inorder = F.foldr (:) []
  --inorder = F.toList
  --inorder = F.foldMap (\x -> [x])

  -- bad
  inorder_slow :: Tree a -> [a]
  inorder_slow Empty = []
  inorder_slow (Node x l r) = (inorder_slow l) ++ [x] ++ (inorder_slow r)

  -- tail recursive
  inorder' :: Tree a -> [a] -> [a]
  inorder' Empty acc = acc
  inorder' (Node x l r) acc = inorder' l (x:(inorder' r acc))

  -- this one should be efficient, O(n)
  -- holy fuck it's also WRONG
  -- apparently I totally misunderstood what "postorder" is
  -- mfw
  postorder_slow :: Tree a -> [a]
  --postorder = F.foldl (flip (:)) [] -- this will stay here so I remember my shame
  postorder_slow Empty = []
  postorder_slow (Node x l r) = (postorder_slow l) ++ (postorder_slow r) ++ [x]

  -- my attempt to write a tail-recursive version
  postorder' :: Tree a -> [a] -> [a]
  postorder' Empty acc = acc
  postorder' (Node x l r) acc =
    let acc' = postorder' r (x:acc)
    in postorder' l acc'

  -- and the nicer interface
  -- also I should totally swap the arguments of the tail-recursive version
  postorder = (flip postorder') []

  -- I don't think there's a clever way of doing this one with folds D:
  -- but it's of course quite possible that I'm just bad
  preorder_slow :: Tree a -> [a]
  preorder_slow Empty = []
  preorder_slow (Node x l r) = (x:preorder l) ++ preorder r

  -- tail recursive version
  preorder' :: Tree a -> [a] -> [a]
  preorder' Empty acc = acc
  preorder' (Node x l r) acc = x:(preorder' l (preorder' r acc))

  -- and the nicer interface
  preorder :: Tree a -> [a]
  preorder t = preorder' t []

  -- using difference lists
  preorder_dl :: Tree a -> DiffList a
  preorder_dl Empty = mempty
  preorder_dl (Node x l r) = (x `cons` preorder_dl l) `mappend` preorder_dl r
