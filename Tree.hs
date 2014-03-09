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

  -- this one should be efficient, O(n)
  postorder :: Tree a -> [a]
  postorder = F.foldl (flip (:)) []

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
