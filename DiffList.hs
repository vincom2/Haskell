module DiffList
where
  import Data.Monoid

  newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

  toDiffList :: [a] -> DiffList a
  toDiffList xs = DiffList (xs++)

  fromDiffList :: DiffList a -> [a]
  fromDiffList (DiffList f) = f []

  infixr `cons`
  cons :: a -> DiffList a -> DiffList a
  x `cons` xs = DiffList ((x:) . getDiffList xs)

  instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f $ g xs)

  -- too damn hard >.>
  instance (Show a) => Show (DiffList a) where
    show = show . fromDiffList
