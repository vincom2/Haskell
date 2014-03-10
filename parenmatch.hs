-- this is horrible

import Control.Applicative
import Control.Monad (join)

data Paren = Oparen | Cparen deriving (Eq,Show,Read)

parenMatch :: [Paren] -> Bool
parenMatch s = case foldl pplus (Just 0) s of
                 Just _ -> True
                 _ -> False

pplus :: (Integral n) => Maybe n -> Paren -> Maybe n
pplus n p = join $ plus <$> n <*> translate p
  -- the join clearly tells you that I haven't figured out
  -- why my values come out of the applicative doubly wrapped
  -- please explain this to me someone
  where translate Oparen = Just 1
        translate Cparen = Just (-1)
        a `plus` b
          | c >= 0 = Just c
          | otherwise = Nothing
              where c = a+b
