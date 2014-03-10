import Data.List
import Control.Monad

type KnightPos = (Int,Int)
type Moves = [KnightPos]

moveKnight :: Moves -> [Moves]
moveKnight xs@((c,r):_) = filter legalPos
              [(c+2,r-1):xs,(c+2,r+1):xs,(c-2,r-1):xs,(c-2,r+1):xs,
              (c+1,r-2):xs,(c+1,r+2):xs,(c-1,r-2):xs,(c-1,r+2):xs]
  where legalPos ((c,r):_) = c `elem` [1..8] && r `elem` [1..8]

inM :: Int -> KnightPos -> [Moves]
inM m start = return [start] >>= foldr (<=<) return (replicate m moveKnight)

-- finds all reachable squares in n moves, 1 <= n <= m
inAtMost :: Int -> KnightPos -> [(Int,[Moves])]
inAtMost m start = map (\(n,f) -> (n,return [start] >>= foldr (<=<) return f)) allMoves
  where allMoves = map (\n -> (n, replicate n moveKnight)) [1..m]

canReachIn :: Int -> KnightPos -> KnightPos -> Moves
canReachIn m start end = case find matchEnd mAway of
                           Just moves -> reverse moves
                           Nothing -> []
  where matchEnd e = head e == end
        mAway = inM m start

main = do
  putStrLn "Enter <starting ending #moves> (blank line to exit):"
  input <- getLine
  when (not $ null input) $ do
    --let [start,end,m] = map read $ words input --goddammit heterogeneous input
    let [start',end',m'] = words input
        (start,end,m) = (read start', read end', read m')
    case canReachIn m start end of
      [] -> putStrLn $ show end ++ " cannot be reached in " ++ show m
        ++ " moves from " ++ show start
      moves -> putStr $ show end ++ " can be reached from "
        ++ show start ++ " by doing\n" ++ show moves ++"\n"
    main
