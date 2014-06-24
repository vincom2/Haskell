-- largely cribbed from https://github.com/quchen/articles/blob/master/write_yourself_a_brainfuck.md
-- I only did exercises 1.1, 1.2, 1.3.i, and 1.3.ii

module Main where
import Data.Maybe (mapMaybe)
import Data.Char (chr, ord)
import System.IO (hFlush, stdout)
import System.Environment (getArgs)

data Command = GoRight       -- >
             | GoLeft        -- >
             | Increment     -- +
             | Decrement     -- -
             | Print         -- .
             | Read          -- ,
             | LoopLeft      -- [
             | LoopRight     -- ]

newtype Source = Source [Command]

showCommand :: Command -> Char
showCommand GoRight   = '>'
showCommand GoLeft    = '<'
showCommand Increment = '+'
showCommand Decrement = '-'
showCommand Print     = '.'
showCommand Read      = ','
showCommand LoopLeft  = '['
showCommand LoopRight = ']'

instance Show Source where
  show (Source cs) = map showCommand cs

checkSyntax :: Source -> Either String Source
checkSyntax s@(Source cs) =
  let n = checkSyntax' 0 cs in
  -- these checks aren't actually good
  if n == 0 then Right s
    else Left (if n < 0 then "Mismatched closing bracket"
      else "Mismatched opening bracket")
  -- this is ugly. also I'm not sure it's idiomatic.
  where checkSyntax' n []               = n
        checkSyntax' n (LoopLeft:rest)
          | n < 0 = n
          | otherwise = checkSyntax' (n+1) rest
        checkSyntax' n (LoopRight:rest)
          | n < 0 = n
          | otherwise = checkSyntax' (n-1) rest
        checkSyntax' n (_:rest)
          | n < 0 = n
          | otherwise = checkSyntax' n rest

parse :: String -> Either String Source
parse bf = checkSyntax parsed
  where parseChar '>' = Just GoRight
        parseChar '<' = Just GoLeft
        parseChar '+' = Just Increment
        parseChar '-' = Just Decrement
        parseChar '.' = Just Print
        parseChar ',' = Just Read
        parseChar '[' = Just LoopLeft
        parseChar ']' = Just LoopRight
        parseChar  _  = Nothing
        parsed = Source $ mapMaybe parseChar bf

data Tape a = Tape [a] -- left of head
                    a  -- head
                   [a] -- right of head

emptyTape :: Tape Int
emptyTape = Tape zeros 0 zeros
  where zeros = repeat 0

moveRight :: Tape a -> Tape a
moveRight (Tape ls h (r:rs)) = Tape (h:ls) r rs

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) h rs) = Tape ls l (h:rs)

advance :: Tape Int -> Tape Command -> IO ()
advance _ (Tape _ _ []) = return ()
advance dataTape sourceTape = run dataTape (moveRight sourceTape)

-- Move the instruction pointer left until a "[" is found.
-- The first parameter ("b" for balance) retains the current
-- bracket balance to find the matching partner. When b is 1,
-- then the found LoopR would reduce the counter to zero,
-- hence we break even and the search is successful.
seekLoopRight :: Int                   -- Parenthesis balance
          -> Tape Int              -- Data tape
          -> Tape Command -- Instruction tape
          -> IO ()
-- don't understand why this is 1 and not 0
-- should read this more carefully
seekLoopRight 1 dataTape source@(Tape _ LoopRight _) = advance dataTape source
seekLoopRight b dataTape source@(Tape _ LoopRight _) =
      seekLoopRight (b-1) dataTape (moveRight source)
seekLoopRight b dataTape source@(Tape _ LoopLeft _) =
      seekLoopRight (b+1) dataTape (moveRight source)
seekLoopRight b dataTape source =
      seekLoopRight b dataTape (moveRight source)

seekLoopLeft :: Int                   -- Parenthesis balance
          -> Tape Int              -- Data tape
          -> Tape Command -- Instruction tape
          -> IO ()
seekLoopLeft 1 dataTape source@(Tape _ LoopLeft _) = advance dataTape source
seekLoopLeft b dataTape source@(Tape _ LoopLeft _) =
      seekLoopLeft (b-1) dataTape (moveLeft source)
seekLoopLeft b dataTape source@(Tape _ LoopRight _) =
      seekLoopLeft (b+1) dataTape (moveLeft source)
seekLoopLeft b dataTape source =
      seekLoopLeft b dataTape (moveLeft source)


run :: Tape Int -> Tape Command -> IO ()
run dataTape sourceTape@(Tape _ GoRight _) =
  advance (moveRight dataTape) sourceTape
run dataTape sourceTape@(Tape _ GoLeft _) =
  advance (moveLeft dataTape) sourceTape
run (Tape ls h rs) sourceTape@(Tape _ Increment _) =
  advance (Tape ls (h+1) rs) sourceTape
run (Tape ls h rs) sourceTape@(Tape _ Decrement _) =
  advance (Tape ls (h-1) rs) sourceTape
run dataTape@(Tape _ h _) sourceTape@(Tape _ Print _) = do
  putChar (chr h)
  hFlush stdout
  advance dataTape sourceTape
run (Tape ls _ rs) sourceTape@(Tape _ Read _) = do
  p <- getChar
  advance (Tape ls (ord p) rs) sourceTape
run dataTape@(Tape _ h _) sourceTape@(Tape _ LoopLeft _)
  -- if the current element under the head is 0, jump to the corresponding ]
  | h == 0 = seekLoopRight 0 dataTape sourceTape
  -- otherwise ignore and continue
  | otherwise = advance dataTape sourceTape
run dataTape@(Tape _ h _) sourceTape@(Tape _ LoopRight _)
  | h /= 0 = seekLoopLeft 0 dataTape sourceTape
  | otherwise = advance dataTape sourceTape

runBrainfuck :: Source -> IO ()
runBrainfuck = run emptyTape . source2tape
  where source2tape (Source (b:bs)) = Tape [] b bs


-- helper functions to deal with the outside world
parseBrainfuck :: String -> Source
parseBrainfuck bf =
  case parse bf of
    Right s -> s
    Left e -> error e

usageGetArgs :: IO String
usageGetArgs = do
  args <- getArgs
  case args of
    [infile] -> return infile
    _        -> error "Usage: bfi <filename>"

main :: IO ()
main = do
  infile <- usageGetArgs
  bfs <- readFile infile
  runBrainfuck $ parseBrainfuck bfs