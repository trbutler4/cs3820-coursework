module Main where
import Data.Char()
import System.Environment(getArgs)
import Data.List(sort, group, permutations, intercalate)
import Data.Tuple()
import Data.Binary.Get (Decoder(Fail))
import Language.Haskell.TH (pragAnnD, conE, TExp (unType))
import Distribution.Simple.LocalBuildInfo (allComponentsInBuildOrder)

main :: IO ()
main =
  do
    args <- getArgs
    case args of
      [] -> fail "enter at least one command line argument"
      "Read":filename:_ ->
        do
          --print filename
          contents <- readFile filename
          let nw = read contents :: [(Int, Int)] in
            writeFile "network.txt" (concat (createStrings nw))
      "Run":filename:sequence:_ ->
        do
          contents <- readFile filename
          let seq = read sequence :: [Int]
          let nw = read contents :: [(Int, Int)] in
            print (runNetwork seq nw)
      "Parallel":filename:_ ->
        do
          contents <- readFile filename
          let nw = read contents :: [(Int, Int)] in
            writeFile "parallel.txt" (concat (createParallelStrings (parallel nw)))
      "Sorting":filename:_ ->
        do
          contents <- readFile filename
          let nw = read contents :: [(Int, Int)] in
            print (isSortingNetwork (createTestLists (getNumWires nw)) nw)
      "Create":num:_ ->
          let n = read num :: Int in
            writeFile "parallel.txt" (concat (createParallelStrings (parallel (createNetwork n))))
      _ -> fail "unrecognized command"
-- in terminal: runhaskell Main.hs Read filename
-- in ghci: :main Read filename



-- problem 2 stuff
-- create network strings for sort input 
createStrings :: [(Int, Int)] -> [String]
createStrings ns = [show (fst n) ++ " -- " ++ show (snd n) ++ "\n" | n <- ns]



-- problem 3 stuff
-- go through sequence and apply wire swaps 
runNetwork :: [Int] -> [(Int, Int)] -> [Int]
runNetwork [] _ = []
runNetwork s [] = s
runNetwork s nw = runNetwork (uncurry swap (head nw) s) (drop 1 nw)

-- swap sequence numbers at wire locations 
swap :: Int -> Int -> [Int] -> [Int]
swap x1 x2 xs = let
  elem1 = xs !! (x1 - 1)
  elem2 = xs !! (x2 - 1)
  left = take (x1 - 1) xs
  middle = take (x2 - x1 - 1) (drop x1 xs)
  right = drop x2 xs
  in
    if elem1 > elem2
      then left ++ [elem2] ++ middle ++ [elem1] ++ right
    else
      xs




-- problem 4 stuff

-- create strings of parrallel objects
createParallelStrings :: [[(Int,Int)]] -> [String]
createParallelStrings ws = [createParallelString w | w <- ws]

-- create single string of parallel wires
createParallelString :: [(Int, Int)] -> String
createParallelString ws = init (init (init (concat [show (fst w) ++ " -- " ++ show (snd w) ++ " , " | w <- ws]))) ++ "\n"


-- group parallel wires together
parallel :: [(Int, Int)] -> [[(Int, Int)]]
parallel [] = []
parallel xs = let
  group = getDistinct xs []
  in
    group : parallel (drop (length group) xs)

-- get a group of distinct (parallel) wires
getDistinct :: [(Int, Int)] -> [(Int,Int)] -> [(Int, Int)]
getDistinct [] ys = []
getDistinct xs ys = let
  t = head xs
  in
  if distinct (head xs) ys
    then
      head xs : getDistinct (drop 1 xs) (head xs : ys)
  else
    getDistinct [] ys

-- determine if any wire in list of connections
distinct :: (Int, Int) -> [(Int, Int)] -> Bool
distinct x xs
  | inList (fst x) (createList xs) || inList (snd x) (createList xs) = False
  | otherwise = True

-- create list of all number in list of tuples
createList :: [(Int, Int)] -> [Int]
createList [] = []
createList xs = fst (head xs):snd (head xs):createList (tail xs)

-- determine if int in list of ints
inList :: Int -> [Int] -> Bool
inList a [] = False
inList a (x:xs)
  | a == x = True
  | otherwise = inList a xs




-- problem 5 stuff
-- If a comparison network with n inputs sorts all 2^n possible sequences of 0's and 1's correctly, then it sorts all sequences of arbitrary numbers correctly.

getNumWires :: [(Int,Int)] -> Int
getNumWires xs = maximum (createList xs)

createTestLists :: Int -> [[Int]]
createTestLists n = mapM (const [0,1]) [1..n]

isSortingNetwork :: [[Int]] -> [(Int,Int)] -> Bool
isSortingNetwork [] ws = True
isSortingNetwork xs ws =
  runNetwork (head xs) ws == sort (head xs) &&
  isSortingNetwork (drop 1 xs) ws

-- run the network with zeros list and the actual network read in 




-- problem 6 stuff 

-- create network (this will create a sorting network, but it wont match the example)
createNetwork :: Int -> [(Int,Int)]
createNetwork n = intercalate [] ([createParallel n | n <- [2..n]] ++ [createParallel n | n <- [(n-1),(n-2)..2]])
-- go through, increase until reach peak, then go down until 0

createLeft :: Int -> Int -> [(Int,Int)]
createLeft 0 _ = []
createLeft 1 _ = []
createLeft n d = undefined

createDiagonal :: Int -> [(Int,Int)]
createDiagonal n = [(n,n+1) | n <- [1..n]]

createParallel :: Int -> [(Int, Int)]
createParallel n
  | even n = [(n,n+1) | n <- [1,3..(n-1)]]
  | otherwise = [(n, n+1) | n <- [2,4..(n-1)]]









