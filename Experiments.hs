import Data.List

-- Standard types
type Row = [Int]
type Swap = (Int, Int)
type Course = [Row]

-- Place notation types
data Change = Exchange | Make [Int]
data Symmetry = Symmetric | Asymmetric
data Method = Changes Int Symmetry [Change]

plainHuntDoubles :: Method
plainHuntDoubles = Changes 5 Symmetric [Make [5], Make [1], Make [5], Make [1]]
plainHuntMinor :: Method
plainHuntMinor = Changes 6 Symmetric [Exchange, Make [1, 6],
                                     Exchange, Make [1, 6],
                                     Exchange, Make [1, 6],
                                     Exchange, Make [1, 6],
                                     Exchange, Make [1, 6],
                                     Exchange, Make [1, 6]]

-- Applies a method
-- Dependent types would really help here
runMethod :: Method -> Course
runMethod (Changes n s cs) = scanl applyChange (rounds n) cs

printMethod :: Method -> IO [()]
printMethod method = mapM putStrLn strings
  where
    strings = courseToStrings course
    course = runMethod method

courseToStrings :: Course -> [String]
courseToStrings course = map rowToString course
  where
    rowToString :: Row -> String
    rowToString row = intercalate " " (map show row)


-- Apply a single change to a row
-- Assumes the bells making places are in order
applyChange :: Row -> Change -> Row
applyChange row change = performChanges
  where
    -- Rules of place notation
    performChanges = applySwaps row swaps
    swaps = concat (mapIndex createSwaps changeSets)
    createSwaps bells 0 = tuple Odd bells
    createSwaps bells n = tuple Even bells
    -- This will be a list of lists, each sub-list a sequence of bells to move
    changeSets = splitMany (rounds n) makingPlaces
    makingPlaces = case change of 
      Exchange -> []
      Make ps -> ps 
    n = length row

-- Swaps two bells in a sequence
swap :: Row -> Swap -> Row
swap s (a, b)
  | a < 1 || a > length s  = error "First bell out of range"
  | b < 1 || b > length s  = error "Second bell out of range"
  | otherwise  =
    let reindex x = if x == a then s !! (b - 1)
                    else if x == b then s !! (a - 1)
                    else s !! (x - 1)
    in map reindex [1..length s] 

-- Applies a list of swaps to a sequence in order
applySwaps :: Row -> [(Int, Int)] -> Row
applySwaps start exchanges = foldl swap start exchanges

-- Generates rounds on n bells
rounds :: Int -> Row
rounds n
  | n < 1     = error "Cannot have rounds on less than 1 bell"
  | otherwise = [1..n]

-- 
-- Generic functions
--

-- Split at list with several (ordered) indices
splitMany :: (Eq a) => [a] -> [Int] -> [[a]]
splitMany xs ys = filter ((/=) []) (splitManyFrom xs 0 ys)
  where
    splitManyFrom xs n [] = [drop n xs]
    splitManyFrom xs n (y:ys) =
      (take (y - 1 - n) (drop n xs)):(splitManyFrom xs y ys)

-- Tuple a list into pairs.
--   If the length is even, the mode is ignored
--   If the length is odd, then Even tupling starts at index 0, Odd at index 1
data TupleMode = Even | Odd deriving Eq
tuple :: TupleMode -> [a] -> [(a, a)]
tuple mode xs
  | odd (length xs) && mode == Even = pairUp xs
  | odd (length xs) && mode == Odd = pairUp (tail xs)
  | otherwise = pairUp xs

pairUp :: [a] -> [(a, a)]
pairUp xs
  | length xs >= 2 = (xs !! 0, xs !! 1):(pairUp (drop 2 xs))
  | otherwise = []

mapIndex :: (a -> Int -> b) -> [a] -> [b]
mapIndex f xs = mapAt f 0 xs
  where
    mapAt :: (a -> Int -> b) -> Int -> [a] -> [b]
    mapAt f n [] = []
    mapAt f n (x:xs) = (f x n):(mapAt f (n + 1) xs)

