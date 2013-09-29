module BellTypes where

-- Standard types
type Row = [Int]
type Swap = (Int, Int)
type Course = [Row]

-- Place notation types
data Change = Exchange | Make [Int] deriving Show
data Symmetry = Symmetric | Asymmetric deriving Show
data Group = UnknownGroup deriving Show
data Method = Changes Group Int Symmetry [Change] deriving Show
