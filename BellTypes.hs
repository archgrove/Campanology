module BellTypes where

-- Standard types
type Row = [Int]
type Swap = (Int, Int)
type Course = [Row]

-- Place notation types
data Change = Exchange | Make [Int]
data Symmetry = Symmetric | Asymmetric
data Group = UnknownGroup
data Method = Changes Int Group Symmetry [Change]
