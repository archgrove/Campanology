module PlaceNotationParser (parseNotation) where

import Data.Char
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error

import BellTypes

{-
   Place notation is:
   GroupIndicator SymmetryMarker Places

   Grammar:
  
   Method ::= Group Sym Places
   Group ::= 'a' | 'b' | ... 
   Sym ::= '&' | '+'
   Places ::= Place Exchange Places | Place '.' Places | Place
   Place ::= Exchange | Making
   Making ::= BellCode+
-}

-- Parses a place notation method p with assumed bell count n
parseNotation :: String -> Int -> Either Method String
parseNotation p n = case (parse method "" p) of
  Left error -> Right (show error)
  Right (Changes g _ s cs) -> Left (Changes g n s cs)

method :: Parser Method
method = do
  g <- group 
  space
  s <- sym
  space
  ps <- places
  return (Changes g 0 s ps)

group :: Parser Group
group = do
  c <- letter
  return UnknownGroup
  <?> "group identifier"

sym :: Parser Symmetry
sym = do
  c <- char '&' <|> char '+'
  return (if (c == '&') then Symmetric else Asymmetric)
  <?> "symmetry marker"

places :: Parser [Change]
places = many1 (do
  p <- place
  optional (char '.')
  return p)

place :: Parser Change
place = try (do
  char 'x'
  return Exchange)
  <|> making <?> "place"

making :: Parser Change
making = do
  is <- many1 digit
  return (Make (map digitToInt is))
