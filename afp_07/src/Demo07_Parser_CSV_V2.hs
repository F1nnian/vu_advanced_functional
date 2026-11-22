module Demo07_Parser_CSV_V2 where

import Text.ParserCombinators.Parsec

csv = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = (try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n" 
  <|> string "\r"
  <?> "end of line") >> return ()
  
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csv "(unknown)" input