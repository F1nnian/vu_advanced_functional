module Demo07_Parser_CSV_V3 where

import Text.ParserCombinators.Parsec

csv = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n")

quotedCell = do 
  _ <- char '"'
  content <- many quotedChar
  _ <- char '"' <?> "missing closing quote at end of cell"
  return content

quotedChar =
      noneOf "\""
  <|> try (string "\"\"" >> return '\"')

eol = (try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n" 
  <|> string "\r"
  <?> "end of line") >> return ()
  
parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csv "(unknown)" input

main = do
  let file = "test_07.csv"
  input <- readFile file
  print $ parse csv file input
