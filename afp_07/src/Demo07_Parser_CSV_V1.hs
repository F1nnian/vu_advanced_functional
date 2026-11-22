module Demo07_Parser_CSV_V1 where

import Text.ParserCombinators.Parsec

csv :: Parser [[String]]
csv = do 
  result <- many line
  eof >> return result

-- Each line contains 1 or more cells, separated by a comma
line :: Parser [String]
line = do 
  result <- cells
  eol >> return result
       
-- Build up a list of cells.  Try to parse the first cell, then figure out 
-- what ends the cell.
cells :: Parser [String]
cells = do 
  first <- cellContent
  next <- remainingCells
  return $ first : next

-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingCells :: Parser [String]
remainingCells = (char ',' >> cells) <|> return []

-- Each cell contains 0 or more characters, which must not be a comma or
-- EOL
cellContent :: Parser String
cellContent = many (noneOf ",\n")
       

-- The end of line character is \n
eol :: Parser ()
eol = char '\n' >> return ()

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csv "(unknown)" input