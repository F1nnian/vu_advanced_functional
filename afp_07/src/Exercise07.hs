module Exercise07 (checkARIs) where

import System.IO (hFlush, stdout)
import TRS
import Text.ParserCombinators.Parsec

eol =
  ( try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"
  )
    >> return ()

{- Task 1: define header -}

example :: String
example =
  unlines
    [ "; @author Takahito Aoto",
      "; @author Yoshihito Toyama",
      "; @cops 157",
      "; full experiments for [35]",
      "(format TRS)",
      "(fun * 2)",
      "(fun + 2)",
      "(fun 0 0)",
      "(fun s 1)",
      "(rule (+ x 0) x)",
      "(rule (+ x (s y)) (s (+ x y)))",
      "(rule (* x 0) 0)",
      "(rule (* x (s y)) (+ (* x y) x))",
      "(rule (+ (+ x y) z) (+ x (+ y z)))",
      "(rule (+ x y) (+ y x))"
    ]

header :: GenParser Char u Metadata
header = do
  entries <- endBy headerLine eol -- repeatedly parse headerLine followed by eol
  return (concat entries)

headerLine :: GenParser Char u [(String, String)]
headerLine = do
  _ <- char ';'
  metaEntry <|> comment

metaEntry :: GenParser Char u [(String, String)]
metaEntry = do
  _ <- try (string " @")
  key <- many1 (noneOf " \t\n\r") <?> "metadata key"
  _ <- char ' ' <?> "space separating metadata key and value"
  val <- many (noneOf "\n\r")
  return [(key, val)]

comment :: GenParser Char u [(String, String)]
comment = many (noneOf "\n\r") >> return []

{- Task 2 and 3: define content -}

content :: GenParser Char u [Rule]
content = do
  formatDecl
  sig <- many funDecl -- save function signatures and pass to ruleDecl
  rules <- many (ruleDecl sig)
  spaces
  eof
  return rules

formatDecl :: GenParser Char u ()
formatDecl = do
  spaces
  _ <- char '('
  spaces
  _ <- string "format"
  _ <- many1 space
  _ <- string "TRS"
  spaces
  _ <- char ')' <?> "closing parenthesis of format declaration"
  return ()

funDecl :: GenParser Char u (String, Int) -- changed form ()
funDecl = do
  _ <- try (spaces >> char '(' >> spaces >> string "fun" >> many1 space)
  f <- identifier
  _ <- many1 space
  arityStr <- many1 digit
  spaces
  _ <- char ')' <?> "closing parenthesis of fun declaration"
  return (f, read arityStr) -- save f and arityStr and return

ruleDecl :: [(String, Int)] -> GenParser Char u Rule -- accept function signatures as argument and pass to term parser
ruleDecl sig = do
  _ <- try (spaces >> char '(' >> spaces >> string "rule" >> many1 space)
  lhs <- term sig
  spaces
  rhs <- term sig
  spaces
  _ <- char ')' <?> "closing parenthesis of rule"
  return (lhs, rhs)

term :: [(String, Int)] -> GenParser Char u Term -- accept function signatures as argument and pass to both alternatives
term sig = funApp sig <|> varOrConstTerm sig

funApp :: [(String, Int)] -> GenParser Char u Term -- accept function signatures
funApp sig = do
  _ <- char '('
  spaces
  f <- identifier
  expectedArity <- case lookup f sig of -- lookup function symbol in signature list and return its arity
    Nothing -> fail ("unknown function symbol: " ++ f)
    Just 0 -> fail ("constant '" ++ f ++ "' must not be put in parentheses")
    Just n -> return n
  spaces
  args <- endBy (term sig) spaces
  _ <- char ')' <?> "closing parenthesis"
  if length args == expectedArity
    then return (Fun f args)
    else fail ("wrong arity for '" ++ f ++ "'")

varOrConstTerm :: [(String, Int)] -> GenParser Char u Term -- accept function signatures
varOrConstTerm sig = do
  x <- identifier
  case lookup x sig of -- if not function symbol, its variable, if arity 0, constant, otherwise cannot stand alone -> error
    Nothing -> return (Var x)
    Just 0 -> return (Fun x [])
    Just n -> fail ("function '" ++ x ++ "' with arity " ++ show n ++ " cannot stand alone")

identifier :: GenParser Char u String
identifier = many1 (noneOf " \t\n\r();:") <?> "identifier"

ari :: GenParser Char u (Metadata, [Rule])
ari = do
  meta <- header
  trs <- content
  return (meta, trs)

parseARI :: FilePath -> IO (Either ParseError (Metadata, [Rule]))
parseARI file = do
  input <- readFile file
  return $ parse ari file input

-- check a single ARI file
checkARI :: FilePath -> IO Bool
checkARI file = do
  putStr $ "Parsing " ++ file ++ " ... "
  hFlush stdout
  p <- parseARI file
  case p of
    Right _ -> putStrLn "ok" >> return True
    _ -> putStrLn "FAILED" >> return False

-- check all ARI files that are listed in input file
checkARIs :: FilePath -> IO ()
checkARIs file = do
  aris <- lines <$> readFile file
  checks <- mapM checkARI aris
  putStrLn $ if and checks then "all files parsed" else "some files failed"
