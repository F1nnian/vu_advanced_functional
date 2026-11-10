module Exercise04 where

import qualified Data.ByteString.Lazy as L
import Data.Char (chr, isDigit, isSpace)
import Data.Int (Int64)
import Data.Word (Word8)

data Greymap = Greymap
  { greyWidth :: Int,
    greyHeight :: Int,
    greyMax :: Int,
    greyData :: L.ByteString
  }
  deriving (Eq)

instance Show Greymap where
  show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

-- data ParseState = ParseState
--   { string :: L.ByteString,
--     offset :: Int64
--   }
--   deriving (Show)

newtype Parse a = Parse
  { runParse :: ParseState -> Either String (a, ParseState)
  }

identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

-- parseByte :: Parse Word8
-- parseByte =
--   getState ==> \state ->
--     case L.uncons (string state) of
--       Nothing ->
--         bail "no more input"
--       Just (byte, remainder) ->
--         putState newState ==> \_ ->
--           identity byte
--         where
--           newState =
--             state
--               { string = remainder,
--                 offset = newOffset
--               }
--           newOffset = offset state + 1

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

-- bail :: String -> Parse a
-- bail err =
--   Parse $
--     ( \s ->
--         Left $
--           "byte offset " ++ show (offset s) ++ ": " ++ err
--     )

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where
    chainedParser initState =
      case runParse firstParser initState of
        Left errMessage ->
          Left errMessage
        Right (firstResult, newState) ->
          runParse (secondParser firstResult) newState

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

instance Functor Parse where
  fmap f parser =
    parser ==> \result ->
      identity (f result)

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p =
  (fmap p <$> peekByte) ==> \mp ->
    if mp == Just True
      then
        parseByte ==> \b ->
          (b :) <$> parseWhile p
      else identity []

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat =
  parseWhileWith w2c isDigit ==> \digits ->
    if null digits
      then bail "digit expected"
      else
        let n = read digits
         in if show n /= digits
              then bail "integer overflow"
              else identity n

-- run parser
-- parse :: Parse a -> L.ByteString -> Either String a
-- parse parser input = fst <$> runParse parser (ParseState input 0)

infoPgm :: FilePath -> IO ()
infoPgm file = do
  bs <- L.readFile file
  case parse parsePlainPGM bs of
    Left e -> error $ "error during parsing of " ++ file ++ "\n" ++ e
    Right pgm -> do
      putStrLn $ "image information for " ++ file
      putStrLn $ "width: " ++ show (greyWidth pgm)
      putStrLn $ "height: " ++ show (greyHeight pgm)
      putStrLn $ "max grey value: " ++ show (greyMax pgm)
      return ()

{--
Task 1

    fmap id = id
<-> fmap id (Parse f) = id (Parse f)
<-> fmap id (Parse f) = Parse f         | by definition of id

<-> (Parse f) ==> \result -> identity (id result) = Parse f     | by definition of fmap, expand lhs

<-> (Parse f) ==> \result -> identity result = Parse f

<-> Parse chainedParser = Parse f
      where chainedParser istate =
              case runParse (Parse f) istate of
                Left err -> Left err
                Right (res, newState) ->
                  runParse ((\result -> identity result) res) newState      | by definition of ==>

<-> Parse chainedParser = Parse f
      where chainedParser istate =
              case f istate of
                Left err -> Left err
                Right (res, newState) ->
                  runParse (identity res) newState      | by definition of runParse and function application

<-> Parse chainedParser = Parse f
      where chainedParser istate =
              case f istate of
                Left err -> Left err
                Right (res, newState) ->
                  runParse (Parse (\s -> Right (res, s))) newState      | by definition of identity

<-> Parse chainedParser = Parse f
      where chainedParser istate =
              case f istate of
                Left err -> Left err
                Right (res, newState) ->
                  (\s -> Right (res, s)) newState       | by definition of runParse

<-> Parse chainedParser = Parse f
      where chainedParser istate =
              case f istate of
                Left err -> Left err
                Right (res, newState) -> Right (res, newState)      | function application

<-> Parse chainedParser = Parse f
      where chainedParser istate = f istate         | simplifying case expression (both branches map f istate to itself)

<-> Parse chainedParser = Parse f
      where chainedParser = f           | by function extensionality (for all istate)

<-> Parse f = Parse f       | substitute

<-> True
--}

-- Task 2.1: define parser

parseTimes :: Int -> Parse a -> Parse [a]
parseTimes 0 _ = identity []
parseTimes n p =
  p ==> \x ->
    parseTimes (n - 1) p ==> \xs ->
      identity (x : xs)

parsePlainPGM :: Parse Greymap
parsePlainPGM = 
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P2") "invalid plain header" ==>& -- header is P2 
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseTimes (width * height) (parsePixel maxGrey) ==> \pixels -> -- uses natural numbers instead of binary bytes 
    identity (Greymap width height maxGrey (L.pack (map fromIntegral pixels)))
  where
    notWhite = (`notElem` "\r\n\t ")
    parsePixel m = skipSpaces ==>& parseNat ==> \p -> assert (p <= m) "grey value exceeds max" ==>& identity p -- for 2.2 assert max grey value

-- Task 2.2: integrate row and column numbers for error reporting (refactor)

-- add line and column to ParseState
data ParseState = ParseState
  { string :: L.ByteString,
    offset :: Int64,
    line :: Int,
    column :: Int
  }
  deriving (Show)

-- include line and column in error messages
bail :: String -> Parse a
bail err =
  Parse $
    ( \s ->
        Left $
          "line "
            ++ show (line s)
            ++ ", col "
            ++ show (column s)
            ++ " (byte offset "
            ++ show (offset s)
            ++ "): "
            ++ err
    )

-- track line and column numbers in parseByte
parseByte :: Parse Word8
parseByte =
  getState ==> \state ->
    case L.uncons (string state) of
      Nothing ->
        bail "no more input"
      Just (byte, remainder) ->
        putState newState ==> \_ ->
          identity byte
        where
          isNL = byte == 10 -- ASCII newline
          newState =
            state
              { string = remainder,
                offset = offset state + 1,
                line = if isNL then line state + 1 else line state,
                column = if isNL then 1 else column state + 1
              }

-- init ParseState with line and column numbers 1 
parse :: Parse a -> L.ByteString -> Either String a
parse parser input = fst <$> runParse parser (ParseState input 0 1 1)

tests = do
  infoPgm "pic_04_plain.pgm"
  infoPgm "pic_04_plain_faulty.pgm"
