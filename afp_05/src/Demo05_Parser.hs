module Demo05_Parser where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace, isDigit, chr)
import Data.Int (Int64)
import Data.Word (Word8)


data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    } deriving Eq
    
instance Show Greymap where
 show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m
 
 

-----------                    
                    
parseP5_MaybeMonad :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_MaybeMonad s =
  matchHeader (L8.pack "P5") s            >>=
  \ s -> skipSpace s                      >>=
  \ s -> getNat s                         >>=
  \ (width, s) -> skipSpace s             >>=
  \ s -> getNat s                         >>=
  \ (height, s) -> skipSpace s            >>=
  \ s -> getNat s                         >>=
  \ (maxGrey, s) -> getBytes 1 s          >>=
  \ (_, s) -> getBytes (width * height) s >>=
  \ (bitmap, s) -> return (Greymap width height maxGrey bitmap, s)


parseP5_doNotation :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_doNotation s = do 
  s <- matchHeader (L8.pack "P5") s
  s <- skipSpace s
  (width, s) <- getNat s
  s <- skipSpace s
  (height, s) <- getNat s
  s <- skipSpace s
  (maxGrey, s) <- getNat s
  (_, s) <- getBytes 1 s
  (bitmap, s) <- getBytes (width * height) s 
  return (Greymap width height maxGrey bitmap, s)

-----------

assertMaybe :: Bool -> Maybe ()
assertMaybe True = Just ()
assertMaybe False = Nothing
                              
matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str = do
  assertMaybe $ prefix `L8.isPrefixOf` str
  return $ L.drop (L.length prefix) str

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = do
  (num, rest) <- L8.readInt s
  assertMaybe $ num >= 0
  return (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = do
  let count           = fromIntegral n
  let both@(prefix,_) = L.splitAt count str
  assertMaybe $ L.length prefix == count
  return both

skipSpace :: L.ByteString -> Maybe L.ByteString
skipSpace s = return $ L8.dropWhile isSpace s


-----------

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64           
    } deriving Show
                    
newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }
    

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser where
  chainedParser initState  =
     case runParse firstParser initState of
        Left errMessage -> Left errMessage
        Right (firstResult, newState) ->
            runParse (secondParser firstResult) newState

identity a = Parse (\s -> Right (a, s))    

instance Functor Parse where
  fmap f parser = parser ==> (\ x -> identity (f x))

instance Applicative Parse where
  pure = identity
  liftA2 f p1 p2 = p1 ==> (\ x -> fmap (f x) p2)
    
instance Monad Parse where
 (>>=) = (==>)
 return = pure
    
    
            
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ (\s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err)
            
parseByte :: Parse Word8
parseByte = do
  state <- getState
  case L.uncons (string state) of
      Nothing ->
          bail "no more input"
      Just (byte,remainder) -> do
        putState newState
        return byte
        where newState = state { string = remainder,
                                 offset = newOffset }
              newOffset = offset state + 1     
               
                    
w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = do
  mp <- fmap p <$> peekByte
  if mp == Just True
     then do 
       b <- parseByte
       bs <- parseWhile p
       return $ b : bs
     else return []

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseRawPGM_noDoNotation :: Parse Greymap                                
parseRawPGM_noDoNotation = 
  parseWhileWith w2c (not . isSpace) >>= \ header -> 
  assert (header == "P5") "invalid raw header" >>
  skipSpaces >>
  parseNat >>= \ width -> 
  skipSpaces >>
  parseNat >>= \ height -> 
  skipSpaces >>
  parseNat >>= \ maxGrey ->
  parseByte >>
  parseBytes (width * height) >>= \ bitmap ->
  return (Greymap width height maxGrey bitmap)
                                
parseRawPGM :: Parse Greymap                                
parseRawPGM = do
  header <- parseWhileWith w2c (not . isSpace)
  assert (header == "P5") "invalid raw header" 
  skipSpaces 
  width <- parseNat
  skipSpaces
  height <- parseNat
  skipSpaces
  maxGrey <- parseNat
  _ <- parseByte
  bitmap <- parseBytes (width * height) 
  return (Greymap width height maxGrey bitmap)


parseNat :: Parse Int
parseNat = do 
  digits <- parseWhileWith w2c isDigit 
  assert (not $ null digits) "digit expected"
  let n = read digits
  assert (show n == digits) "integer overflow"
  return n

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace >> return ()

assert :: Bool -> String -> Parse ()
assert True  _   = return ()
assert False err = bail err       

parseBytes :: Int -> Parse L.ByteString
parseBytes n = do
  st <- getState
  let n' = fromIntegral n
      (h, t) = L.splitAt n' (string st)
      st' = st { offset = offset st + L.length h, string = t }
   in do 
    assert (L.length h == n') "end of input"
    putState st'
    return h
       
-- run parser       
parse :: Parse a -> L.ByteString -> Either String a
parse parser input = fst <$> runParse parser (ParseState input 0)


infoPgm :: FilePath -> IO ()
infoPgm file = do 
  bs <- L.readFile file
  case parse parseRawPGM bs of
    Left e -> error $ "error during parsing of " ++ file ++ "\n" ++  e
    Right pgm -> do 
      putStrLn $ "image information for " ++ file
      putStrLn $ "width: " ++ show (greyWidth pgm)
      putStrLn $ "height: " ++ show (greyHeight pgm)
      putStrLn $ "max grey value: " ++ show (greyMax pgm)
      return () 
      
testParser = infoPgm "pic_04.pgm"