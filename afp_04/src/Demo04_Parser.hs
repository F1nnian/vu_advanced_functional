module Demo04_Parser where

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
 
 
parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 ->
      case getNat (dropSpace s1) of
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (dropSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (dropSpace s3) of
                Nothing -> Nothing
                Just (maxGrey, s4)
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                      case getBytes 1 s4 of
                        Nothing -> Nothing
                        Just (_, s5) ->
                          case getBytes (width * height) s5 of
                            Nothing -> Nothing
                            Just (bitmap, s6) ->
                              Just (Greymap width height maxGrey bitmap, s6)

-----------

dropSpace = L8.dropWhile isSpace
                              
matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = Just (L.drop (L.length prefix) str)
    | otherwise
        = Nothing

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
             Nothing -> Nothing
             Just (num,rest)
                 | num < 0   -> Nothing
                 | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count           = fromIntegral n
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both                              
                    
-----------                    
                    
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v

parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
    matchHeader (L8.pack "P5") s      >>?
    skipSpace                         >>?
    getNat                            >>?
    \(width, s1) -> skipSpace s1      >>?
    getNat                            >>?
    \(height, s2) -> skipSpace s2     >>?
    getNat                            >>?
    \(maxGrey, s3) -> getBytes 1 s3   >>?
    (getBytes (width * height) . snd) >>?
    \(bitmap, s4) -> Just (Greymap width height maxGrey bitmap, s4)

skipSpace :: L.ByteString -> Maybe L.ByteString
skipSpace s = Just (L8.dropWhile isSpace s)

-----------

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64           
    } deriving Show
                    
newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }
    
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))
        
parseByte :: Parse Word8
parseByte =
    getState ==> \state ->
    case L.uncons (string state) of
      Nothing ->
          bail "no more input"
      Just (byte,remainder) ->
          putState newState ==>&
          identity byte
        where newState = state { string = remainder,
                                 offset = newOffset }
              newOffset = offset state + 1     
              
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ (\s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err)


(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser  =  Parse chainedParser
  where chainedParser initState   =
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState

(==>&) :: Parse a -> Parse b -> Parse b
p1 ==>& p2 = p1 ==> \_ -> p2

instance Functor Parse where
    fmap f parser = parser ==> \result ->
                    identity (f result)
                    
w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
               then parseByte ==> \b -> (b:) <$> parseWhile p
               else identity []
                                
parseRawPGM :: Parse Greymap                                
parseRawPGM =
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)
  where notWhite = (`notElem` " \r\n\t")
  
parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& 
             identity ()

assert :: Bool -> String -> Parse ()
assert True  _   = identity ()
assert False err = bail err       

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           assert (not (null digits)) "digit expected" ==>&
           let n = read digits in
           assert (show n == digits) "integer overflow" ==>&
           identity n

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
    getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st { offset = offset st + n', string = t }
    in putState st' ==>&
       assert (L.length h == n') "end of input" ==>&
       identity h   
       
-- run parser       
parse :: Parse a -> L.ByteString -> Either String a
parse parser input = fst <$> runParse parser (ParseState input 0)

parseMaybe :: (L.ByteString -> Maybe (a,b)) -> L.ByteString -> Either String a
parseMaybe parser input = case parser input of
  Nothing -> Left ""
  Just (x,r) -> Right x

infoPgm :: (L.ByteString -> Either String Greymap) -> FilePath -> IO ()
infoPgm parser file = do 
  bs <- L.readFile file
  case parser bs of
    Left e -> error $ "error during parsing of " ++ file ++ "\n" ++ e
    Right pgm -> do 
      putStrLn $ "image information for " ++ file
      putStrLn $ "width: " ++ show (greyWidth pgm)
      putStrLn $ "height: " ++ show (greyHeight pgm)
      putStrLn $ "max grey value: " ++ show (greyMax pgm)
      putStrLn ""

      
tests = do
  let file = "pic_04.pgm"
  putStrLn "initial parser"
  infoPgm (parseMaybe parseP5) file
  putStrLn "intermediate parser"
  infoPgm (parseMaybe parseP5_take2) file
  putStrLn "final parser"  
  infoPgm (parse parseRawPGM) file
