{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, OverlappingInstances  #-}
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Char 
import Data.List

data ContextItem = ContextPairs [(String,ContextItem)]
                 | ContextValue String
                 | ContextList [ContextItem]
                   deriving (Show)

data CX = forall a. (ToContext a) => CX a

data ParserState = PS { getDisplay  :: String
                      , getTemplate :: String
                      , getContext  :: ContextItem } 


class ToContext a where
    toContext :: a -> ContextItem

instance ToContext CX where
    toContext (CX a) = toContext a

instance ToContext [CX] where
    toContext = ContextList . map toContext

instance ToContext a => ToContext [a] where
    toContext x = ContextList $ map toContext x

instance ToContext a => ToContext [(String,a)] where
    toContext x = ContextPairs $ map (\(a,b) -> (a,toContext b)) x

instance ToContext a => ToContext (String,a) where
    toContext x = ContextPairs $ [(\(a,b) -> (a,toContext b)) x]

instance ToContext String where
    toContext a = ContextValue a


-- instance ToContext ([Char], [[([Char], [Char])]]) where
--     toContext (x1,x2) = ContextPairs [(x1,ContextList $ map toContext x2)]


mergeCXP (ContextPairs a) (ContextPairs b) = ContextPairs (a ++ b)
cxpLookup k (ContextPairs a) = lookup k a

test = [("fname","James")
       ,("lname","Sanders")
       ,("city","Chattanooga")
       ,("state","Tennessee")]

data Pet = Pet {petName :: String, petType :: String}

instance ToContext Pet where
    toContext x = ContextPairs [("name",ContextValue $ petName x)
                               ,("type",ContextValue $ petType x)]

pets = toContext $ ("pets",[Pet "Samson" "Dog"
                           ,Pet "Kiwi"   "Bird"
                           ,Pet "Simon"  "Cat"])

showTmp (ContextValue x) = x
showTmp (ContextList  x) = show x
showTmp (ContextPairs x) = show x

toCX (ContextValue _) = error "not a context"
toCX (ContextPairs x) = x
toCX (ContextList  x) = toCX (head x)

addPrefix pf (ContextPairs x) = ContextPairs $ map (\(a,b) -> (pf ++ "." ++ a,b)) x
addPrefix pf (ContextValue x) = ContextPairs [(pf,ContextValue x)]

mapCL f (ContextList x)  = map f x
mapCL f a@(ContextValue x) = [f a]
mapCL f _ = error "not a list of key/value pairs"

makePS = PS "" 

shift :: State ParserState ()
shift = do ps <- get
           let nd = getDisplay ps ++ [head (getTemplate ps) ]
           put $ ps { getDisplay = nd
                    , getTemplate = (tail $ getTemplate ps) }

dropC = do ps <- get
           put $ ps { getTemplate = (tail $ getTemplate ps) }
           return (head $ getTemplate ps)

dropN 0 = return ()
dropN n = dropC >> dropN (n-1)

toDisplay d = do ps <- get
                 put $ ps { getDisplay = (getDisplay ps ++ d) }

stepParser :: State ParserState ()
stepParser = do c <- getChar
                case c of
                  '\\'-> dropC >> continue
                  '{' -> dropC >> runCommand
                  '\0'-> return ()
                  otherwise -> continue
    where runCommand = do n <- getChar
                          case n of
                            '{' -> dropC >> substitute 
                            '|' -> dropC >> loopParser
                            _   -> toDisplay "{" >> continue
          getChar    = do t <- fmap getTemplate get
                          if null t then return '\0' else return (head t)
          dropTillChar c o = do ch <- getChar
                                if ch == c 
                                  then return o 
                                  else dropC >>= \u->dropTillChar c (o ++ [u])
          continue   = shift >> stepParser
          substitute = do find <- fmap strip (dropTillChar '}' "")
                          dropC 
                          dropC 
                          cx <- fmap getContext get
                          toDisplay $ showTmp $ fromMaybe (ContextValue "") (cxpLookup find $ cx)
                          stepParser

          loopParser = do tx <- fmap getTemplate get
                          cx <- fmap getContext  get 
                          let v  = strip $ takeWhile (/= '|') tx
                          let ei = findClosing "{|" "|}" tx
                          let tmp = take (ei - 1) tx
                          let d = mapCL (parseTemplate (tail $ dropWhile (/= '|') tmp) . (mergeCXP cx) . addPrefix v) 
                                  $ fromMaybe (ContextList []) $ cxpLookup v cx
                          dropN (ei + 1)
                          toDisplay $ concat d
                          stepParser

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseTemplate :: String -> ContextItem -> String
parseTemplate t cx = getDisplay $ execState stepParser $ makePS t cx


findClosing open close text = findClosing' text 0 0
   where
    findClosing' text i 0 = if i == 0 then findClosing' text i 1 else i
    findClosing' [x]  i n = i + 1
    findClosing' text i n = if open `isPrefixOf` text
                                       then findClosing' (tail text) (i+1) (n+1)
                                       else if close `isPrefixOf` text
                                             then findClosing' (tail text) (i+1) (n-1)
                                             else findClosing' (tail text) (i+1) n
