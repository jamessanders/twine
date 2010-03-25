{-# LANGUAGE ExistentialQuantification, 
  TypeSynonymInstances, 
  FlexibleInstances, 
  OverlappingInstances, 
  UndecidableInstances  #-}
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Char 
import Data.List

type Key = String
data TemplateCode = Text String 
                  | Slot Key
                  | Loop Key TemplateCode
                  | Cond Key TemplateCode

data ContextItem = ContextPairs [(Key,ContextItem)]
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

instance (Num a,Show a) => ToContext a where
    toContext a = ContextValue $ show a

mergeCXP (ContextPairs a) (ContextPairs b) = ContextPairs (a ++ b)
cxpLookup k (ContextPairs a) = lookup k a

test = [("fname","James")
       ,("lname","Sanders")
       ,("city","Chattanooga")
       ,("sastate","Tennessee")]

data Pet = Pet {petName :: String, petType :: String}

instance ToContext Pet where
    toContext x = ContextPairs [("name",ContextValue $ petName x)
                               ,("type",ContextValue $ petType x)]

pets = toContext $ ("pets",[Pet "Samson" "Dog"
                           ,Pet "Kiwi"   "Bird"
                           ,Pet "Maple"  "Dog"
                           ,Pet "Simon"  "Cat"])

showTmp (ContextValue x) = x
showTmp (ContextList  x) = show x
showTmp (ContextPairs x) = show x

toCX (ContextValue _) = error "not a context"
toCX (ContextPairs x) = x
toCX (ContextList  x) = toCX (head x)

addPrefix n pf (ContextPairs x) = (ContextPairs $ map (\(a,b) -> (pf ++ "." ++ a,b)) x) `mergeCXP` toContext [("#",show n)]
addPrefix n pf (ContextValue x) = ContextPairs [("#",ContextValue $ show n),("_",ContextValue x)]

mapCL f (ContextList x)  = map (\(a,b)->f b a) $ zip x [1..]
mapCL f a@(ContextValue x) = [f 1 a]
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
                            '@' -> dropC >> loop
                            '?' -> dropC >> conditional
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

          conditional = do tx <- fmap getTemplate get
                           cx <- fmap getContext  get 
                           let v  = getParam tx
                           let ei = findClosing "{?" "?}" tx
                           let tmp = take (ei - 1) tx
                           case cxpLookup v cx of
                             Just a -> toDisplay $ parseTemplate (jumpParam tmp) $ cx
                             Nothing -> toDisplay ""
                           dropN (ei + 1)
                           stepParser

          loop       = do tx <- fmap getTemplate get
                          cx <- fmap getContext  get 
                          let v  = getParam tx
                          let ei = findClosing "{@" "@}" tx
                          let tmp = take (ei - 1) tx
                          let d = mapCL (\a b->parseTemplate (jumpParam tmp) . (mergeCXP cx) $ addPrefix a v $ b ) 
                                  $ fromMaybe (ContextList []) $ cxpLookup v cx
                          --toDisplay $ show $ fmap (mapCL (addPrefix v)) $ cxpLookup v cx
                          dropN (ei + 1)
                          toDisplay $ concat d
                          stepParser

          getParam  = strip . takeWhile (/='|') . tail . dropWhile (/= '|') 
          jumpParam = tail . dropWhile (/= '|') . tail . dropWhile (/= '|')

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
