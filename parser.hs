import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Char 
import Data.List


data ContextItem = CV String 
                 | CL [[(String,ContextItem)]] 
                 | CI ContextItem deriving Show

data ParserState = PS { getDisplay  :: String
                      , getTemplate :: String
                      , getContext  :: [(String,ContextItem)] } 
                   deriving (Show)

showTmp (CV x) = x
showTmp (CL x) = show x
showTmp (CI x) = showTmp x

toCX (CV _) = error "not a context"
toCX (CI x) = toCX x 
toCX (CL x) = x

addPrefix pf x = map (\(a,b) -> (pf ++ "." ++ a,b)) x

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
                          toDisplay $ showTmp $ fromMaybe (CV "") (lookup find cx)
                          stepParser

          loopParser = do tx <- fmap getTemplate get
                          cx <- fmap getContext  get 
                          let v  = strip $ takeWhile (/= '|') tx
                          let ei = findClosing "{|" "|}" tx
                          let tmp = take (ei - 1) tx
                          let d = map (parseTemplate (tail $ dropWhile (/= '|') tmp) . (cx ++) . addPrefix v) 
                                  $ toCX $ fromMaybe (CL []) $ lookup v cx
                          dropN (ei + 1)
                          toDisplay $ concat d
                          stepParser

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseTemplate :: String -> [(String, ContextItem)] -> String
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
