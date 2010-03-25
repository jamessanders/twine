{-# LANGUAGE ExistentialQuantification, 
  TypeSynonymInstances, 
  FlexibleInstances, 
  OverlappingInstances, 
  UndecidableInstances  #-}
module Text.RSTemplate.Parse (parseTemplate,evalTemplate,(<+>))) where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Char 
import Data.List
import Text.RSTemplate.Types

mergeCXP (ContextPairs a) (ContextPairs b) = ContextPairs (a ++ b)
(<+>) = mergeCXP

cxpLookup k (ContextPairs a) = lookup k a

showCX (ContextValue x) = x
showCX (ContextList  x) = show x
showCX (ContextPairs x) = show x

toCX (ContextValue _) = error "not a context"
toCX (ContextPairs x) = x
toCX (ContextList  x) = toCX (head x)

addPrefix n pf (ContextPairs x) = (ContextPairs $ map (\(a,b) -> (pf ++ "." ++ a,b)) x) `mergeCXP` toContext [("#",show n)]
addPrefix n pf (ContextValue x) = ContextPairs [("#",ContextValue $ show n),("_",ContextValue x)]

mapCL f (ContextList x)  = map (\(a,b)->f b a) $ zip x [1..]
mapCL f a@(ContextValue x) = [f 1 a]
mapCL f _ = error "not a list of key/value pairs"

makePS = ParserState [] "" 

shift :: State ParserState ()
shift = do ps <- get
           let nd = getTextQ ps ++ [head (getTemplate ps) ]
           put $ ps { getTextQ = nd
                    , getTemplate = (tail $ getTemplate ps) }

addToTextQ st = do ps <- get 
                   put $ ps { getTextQ = getTextQ ps ++ st }

dropC :: State ParserState Char
dropC = do ps <- get
           put $ ps { getTemplate = (tail $ getTemplate ps) }
           return (head $ getTemplate ps)

dropN 0 = return ()
dropN n = dropC >> dropN (n-1)

digestTextBlock :: State ParserState ()
digestTextBlock = do ps <- get
                     put $ ps { getBlocks = (getBlocks ps) ++ [Text $ getTextQ ps]
                              , getTextQ = "" }

addBlock bl = do ps <- get
                 put $ ps { getBlocks = getBlocks ps ++ [bl] }

stepParser :: State ParserState ()
stepParser = do c <- getChar
                case c of
                  '\\'-> dropC >> continue
                  '{' -> dropC >> runCommand
                  '\0'-> digestTextBlock
                  otherwise -> continue

    where runCommand = do n <- getChar
                          case n of
                            '{' -> digestTextBlock >> dropC >> substitute 
                            '@' -> digestTextBlock >> dropC >> loop
                            '?' -> digestTextBlock >> dropC >> conditional
                            _   -> addToTextQ "{"  >> continue

          getChar    = do t <- fmap getTemplate get
                          if null t then return '\0' else return (head t)

          dropTillChar c o = do ch <- getChar
                                if ch == c 
                                  then return o 
                                  else dropC >>= \u->dropTillChar c (o ++ [u])
          continue   = shift >> stepParser
          substitute = do key <- fmap strip (dropTillChar '}' "")
                          dropC 
                          dropC 
                          addBlock $ Slot key
                          stepParser

          conditional = do tx <- fmap getTemplate get
                           let v   = getParam tx
                           let ei  = findClosing "{?" "?}" tx
                           let tmp = take (ei - 1) tx
                           let psd = parseTemplate (jumpParam tmp)
                           addBlock $ Cond v psd
                           dropN (ei + 1)
                           stepParser

          loop       = do tx <- fmap getTemplate get
                          let v  = getParam tx
                          let ei = findClosing "{@" "@}" tx
                          let tmp = take (ei - 1) tx
                          addBlock $ Loop v (parseTemplate (jumpParam tmp))
                          dropN (ei + 1)
                          stepParser

          getParam  = strip . takeWhile (/='|') . tail . dropWhile (/= '|') 
          jumpParam = tail . dropWhile (/= '|') . tail . dropWhile (/= '|')

strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

findClosing open close text = findClosing' text 0 0
   where
    findClosing' text i 0 = if i == 0 then findClosing' text i 1 else i
    findClosing' [x]  i n = i + 1
    findClosing' text i n = if open `isPrefixOf` text
                              then findClosing' (tail text) (i+1) (n+1)
                              else if close `isPrefixOf` text
                                     then findClosing' (tail text) (i+1) (n-1)
                                     else findClosing' (tail text) (i+1) n

------------------------------------------------------------------------

evalTemplate tc cx = concat $ map (evalTemplateBlock cx) tc

evalTemplateBlock cx (Text t) = t
evalTemplateBlock cx (Slot k) = showCX $ fromMaybe (ContextValue "") (cxpLookup k cx)
evalTemplateBlock cx (Cond k bls) = case cxpLookup k cx of
                                      Just _ -> evalTemplate bls cx
                                      Nothing-> ""
evalTemplateBlock cx (Loop k bls) = case cxpLookup k cx of
                                      Just val -> concat $ mapCL runLoop val
                                      Nothing -> ""
    where runLoop n ls = let ncx = addPrefix n k ls `mergeCXP` cx in evalTemplate bls ncx


parseTemplate :: String -> [TemplateCode]
parseTemplate t = getBlocks $ execState stepParser $ makePS t

parseFile fp = readFile fp >>= return . parseTemplate
