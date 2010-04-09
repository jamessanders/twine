{-# LANGUAGE ExistentialQuantification, 
  TypeSynonymInstances, 
  FlexibleInstances, 
  OverlappingInstances, 
  UndecidableInstances  #-}

-- This is where all the parsing magic happens.

module Text.RSTemplate.Parser (parseTemplate
                              ,parseFile
                              ,doInclude
                              ) where

import Control.Monad
import Control.Monad.State
import Data.Char 
import Data.List
import Data.Maybe
import System.FilePath
import Text.RSTemplate.Parser.Types
import qualified Data.ByteString.Char8 as C


strip :: C.ByteString -> C.ByteString
strip = C.reverse . C.dropWhile isSpace . C.reverse . C.dropWhile isSpace

split :: Char -> [Char] -> [[Char]]
split delim s
    | [] == rest = [token]
    | otherwise = token : split delim (tail rest)
    where (token,rest) = span (/= delim) s


shift :: State ParserState ()
shift = do ps <- get
           let nd = getTextQ ps `C.append` C.singleton (C.head (getTemplate ps) )
           put $ ps { getTextQ = nd
                    , getTemplate = (C.tail $ getTemplate ps) }

addToTextQ st = do ps <- get 
                   put $ ps { getTextQ = getTextQ ps `C.append` st}

dropC :: State ParserState Char
dropC = do ps <- get
           put $ ps { getTemplate = (C.tail $ getTemplate ps) }
           return (C.head $ getTemplate ps)

dropN 0 = return ()
dropN n = dropC >> dropN (n-1)

digestTextBlock :: State ParserState ()
digestTextBlock = do ps <- get
                     put $ ps { getBlocks = (getBlocks ps) ++ [Text $ getTextQ ps]
                              , getTextQ = C.empty  }

addBlock bl = do ps <- get
                 put $ ps { getBlocks = getBlocks ps ++ [bl] }

stepParser :: State ParserState ()
stepParser = do c <- getChar 
                case c of
                  '\\'-> dropC >> continue
                  '{' -> dropC >> runCommand
                  otherwise -> if c == '\0' then digestTextBlock else continue

    where runCommand = do n <- getChar
                          case n of
                            '{' -> digestTextBlock >> dropC >> substitute 
                            '@' -> digestTextBlock >> dropC >> loop
                            '?' -> digestTextBlock >> dropC >> conditional
                            '+' -> digestTextBlock >> dropC >> include
                            _   -> addToTextQ (C.pack "{")  >> continue

          getChar    = do t <- fmap getTemplate get
                          if C.null t then return '\0' else return (C.head t)

          dropTillChar c o = do ch <- getChar
                                if ch == c 
                                  then return o 
                                  else dropC >>= \u->dropTillChar c (C.append o (C.singleton u))

          continue   = shift >> stepParser

          substitute = do key <- fmap strip (dropTillChar '}' (C.pack ""))
                          dropC 
                          dropC 
                          addBlock $ Slot (C.unpack key)
                          stepParser

          conditional = do tx <- fmap getTemplate get
                           let v   = getParam tx
                           let ei  = findClosing "{?" "?}" tx
                           let tmp = C.take (ei - 1) tx
                           let psd = parseTemplate (jumpParam tmp)
                           addBlock $ Cond (C.unpack v) psd
                           dropN (ei + 1)
                           stepParser

          loop       = do tx <- fmap getTemplate get
                          let k  = getLoopParamL tx
                          let as = getLoopParamN tx
                          let ei = findClosing "{@" "@}" tx
                          let tmp = C.take (ei - 1) tx
                          addBlock $ Loop (C.unpack k) (C.unpack as) (parseTemplate (jumpParam tmp))
                          dropN (ei + 1)
                          stepParser

          include     = do tx <- fmap getTemplate get
                           let ei  = findClosing "{+" "+}" tx
                           let tmp = C.take (ei - 1) tx
                           addBlock $ Incl (C.unpack (strip tmp))
                           dropN (ei + 1)
                           stepParser

          getLoopParamN  = strip . C.takeWhile (/='<') . C.tail . C.dropWhile (/= '|') 
          getLoopParamL  = strip . C.takeWhile (/='|') . C.drop 2 . C.dropWhile (/='<') . C.tail . C.dropWhile (/= '|') 
          getParam  = strip . C.takeWhile (/='|') . C.tail . C.dropWhile (/= '|') 
          jumpParam = C.tail . C.dropWhile (/= '|') . C.tail . C.dropWhile (/= '|')


findClosing open' close' text = findClosing' text 0 0
   where
    open  = C.pack open'
    close = C.pack close'
    findClosing' text i 0 = if i == 0 then findClosing' text i 1 else i
    findClosing' x  i n   | C.length x == 1 = i + 1
    findClosing' text i n = if open `C.isPrefixOf` text
                              then findClosing' (C.tail text) (i+1) (n+1)
                              else if close `C.isPrefixOf` text
                                     then findClosing' (C.tail text) (i+1) (n-1)
                                     else findClosing' (C.tail text) (i+1) n

------------------------------------------------------------------------

parseTemplate t = getBlocks $ execState stepParser $ makePS t

parseFile fp = C.readFile fp >>= return . parseTemplate

doInclude base ps = foldM ax [] ps 
    where ax a (Incl fs) = do pf <- parseFile (base </> fs) 
                              wi <- doInclude base pf
                              return (a ++ wi)
          ax a x         = return (a ++ [x])

