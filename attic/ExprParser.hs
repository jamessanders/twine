{-# LANGUAGE OverloadedStrings #-}
module Text.RSTemplate.ExprParser where

import Text.RSTemplate.Parser.Types
import Text.RSTemplate.Parser.Utils
import Data.List
import Data.Char
import qualified Data.ByteString.Char8 as C

data EPState = Left String | Right String
               deriving (Show)


-- TODO Make this parser not suck.

parseExpr :: C.ByteString -> Expr
parseExpr str | not ('(' `C.elem` str) = Func "id" [Var str]
parseExpr str = parseFunc str

parseFunc str = let name  = C.takeWhile (/= '(') str
                    rest  = C.dropWhile (/= '(') str
                    close = findClosing "(" ")" $ rest
                in Func name (parseArgs $ C.drop 1 . C.take (close - 1) $ rest)

parseArgs :: C.ByteString -> [Expr]
parseArgs ""  = []
parseArgs str = if isFunc str 
                  then let nstr = C.dropWhile (/=')') str
                           close = findClosing "(" ")" $ nstr 
                       in parseFunc str : parseArgs ( C.drop close $ nstr)
                  else let n = C.takeWhile (notend) str in
                       if not (C.null n) then parseT n : parseArgs (toNext str) else parseArgs (toNext str)
    where toNext x = let r = C.dropWhile (notend) x in if C.null r || C.head r == ')' then "" else C.tail r
          parseT "" = Var ""
          parseT x | isDigit (C.head x) = NumberLiteral $ readbs x
                   | C.head x == '"'    = StringLiteral . C.tail . C.init $ x
          parseT x = Var x
          notend x = x /= ')' && x /= ','

          isFunc = (== '(') . head' . C.dropWhile (\x->x /= '(' && x /= ',') 
              where head' :: C.ByteString -> Char
                    head' "" = ' '
                    head' x  = C.head x

readbs :: (Read a) => C.ByteString -> a
readbs = read . C.unpack