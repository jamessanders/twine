module Text.RSTemplate.ExprParser where

import Text.RSTemplate.Parser.Types
import Text.RSTemplate.Parser.Utils
import Data.List
import Data.Char
import qualified Data.ByteString.Char8 as C

data EPState = Left String | Right String
               deriving (Show)


-- TODO Make this parser not suck.

parseExpr str | not ('(' `elem` str) = Func "id" [Var str]
parseExpr str = parseFunc str

parseFunc str = let name  = takeWhile (/= '(') str
                    rest  = dropWhile (/= '(') str
                    close = findClosing "(" ")" $ C.pack rest
                in Func name (parseArgs $ drop 1 . take (close - 1) $ rest)

parseArgs ""  = []
parseArgs str = if isFunc str 
                  then let nstr = dropWhile (/=')') str
                           close = findClosing "(" ")" $ C.pack nstr 
                       in parseFunc str : parseArgs ( drop close $ nstr)
                  else let n = takeWhile (notend) str in
                       if not (null n) then parseT n : parseArgs (toNext str) else parseArgs (toNext str)
    where toNext x = let r = dropWhile (notend) x in if null r || head r == ')' then "" else tail r
          parseT "" = Var ""
          parseT x | isDigit (head x) = NumberLiteral $ read x
                   | head x == '"'    = StringLiteral . tail . init $ x
          parseT x = Var x
          notend x = x /= ')' && x /= ','

          isFunc = (== '(') . head' . dropWhile (\x->x /= '(' && x /= ',') 
              where head' [] = ' '
                    head' x  = head x