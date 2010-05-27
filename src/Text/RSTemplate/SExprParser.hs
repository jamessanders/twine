{-# LANGUAGE OverloadedStrings #-}
module Text.RSTemplate.SExprParser (parseExpr) where

import Control.Applicative ((<$>))
import Data.ByteString.Char8 hiding (filter,tail)
import Prelude hiding (head)
import Text.Parsec
import Text.RSTemplate.Parser.Types
import qualified Data.Char as C
import qualified Prelude

mkNumber = NumberLiteral . read
mkVar    = Var . pack

getName (Var n) = n
             
openP = char '('
closeP = char ')' 

parser = many block 

block = do manyTill space (lookAhead openP)
           sexpr

valid = letter <|> oneOf "#+-*$/?._" <|> digit

sexpr = do openP 
           x <- filter (/= Var (pack "")) <$> manyTill (atom <|> sexpr) closeP
           return (Func (getName $ Prelude.head x) (tail x))

atom = do n <- lookAhead $ choice [char '"'
                                  ,digit
                                  ,valid]
          next n
    where
      next x | C.isNumber x = var mkNumber
             | x == '"' = sstring
      next _ = var mkVar
            
sstring = do char '"'
             x <- manyTill (anyChar) (char '"') 
             return . StringLiteral. pack $ x

var f = do x <- manyTill (valid) (lookAhead closeP <|> space)
           return (f x)

parseSexpr :: ByteString -> Either ParseError [Expr]
parseSexpr x = parse parser (unpack x) x

parseExpr x = let x' = if head x /= '(' then "(" `append` x `append` ")" else x
              in case parseSexpr x' of
                   Left  a -> error (show a)
                   Right a -> case a of 
                                [] -> error $ "Parser failed: " ++ (show a)
                                b  -> check $ Prelude.head b
              where check (Func n []) = Func "id" [Var n]
                    check x = x
