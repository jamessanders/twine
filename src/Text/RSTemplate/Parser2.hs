{-# LANGUAGE NoMonomorphismRestriction #-}
module Text.RSTemplate.Parser2 where

import Text.RSTemplate.Parser.Types
import qualified Text.RSTemplate.Parser.ExprParser as EP
import Text.Parsec hiding (token)
import Text.Parsec.ByteString
import Data.ByteString.Char8 (ByteString, pack)

token t = do
  x <- string t
  spaces
  return t

parseTemplate =  try templateEntities <|> textBlock

templateEntities = try slot <|> try loop <?> "Template entity"

startOfEntities = string "{{" <|> string "{@" <?> "start of entity"
endOfEntities = string "}}" <|> string "@}" <?> "end of entity"

--expression = many alphaNum >>= return . Var . pack
expression = try EP.sexpr <|> EP.atom <?> "expression"

textBlock = do 
  text <- manyTill anyChar ((lookAhead startOfEntities >> return ()) <|> (lookAhead endOfEntities >> return ()) <|> eof)
  return (Text $ pack text)

slot = do
  token "{{"
  expr <- expression
  string "}}"
  return (Slot expr)

loop = do
  token "{@"
  token "|" <?> "start of loop expression"
  ident <- manyTill (anyChar) (lookAhead (try (many1 space) <|> try (string "<-") <|> (try newline >> return "")))
  spaces
  token "<-"
  from <- expression
  char '|' <?> "end of loop expression"
  blocks <- manyTill parseTemplate (string "@}")
  return (Loop (from) (pack ident) blocks)

