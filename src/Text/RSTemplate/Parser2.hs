{-# LANGUAGE NoMonomorphismRestriction #-}
module Text.RSTemplate.Parser2 where

import Text.RSTemplate.Parser.Types
import qualified Text.RSTemplate.Parser.ExprParser as EP
import Text.Parsec hiding (token)
import Text.Parsec.ByteString
import Data.ByteString.Char8 (ByteString, pack)
import Debug.Trace

token t = do
  x <- string t
  spaces
  return t

parseTemplate =  templateEntities <|> textBlock

templateEntities = try slot <|> try conditional <|> try loop <|> try assign <|> try include <?> "Template entity"

startOfEntities = try (string "{{") 
                  <|> try (string "{@")
                  <|> try (string "{|")
                  <|> try (string "{+")
                  <|> try (string "{?")
                  <?> "start of entity"

endOfEntities = try (string "}}") 
                <|> try (string "@}")
                <|> try (string "|}")
                <|> try (string "+}")
                <|> try (string "?}")
                <?> "end of entity"

textBlock = do 
  text <- manyTill anyChar ((lookAhead startOfEntities >> return ()) <|> (lookAhead endOfEntities >> return ()) <|> eof)
  return (Text $ pack text)

slot = do
  token "{{" <?> "Start of slot"
  expr <- expression
  string "}}" <?> "End of slot"
  return (Slot expr)

loop = do
  token "{@"
  token "|" <?> "start of loop expression"
  ident <- name
  spaces
  token "<-"
  from <- expression
  spaces
  char '|' <?> "end of loop expression"
  blocks <- manyTill parseTemplate (string "@}")
  return (Loop (from) ident blocks)

conditional = do
  token "{?"
  token "|" <?> "start of conditional expression"
  expr <- expression
  spaces
  char '|' <?> "end of conditional expression"
  blocks <- manyTill parseTemplate (string "?}")
  return (Cond expr blocks)

assign = do
  token "{|"
  key <- name
  spaces
  token "="
  expr <- expression
  spaces
  string "|}"
  return (Assign key expr)

include = do
  token "{+"
  path <- try string' <|> many1 (noneOf " +")  <?> "Filepath"
  spaces
  string "+}"
  return (Incl path)

------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------

sexpr = do
  token "("
  n <- name
  spaces
  expr <- sepBy expression (space)
  token ")"
  return $ Func n expr

string' = do
  char '"'
  manyTill (noneOf "\"") (char '"')

stringLiteral = do
  st <- string'
  return (StringLiteral (pack st))

numberLiteral = do
  num <- many1 (digit)
  trace num $ return (NumberLiteral (read num))

valid = (letter <|> (oneOf "#+-*$/?._") <|> digit)

name = do
  first <- try letter <|> oneOf "#+-*$/?._" 
  at <- many valid
  return (pack $ first : at)

atom = do
  n <- name
  return (Var n)

expression = try sexpr <|> try atom <|> try stringLiteral <|> numberLiteral  <?> "expression"

