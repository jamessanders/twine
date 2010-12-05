{-# LANGUAGE NoMonomorphismRestriction #-}
module Text.RSTemplate.Parser2 where

import Data.ByteString.Char8 (ByteString, pack)
import Debug.Trace
import System.FilePath
import Text.Parsec hiding (token)
import Text.Parsec.ByteString
import Text.RSTemplate.Parser.Types
import Control.Monad
import qualified Text.RSTemplate.Parser.ExprParser as EP

token t = do
  x <- string t
  spaces
  return t

template =  templateEntities <|> textBlock

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
  blocks <- manyTill template (string "@}")
  return (Loop (from) ident blocks)

conditional = do
  token "{?"
  token "|" <?> "start of conditional expression"
  expr <- expression
  spaces
  char '|' <?> "end of conditional expression"
  blocks <- manyTill template (string "?}")
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

------------------------------------------------------------------------

templateParser = manyTill template eof

parseTemplate name src = case parse templateParser name src of
                           Right res -> res
                           Left  err -> error (show err)

parseFile fp = do 
  parsed <- parseFromFile templateParser fp 
  case parsed of
    Right res -> return res
    Left  err -> error (show err)

loadTemplate fp = parseFile fp >>= doInclude (takeDirectory fp)

doInclude base ps = foldM ax [] ps 
    where ax a (Incl fs) = do pf <- parseFile (base </> fs) 
                              wi <- doInclude (takeDirectory (base </> fs)) pf
                              return (a ++ wi)
          ax a x         = return (a ++ [x])



