{-# LANGUAGE NoMonomorphismRestriction #-}
module Text.Twine.Parser (loadTemplateFromFile, loadTemplateFromString, Template) where

import Data.ByteString.Char8 (ByteString, pack)
import Debug.Trace
import System.FilePath
import Text.Parsec hiding (token)
import Text.Parsec.ByteString
import Text.Twine.Parser.Types
import Control.Monad

token t = do
  x <- string t
  spaces
  return t

template  =  try altMacro <|> templateEntities <|> textBlock
template'  =  templateEntities <|> textBlock

templateEntities = try slot <|> try conditional <|> try macro <|> try loop <|> try assign <|> include <?> "Template entity"

startOfEntities = try (string "{{") 
                  <|> try (string "{@")
                  <|> try (string "{|")
                  <|> try (string "{+")
                  <|> try (string "{?")
                  <|> try (string "@")
                  <|> try (string "{=")
                  <?> "start of entity"

endOfEntities = try (string "}}") 
                <|> try (string "@}")
                <|> try (string "|}")
                <|> try (string "+}")
                <|> try (string "?}")
                <|> try (string "=}")
                <|> try (string "\n@")
                <|> try (string "\n-end")
                <?> "end of entity"

textBlock = do 
  text <- manyTill anyChar ((lookAhead startOfEntities >> return ()) <|> (lookAhead endOfEntities >> return ()) <|> eof)
  return (Text $ pack text)

macro = do
  token "{=" <?> "Start of macro"
  token "|" <?> "Start of macro signature"
  ident <- name
  spaces
  token "("
  names <- sepBy name (token ",")
  token ")"
  token "|" <?> "end of macro signature"
  blocks <- manyTill template (string "=}")
  return (Macro ident names blocks)
  
altMacro = do
  optional (try $ char '\n')
  char '@'
  ident <- name
  spaces
  token "("
  names <- sepBy name (token ",")
  token ")"
  spaces
  token "=>"
  blocks <- manyTill template' (try (string "\n-end") <|> lookAhead (try (string "\n@")) <|> (eof >> return ""))
  return (Macro ident names blocks)

slot = do
  token "{{" <?> "Start of slot"
  spaces
  expr <- expression
  spaces
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

-- TODO Remove the cruft from the original expression parser.

macroAccessor = do
  char '@'
  b <- expression
  return $ Accessor (Var $ pack "macros") b

accessor = do
  a <- try method <|> try atom <?> "property or method"
  char '.'
  b <- expression
  return $ Accessor a b

method = do
  a <- name
  token "("
  expr <- sepBy expression (token ",")
  token ")"
  return $ Func a expr

sexpr = do
  token "("
  n <- name
  spaces
  expr <- sepBy expression' (space)
  token ")"
  return $ Func n expr

openExpr = do
  n <- name
  spaces
  expr <- sepBy1 expression' (space)
  return $ Func n expr

string' = do
  char '"'
  manyTill (noneOf "\"") (char '"')

stringLiteral = do
  st <- string'
  return (StringLiteral (pack st))

numberLiteral = do
  num <- many1 (digit)
  return (NumberLiteral (read num))

valid = (letter <|> (oneOf "#+-*$/?_") <|> digit)

name = do
  first <- try letter <|> oneOf "#+-*$/?_" 
  at <- many valid
  return (pack $ first : at)

atom = do
  n <- name
  return (Var n)

expression = try sexpr <|> try macroAccessor <|> try accessor <|> try method <|> try openExpr <|> try atom <|> try stringLiteral <|> numberLiteral  <?> "expression"
expression' = try sexpr <|> try atom <|> try stringLiteral <|> numberLiteral  <?> "expression"
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

doInclude base ps = foldM ax [] ps 
    where ax a (Incl fs) = do pf <- parseFile (base </> fs) 
                              wi <- doInclude (takeDirectory (base </> fs)) pf
                              return (a ++ wi)
          ax a x         = return (a ++ [x])

------------------------------------------------------------------------
 
loadTemplateFromFile :: FilePath -> IO Template
loadTemplateFromFile fp = parseFile fp >>= doInclude (takeDirectory fp)

loadTemplateFromString :: String -> Template
loadTemplateFromString = parseTemplate "theTemplate"
