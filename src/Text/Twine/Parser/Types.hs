module Text.Twine.Parser.Types where

import Data.ByteString.Char8 (ByteString)

type Key      = ByteString
type Name     = ByteString 
type Template = [TemplateCode]

data Expr = 
  Func Name [Expr] 
  | Var Name 
  | StringLiteral ByteString
  | NumberLiteral Integer
  | Accessor Expr Expr
  deriving (Show,Read,Eq)

data TemplateCode = 
  Text ByteString 
  | Slot Expr
  | Loop Expr Key [TemplateCode]
  | Cond Expr     [TemplateCode]
  | Incl FilePath
  | Assign Key Expr
  | Macro Name [Name] [TemplateCode]
  deriving (Show)

