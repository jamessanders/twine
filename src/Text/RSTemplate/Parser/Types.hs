module Text.RSTemplate.Parser.Types where

import qualified Data.ByteString.Char8 as C

type Key  = String
type Name = String 

data Expr = Func Name [Expr] 
          | Var Name 
          | StringLiteral String
          | NumberLiteral Integer
            deriving (Show,Read)

data TemplateCode = Text C.ByteString 
                  | Slot Expr
                  | Loop Expr Key [TemplateCode]
                  | Cond Expr     [TemplateCode]
                  | Incl FilePath
                  deriving (Show)


data ParserState = ParserState { getBlocks   :: [TemplateCode] 
                               , getTextQ    :: C.ByteString
                               , getTemplate :: C.ByteString } deriving (Show)

makePS = ParserState [] C.empty 