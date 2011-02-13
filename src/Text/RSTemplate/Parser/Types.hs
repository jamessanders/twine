module Text.RSTemplate.Parser.Types where

import qualified Data.ByteString.Char8 as C

type Key  = C.ByteString
type Name = C.ByteString 

data Expr = Func Name [Expr] 
          | Var Name 
          | StringLiteral C.ByteString
          | NumberLiteral Integer
          | Accessor Expr Expr
            deriving (Show,Read,Eq)

data TemplateCode = Text C.ByteString 
                  | Slot Expr
                  | Loop Expr Key [TemplateCode]
                  | Cond Expr     [TemplateCode]
                  | Incl FilePath
                  | Assign Key Expr
                  deriving (Show)


data ParserState = ParserState { getBlocks   :: [TemplateCode] 
                               , getTextQ    :: C.ByteString
                               , getTemplate :: C.ByteString } deriving (Show)

makePS = ParserState [] C.empty 