module Text.RSTemplate.Parser.Types where

import qualified Data.ByteString.Char8 as C

type Key = String

data TemplateCode = Text C.ByteString 
                  | Slot Key
                  | Loop Key Key [TemplateCode]
                  | Cond Key     [TemplateCode]
                  | Incl FilePath
                  deriving (Show)


data ParserState = ParserState { getBlocks   :: [TemplateCode] 
                               , getTextQ    :: C.ByteString
                               , getTemplate :: C.ByteString } deriving (Show)

makePS = ParserState [] C.empty 