{-# LANGUAGE ExistentialQuantification, 
  TypeSynonymInstances, 
  FlexibleInstances, 
  OverlappingInstances, 
  UndecidableInstances  #-}

module Text.RSTemplate.Types where

import qualified Data.ByteString.Char8 as C

type Key = String
data TemplateCode = Text C.ByteString 
                  | Slot Key
                  | Loop Key [TemplateCode]
                  | Cond Key [TemplateCode]
                  deriving (Show)


data ContextItem = ContextPairs [(Key,ContextItem)]
                 | ContextValue C.ByteString
                 | ContextList [ContextItem]
                   deriving (Show)

data CX = forall a. (ToContext a) => CX a

data ParserState = ParserState { getBlocks   :: [TemplateCode] 
                               , getTextQ    :: C.ByteString
                               , getTemplate :: C.ByteString } deriving (Show)

data EvalState = EvalState { getDisplay :: C.ByteString }

class ToContext a where
    toContext :: a -> ContextItem

instance ToContext CX where
    toContext (CX a) = toContext a

instance ToContext [CX] where
    toContext = ContextList . map toContext

instance ToContext a => ToContext [a] where
    toContext x = ContextList $ map toContext x

instance ToContext a => ToContext [(String,a)] where
    toContext x = ContextPairs $ map (\(a,b) -> (a,toContext b)) x

instance ToContext a => ToContext (String,a) where
    toContext x = ContextPairs $ [(\(a,b) -> (a,toContext b)) x]

instance ToContext String where
    toContext a = ContextValue (C.pack a)

instance (Num a,Show a) => ToContext a where
    toContext a = ContextValue (C.pack . show $ a)
