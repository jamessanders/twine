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
                  | Loop Key Key [TemplateCode]
                  | Cond Key     [TemplateCode]
                  deriving (Show)


data ContextItem = ContextPairs [CX]
                 | ContextValue C.ByteString
                 | ContextList [ContextItem]
                   deriving (Show)

data ParserState = ParserState { getBlocks   :: [TemplateCode] 
                               , getTextQ    :: C.ByteString
                               , getTemplate :: C.ByteString } deriving (Show)

data EvalState = EvalState { getDisplay :: C.ByteString }



class ContextLookup a where
    cxLookup :: String -> a -> Maybe ContextItem

instance ContextLookup CX where
    cxLookup k (CX a) = cxLookup k a

instance ContextLookup [CX] where
    cxLookup k []     = Nothing
    cxLookup k (x:xs) = case cxLookup k x of
                          Just a  -> Just a
                          Nothing -> cxLookup k xs

instance ContextLookup a => ContextLookup [a] where
    cxLookup k []     = Nothing
    cxLookup k (x:xs) = case cxLookup k x of
                          Just a  -> Just a
                          Nothing -> cxLookup k xs
    

instance ContextLookup ContextItem where
    cxLookup k (ContextPairs a) = cxLookup k a
 
instance ContextLookup [(String,ContextItem)] where
    cxLookup k a = lookup k a


data CX = forall a. (ContextLookup a) => CX a
instance Show CX where
    show _ = "<CX>"


class ToContext a where
    toContext :: a -> ContextItem

instance ToContext C.ByteString where
    toContext s = ContextValue s

instance ToContext String where
    toContext s = toContext (C.pack s)

instance ContextLookup a => ToContext a where
    toContext x = ContextPairs [CX x]

instance ToContext a => ToContext [a] where
    toContext x = ContextList $ map toContext x

instance ToContext a => ToContext (String,a) where
    toContext (k,v) = ContextPairs [CX [(k,toContext v)]]

instance ToContext a => ToContext [(String,a)] where
    toContext ls = foldl (<+>) (ContextPairs []) $ map toContext ls

mergeCXP (ContextPairs a) (ContextPairs b) = ContextPairs (a ++ b)
(<+>) = mergeCXP

justcx :: (ToContext a) => a -> Maybe ContextItem
justcx = Just . toContext

------------------------------------------------------------------------
-- TEST
------------------------------------------------------------------------

data PetType = Dog | Cat | Bird deriving (Show)
data Pet = Pet {  getPetsType :: PetType, getPetsName :: String , getPetsMother :: Maybe Pet } deriving (Show)

instance ContextLookup Pet where
    cxLookup k pet = case k of
                       "name" -> justcx $ getPetsName pet
                       "type" -> justcx . show $ getPetsType pet
                       "mother" -> case getPetsMother pet of
                                     Just a  -> justcx a
                                     Nothing -> Nothing
                       otherwise -> Nothing



roxy  = Pet Cat "Roxy" Nothing
simon = Pet Cat "Simon" (Just roxy)

template = C.pack "Pets: {@|p <- pets| {{p.name}} \n@}"