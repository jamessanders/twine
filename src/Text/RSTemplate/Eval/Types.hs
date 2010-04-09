{-# LANGUAGE ExistentialQuantification, 
  TypeSynonymInstances, 
  FlexibleInstances, 
  OverlappingInstances, 
  UndecidableInstances  #-}

module Text.RSTemplate.Eval.Types where

import qualified Data.ByteString.Char8 as C

data ContextItem a = ContextPairs [a]
                   | ContextValue C.ByteString
                   | ContextList [ContextItem a]
                     deriving (Show)

data EvalState = EvalState { getDisplay :: C.ByteString }

class ContextLookup a where
    cxLookup :: String -> a -> Maybe (ContextItem CX)

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
    

instance ContextLookup (ContextItem CX) where
    cxLookup k (ContextPairs a) = cxLookup k a
 
instance ContextLookup [(String,ContextItem CX)] where
    cxLookup k a = lookup k a


data CX = forall a. (ContextLookup a) => CX a
instance Show CX where
    show _ = "<CX>"


class ToContext a where
    toContext :: a -> ContextItem CX

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

justcx :: (ToContext a) => a -> Maybe (ContextItem CX)
justcx = Just . toContext
