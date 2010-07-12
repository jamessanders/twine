{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverlappingInstances, TypeSynonymInstances #-}
module Text.RSTemplate.Eval.Context where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Text.RSTemplate.Eval.Types
import Data.Maybe

-- instance (Monad m) => ContextLookup m [(String,String)] where

-- instance (Monad m) => ContextLookup m [(String,ContextItem m)] where
--     cxLookup k = return . lookup (C.unpack k) 

instance (Monad m) => ContextLookup m [(ByteString,ContextItem m)] where
     cxLookup k = return . fromMaybe ContextNull . lookup k

instance (Monad m) => ContextLookup m String where
    toContext = ContextValue . C.pack
    cxLookup _ _ = return ContextNull

instance (Monad m) => ContextLookup m ByteString where
    toContext a = ContextValue a
    cxLookup _ _ = return ContextNull

instance (Monad m) => ContextLookup m Bool where
    toContext = ContextBool
    cxLookup _ _ = return ContextNull

instance (Monad m,ContextLookup m a) => ContextLookup m [a] where
    toContext a  = ContextList (map toContext a)
    cxLookup _ _ = return ContextNull

-- instance (Monad m) => ContextLookup m [Context m] where
--     cxLookup k (x:xs) = do
--       x <- (getContext x) k 
--       case x of 
--         Just a -> return (Just a)
--         Nothing  -> cxLookup k xs

-- instance (Monad m) => ContextLookup m (ContextItem m) where
--     cxLookup k (ContextPairs a) = cxLookup k a
--     cxLookup k _ = return Nothing

