{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverlappingInstances, TypeSynonymInstances #-}
module Text.RSTemplate.Eval.Context where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Text.RSTemplate.Eval.Types
import Data.Maybe

-- instance (Monad m) => ContextBinding m [(String,String)] where

-- instance (Monad m) => ContextBinding m [(String,ContextItem m)] where
--     binding k = return . lookup (C.unpack k) 

instance (Monad m) => ContextBinding m [(ByteString,ContextItem m)] where
     binding k = return . fromMaybe ContextNull . lookup k

instance (Monad m) => ContextBinding m String where
    bind = ContextValue . C.pack
    binding _ _ = return ContextNull

instance (Monad m) => ContextBinding m ByteString where
    bind a = ContextValue a
    binding _ _ = return ContextNull

instance (Monad m) => ContextBinding m Bool where
    bind = ContextBool
    binding _ _ = return ContextNull

instance (Monad m,ContextBinding m a) => ContextBinding m [a] where
    bind a  = ContextList (map bind a)
    binding _ _ = return ContextNull

-- instance (Monad m) => ContextBinding m [Context m] where
--     binding k (x:xs) = do
--       x <- (getContext x) k 
--       case x of 
--         Just a -> return (Just a)
--         Nothing  -> binding k xs

-- instance (Monad m) => ContextBinding m (ContextItem m) where
--     binding k (ContextPairs a) = binding k a
--     binding k _ = return Nothing

