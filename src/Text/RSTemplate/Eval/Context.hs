{-#LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , FlexibleContexts   
  , NoMonomorphismRestriction
  , OverlappingInstances
  , OverloadedStrings
  , UndecidableInstances
 #-}

module Text.RSTemplate.Eval.Context where

import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Monoid
import Text.RSTemplate.Eval.Types
import qualified Data.ByteString.Char8 as C
import Control.Monad.Writer
import qualified Data.Map as M

instance (Monad m) => ContextBinding m (ContextItem m) where
  bind = id
  makeString = return . show

instance (Monad m) => ContextBinding m ([ContextItem m] -> m (ContextItem m)) where
  bind = ContextFunction

instance (Monad m) => ContextBinding m EmptyContext 

instance (Monad m, ContextBinding m a) => ContextBinding m (Maybe a) where
  bind (Just a)  = bind a
  bind Nothing   = ContextNull

instance (Monad m) => ContextBinding m [(ByteString,ContextItem m)] where
  binding k = return . fromMaybe ContextNull . lookup k

instance (Monad m) => ContextBinding m String where
  bind = ContextValue . C.pack

instance (Monad m) => ContextBinding m ByteString where
  bind a = ContextValue a

instance (Monad m) => ContextBinding m Bool where
  bind = ContextBool

instance (Monad m) => ContextBinding m (M.Map ByteString (ContextItem m)) where
  binding k = return . fromMaybe (ContextNull) . M.lookup k

emptyContext :: (Monad m) => ContextItem m
emptyContext = bind EmptyContext

-- simpleContext

foldCX :: (Monad m) => [ContextItem m] -> ContextItem m
foldCX = foldl (<+>) emptyContext

-- Context Writer Monad --

mergeCXP (ContextPairs a) (ContextPairs b) = ContextPairs (a ++ b)
mergeCXP (ContextList a)  (ContextList b)  = ContextList (a ++ b)
mergeCXP (ContextMap a)   x   = ContextPairs [a] `mergeCXP` x
mergeCXP x (ContextMap a)   = x `mergeCXP` ContextPairs [a]
mergeCXP a b = error $ "Cannot merge " ++ show a ++ " and " ++ show b
(<+>) = mergeCXP
