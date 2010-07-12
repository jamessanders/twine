{-#LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , UndecidableInstances
  , FlexibleContexts
  , OverlappingInstances
  , OverloadedStrings
  , NoMonomorphismRestriction
 #-}

module Text.RSTemplate.Eval.Types where

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Char8 (ByteString,pack)
import Control.Monad.Writer
import Data.Monoid
import Control.Monad.Identity 
import Control.Monad.Trans
import qualified Data.Map as M

data ContextItem m = ContextPairs [Context m]
                   | ContextValue ByteString
                   | ContextBool Bool
                   | ContextNull
                   | ContextList [ContextItem m]
                   | ContextFunction ([ContextItem m] -> m (ContextItem m))

type BuiltinFunc m = [ContextItem m] -> m (ContextItem m)

data ContextState m = ContextState { getContextState :: (ContextItem m)
                                   , getContextFuns  :: M.Map C.ByteString (BuiltinFunc m) }


instance (Monad m) => Eq (ContextItem m) where
    (ContextValue x) == (ContextValue y) = x == y
    (ContextList  x) == (ContextList y)  = x == y
    _ == _ = error "Unable to determine equality."

instance (Monad m) => Show (ContextItem m) where
    show (ContextValue x) = C.unpack x
    show (ContextPairs _) = "((ContextMap))"
    show (ContextList  _) = "((ContextList))"
    show (ContextBool x)  = show x
    show (ContextNull)    = ""
    show (ContextFunction _) = "((ContextFunction))"

data EmptyContext = EmptyContext

newtype Context m = Context { getContext :: ByteString -> m (ContextItem m) }

class (Monad m) => ContextLookup m a where
    cxLookup  :: ByteString -> a -> m (ContextItem m)
    toContext :: a -> ContextItem m

    toContext a = ContextPairs [Context (flip cxLookup a)]

instance (Monad m) => ContextLookup m (ContextItem m) where
    cxLookup _ _ = return ContextNull
    toContext = id

instance (Monad m) => ContextLookup m EmptyContext where
    cxLookup _ _ = return ContextNull


-- simpleContext

foldCX :: (Monad m) => [ContextItem m] -> ContextItem m
foldCX = foldl (<+>) emptyContext

justcx :: (Monad m, ContextLookup m a) => a -> ContextItem m
justcx = toContext


-- Context Writer Monad --

mergeCXP (ContextPairs a) (ContextPairs b) = ContextPairs (a ++ b)
mergeCXP (ContextList a) (ContextList b) = ContextList (a ++ b)
mergeCXP a b = error $ "Cannot merge " ++ show a ++ " and " ++ show b
(<+>) = mergeCXP

emptyContext :: (Monad m) => ContextItem m
emptyContext = toContext EmptyContext

instance (Monad m) => Monoid (ContextItem m) where
    mappend = (<+>)
    mempty  = emptyContext

type ContextWriter m = WriterT (ContextItem m) m () 


execCXW,cxw :: (Monad m) => ContextWriter m -> m (ContextItem m) 
execCXW = execWriterT 
cxw     = execCXW

set k v = tell $ ContextPairs [Context aux]
    where aux x = if x == (C.pack k) 
                    then return . justcx $ v 
                    else return ContextNull


with k fn = do cx <- lift $ execCXW fn
               set "page" cx

