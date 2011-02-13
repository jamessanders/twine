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
                   | ContextInteger Integer
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

data Context m = Context { 
  getContext :: ByteString -> m (ContextItem m),
  getIterable :: m [ContextItem m]
  }

class (Monad m) => ContextBinding m a where
    binding  :: ByteString -> a -> m (ContextItem m)
    makeIterable :: a -> m [ContextItem m]
    bind :: a -> ContextItem m

    makeIterable _ = return [ContextValue "((Not Iterable))"]
    bind a = ContextPairs $ [Context {
      getContext  = (flip binding a),
      getIterable = makeIterable a
      }]
                   

instance (Monad m) => ContextBinding m ([ContextItem m] -> m (ContextItem m)) where
  bind = ContextFunction

instance (Monad m) => ContextBinding m (ContextItem m) where
    binding _ _ = return ContextNull
    bind = id

instance (Monad m) => ContextBinding m EmptyContext where
    binding _ _ = return ContextNull

instance (Monad m, ContextBinding m a) => ContextBinding m (Maybe a) where
    bind (Just a)  = bind a
    bind Nothing   = ContextNull
    binding = undefined


-- simpleContext

foldCX :: (Monad m) => [ContextItem m] -> ContextItem m
foldCX = foldl (<+>) emptyContext

justcx :: (Monad m, ContextBinding m a) => a -> ContextItem m
justcx = bind


-- Context Writer Monad --

mergeCXP (ContextPairs a) (ContextPairs b) = ContextPairs (a ++ b)
mergeCXP (ContextList a) (ContextList b) = ContextList (a ++ b)
mergeCXP a b = error $ "Cannot merge " ++ show a ++ " and " ++ show b
(<+>) = mergeCXP

emptyContext :: (Monad m) => ContextItem m
emptyContext = bind EmptyContext

instance (Monad m) => Monoid (ContextItem m) where
    mappend = (<+>)
    mempty  = emptyContext

type ContextWriter m = WriterT (ContextItem m) m () 

makeContext :: (Monad m) => ContextWriter m -> m (ContextItem m) 
makeContext = execWriterT 

set k v = tell $ ContextPairs [Context { getContext = aux, getIterable = undefined }]
    where aux x = if x == (C.pack k) 
                    then return . justcx $ v 
                    else return ContextNull

