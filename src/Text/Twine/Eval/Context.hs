{-#LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , FlexibleContexts   
  , NoMonomorphismRestriction
  , OverlappingInstances
  , OverloadedStrings
  , UndecidableInstances
  , FunctionalDependencies
 #-}

module Text.Twine.Eval.Context (emptyContext, ContextBinding (..)) where

import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Monoid
import Text.Twine.Eval.Types
import qualified Data.ByteString.Char8 as C
import Control.Monad.Writer
import qualified Data.Map as M

class (Monad m) => ContextBinding m a | a -> m where
    binding      :: ByteString -> a -> m (TwineElement m)
    makeIterable :: a -> m [TwineElement m]
    makeString   :: a -> m String
    bind         :: (ContextBinding m a) => a -> TwineElement m

    binding _ _ = return ContextNull
    makeIterable _ = return []
    makeString   _ = return ""
    bind a = ContextMap $ Context {
      getContext  = (flip binding a),
      getIterable = makeIterable a,
      getString   = makeString a
      }


instance (Monad m) => ContextBinding m (TwineElement m) where
  bind = id
  makeString = return . show

instance (Monad m) => ContextBinding m ([TwineElement m] -> m (TwineElement m)) where
  bind = ContextFunction

instance (Monad m) => ContextBinding m EmptyContext 

instance (Monad m, ContextBinding m a) => ContextBinding m (Maybe a) where
  bind (Just a)  = bind a
  bind Nothing   = ContextNull

instance (Monad m) => ContextBinding m [(ByteString,TwineElement m)] where
  binding k = return . fromMaybe ContextNull . lookup k

instance (Monad m) => ContextBinding m String where
  bind = ContextValue . C.pack

instance (Monad m) => ContextBinding m ByteString where
  bind a = ContextValue a

instance (Monad m) => ContextBinding m Bool where
  bind = ContextBool

instance (Monad m) => ContextBinding m (M.Map ByteString (TwineElement m)) where
  binding k = return . fromMaybe (ContextNull) . M.lookup k

emptyContext :: (Monad m) => TwineElement m
emptyContext = bind EmptyContext

