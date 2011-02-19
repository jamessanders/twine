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

module Text.Twine.Interpreter.Context (emptyContext, TemplateInterface (..)) where

import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Monoid
import Text.Twine.Interpreter.Types
import qualified Data.ByteString.Char8 as C
import Control.Monad.Writer
import qualified Data.Map as M

class (Monad m) => TemplateInterface m a | a -> m where
    binding      :: ByteString -> a -> m (TwineElement m)
    makeIterable :: a -> m [TwineElement m]
    makeString   :: a -> m String
    bind         :: (TemplateInterface m a) => a -> TwineElement m

    binding _ _ = return TwineNull
    makeIterable _ = return []
    makeString   _ = return ""
    bind a = TwineObject $ Context {
      getContext  = (flip binding a),
      getIterable = makeIterable a,
      getString   = makeString a
      }


instance (Monad m) => TemplateInterface m (TwineElement m) where
  bind = id
  makeString = return . show

instance (Monad m) => TemplateInterface m ([TwineElement m] -> m (TwineElement m)) where
  bind = TwineFunction

instance (Monad m) => TemplateInterface m EmptyContext 

instance (Monad m, TemplateInterface m a) => TemplateInterface m (Maybe a) where
  bind (Just a)  = bind a
  bind Nothing   = TwineNull

instance (Monad m) => TemplateInterface m [(ByteString,TwineElement m)] where
  binding k = return . fromMaybe TwineNull . lookup k

instance (Monad m) => TemplateInterface m String where
  bind = TwineString . C.pack

instance (Monad m) => TemplateInterface m ByteString where
  bind a = TwineString a

instance (Monad m) => TemplateInterface m Bool where
  bind = TwineBool

instance (Monad m) => TemplateInterface m (M.Map ByteString (TwineElement m)) where
  binding k = return . fromMaybe (TwineNull) . M.lookup k

emptyContext :: (Monad m) => TwineElement m
emptyContext = bind EmptyContext

