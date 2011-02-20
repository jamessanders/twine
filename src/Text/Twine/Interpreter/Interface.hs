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

module Text.Twine.Interpreter.Interface (TemplateInterface (..)) where

import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Monoid
import Text.Twine.Interpreter.Types
import qualified Data.ByteString.Char8 as C
import Control.Monad.Writer
import qualified Data.Map as M

class (Monad m) => TemplateInterface m a | a -> m where
    property      :: ByteString -> a -> m (TwineElement m)
    makeIterable :: a -> m [TwineElement m]
    makeString   :: a -> m String
    bind         :: (TemplateInterface m a) => a -> TwineElement m

    property _ _ = return TwineNull
    makeIterable _ = return []
    makeString   _ = return ""
    bind a = TwineObject $ Context {
      getContext  = (flip property a),
      getIterable = makeIterable a,
      getString   = makeString a
      }


