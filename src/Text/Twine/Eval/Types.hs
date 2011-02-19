{-#LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , UndecidableInstances
  , FlexibleContexts
  , OverlappingInstances
  , OverloadedStrings
  , NoMonomorphismRestriction
  , FunctionalDependencies
 #-}

module Text.Twine.Eval.Types where

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Char8 (ByteString,pack)
import Control.Monad.Writer
import Control.Monad.Identity 
import Control.Monad.Trans
import qualified Data.Map as M

data TwineElement m = ContextPairs [Context m]
                    | ContextMap (Context m)
                    | ContextValue ByteString
                    | ContextInteger Integer
                    | ContextBool Bool
                    | ContextNull
                    | ContextList [TwineElement m]
                    | ContextFunction ([TwineElement m] -> m (TwineElement m))

newtype CXListLike m = CXListLike { unCXListLike :: [TwineElement m] }
newtype CXInteger    = CXInteger  { unCXInteger  :: Integer }

type BuiltinFunc m = [TwineElement m] -> m (TwineElement m)

data ContextState m = ContextState { getContextState :: (TwineElement m)
                                   , getContextFuns  :: M.Map C.ByteString (BuiltinFunc m) }


instance (Monad m) => Eq (TwineElement m) where
    (ContextValue x) == (ContextValue y) = x == y
    (ContextList  x) == (ContextList y)  = x == y
    _ == _ = error "Unable to determine equality."

instance (Monad m) => Show (TwineElement m) where
    show (ContextValue x) = C.unpack x
    show (ContextPairs _) = "((ContextMap))"
    show (ContextMap _)   = "((ContextMap))"
    show (ContextList  _) = "((ContextList))"
    show (ContextBool x)  = show x
    show (ContextNull)    = ""
    show (ContextFunction _) = "((ContextFunction))"

data EmptyContext = EmptyContext

data Context m = Context { 
  getContext    :: ByteString -> m (TwineElement m),
  getIterable   :: m [TwineElement m],
  getString     :: m String
}


