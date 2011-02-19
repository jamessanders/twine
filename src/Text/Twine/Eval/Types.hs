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

data TwineElement m = TwineObjectList [Context m]
                    | TwineObject (Context m)
                    | TwineString ByteString
                    | TwineInteger Integer
                    | TwineBool Bool
                    | TwineNull
                    | TwineList [TwineElement m]
                    | TwineFunction ([TwineElement m] -> m (TwineElement m))

newtype CXListLike m = CXListLike { unCXListLike :: [TwineElement m] }
newtype CXInteger    = CXInteger  { unCXInteger  :: Integer }

type BuiltinFunc m = [TwineElement m] -> m (TwineElement m)

data ContextState m = ContextState { getContextState :: (TwineElement m)
                                   , getContextFuns  :: M.Map C.ByteString (BuiltinFunc m) }


instance (Monad m) => Eq (TwineElement m) where
    (TwineString x) == (TwineString y) = x == y
    (TwineList  x) == (TwineList y)  = x == y
    _ == _ = error "Unable to determine equality."

instance (Monad m) => Show (TwineElement m) where
    show (TwineString x) = C.unpack x
    show (TwineObjectList _) = "((TwineObject))"
    show (TwineObject _)   = "((TwineObject))"
    show (TwineList  _) = "((TwineList))"
    show (TwineBool x)  = show x
    show (TwineNull)    = ""
    show (TwineFunction _) = "((TwineFunction))"

data EmptyContext = EmptyContext

data Context m = Context { 
  getContext    :: ByteString -> m (TwineElement m),
  getIterable   :: m [TwineElement m],
  getString     :: m String
}


