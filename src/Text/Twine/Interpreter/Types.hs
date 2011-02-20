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

module Text.Twine.Interpreter.Types where

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Char8 (ByteString)
import qualified Data.Map as M
import Data.Convertible.Base

data TwineElement m = TwineObjectList [Context m]
                    | TwineObject (Context m)
                    | TwineString ByteString
                    | TwineInteger Integer
                    | TwineBool Bool
                    | TwineNull
                    | TwineList [TwineElement m]
                    | TwineFunction ([TwineElement m] -> m (TwineElement m))

instance (Monad m) => Show (TwineElement m) where
    show (TwineString x) = C.unpack x
    show (TwineObjectList _) = "((TwineObject))"
    show (TwineObject _)   = "((TwineObject))"
    show (TwineList  _) = "((TwineList))"
    show (TwineBool x)  = show x
    show (TwineNull)    = ""
    show (TwineFunction _) = "((TwineFunction))"
    show _ = ""

instance (Monad m) => Eq (TwineElement m) where
    (TwineString x) == (TwineString y) = x == y
    (TwineList  x) == (TwineList y)  = x == y
    _ == _ = error "Unable to determine equality."


newtype CXListLike m = CXListLike { unCXListLike :: [TwineElement m] }
newtype CXInteger    = CXInteger  { unCXInteger  :: Integer }

type BuiltinFunc m = [TwineElement m] -> m (TwineElement m)

data ContextState m = ContextState { getContextState :: (TwineElement m)
                                   , getContextFuns  :: M.Map C.ByteString (BuiltinFunc m) }

data Context m = Context { 
  getContext    :: ByteString -> m (TwineElement m),
  getIterable   :: m [TwineElement m],
  getString     :: m String
}


data EmptyContext = EmptyContext

