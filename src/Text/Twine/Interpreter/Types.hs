{-#LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , FlexibleContexts   
  , NoMonomorphismRestriction
  , OverlappingInstances
  , OverloadedStrings
  , UndecidableInstances
  , FunctionalDependencies
  , GeneralizedNewtypeDeriving
 #-}

module Text.Twine.Interpreter.Types where

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Char8 (ByteString)
import Data.Map (Map)
import Data.Convertible.Base
import Data.Monoid

data TwineElement m = TwineObjectList [Object m]
                    | TwineObject (Object m)
                    | TwineString ByteString
                    | TwineInteger Integer
                    | TwineBool Bool
                    | TwineNull
                    | TwineList [TwineElement m]
                    | TwineFunction ([TwineElement m] -> m (TwineElement m))

newtype Context m = Context {
      unContext :: Map ByteString (TwineElement m)
} deriving (Monoid)

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
    (TwineNull) == (TwineNull)           = True
    (TwineBool x) == (TwineBool y)       = x == y
    (TwineInteger x) == (TwineInteger y) = x == y
    (TwineString x) == (TwineString y)   = x == y
    (TwineList  x) == (TwineList y)      = x == y
    _ == _ = error "Unable to determine equality."


newtype CXListLike m = CXListLike { unCXListLike :: [TwineElement m] }
newtype CXInteger    = CXInteger  { unCXInteger  :: Integer }

type BuiltinFunc m = [TwineElement m] -> m (TwineElement m)

data ContextState m = ContextState { 
      getContextState :: (TwineElement m)
    , getContextFuns  :: Map C.ByteString (BuiltinFunc m) 
}

data Object m = Object { 
  getContext    :: ByteString -> m (TwineElement m),
  getIterable   :: m [TwineElement m],
  getString     :: m String
}

data EmptyContext = EmptyContext
