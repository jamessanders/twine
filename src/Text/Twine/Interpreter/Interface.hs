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

module Text.Twine.Interpreter.Interface (TemplateInterface (..), emptyContext, bind, unbind, method) where

import Control.Monad.Writer
import Data.ByteString.Char8 (ByteString)
import Data.Convertible.Base
import Data.Maybe
import Data.Monoid
import Text.Twine.Interpreter.Types
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M


class (Monad m) => TemplateInterface m a | a -> m where
    property      :: ByteString -> a -> m (TwineElement m)
    makeIterable :: a -> m [TwineElement m]
    makeString   :: a -> m String

    property _ _   = return TwineNull
    makeIterable _ = return []
    makeString   _ = return ""

instance (Monad m, TemplateInterface m a) => Convertible a (TwineElement m) where
  safeConvert a = Right $ TwineObject $ Object {
    getContext  = (flip property a),
    getIterable = makeIterable a,
    getString   = makeString a
    }


------------------------------------------------------------------------

bind :: (Monad m, Convertible a (TwineElement m)) => a -> TwineElement m
bind = convert

unbind :: (Convertible (TwineElement m) a, Monad m) => TwineElement m -> a
unbind = convert

method :: (Monad m) => ([TwineElement m] -> m (TwineElement m)) -> TwineElement m
method = TwineFunction

------------------------------------------------------------------------

instance (Monad m) => Convertible String (TwineElement m) where
  safeConvert = Right . TwineString . C.pack

instance (Monad m) => Convertible ByteString (TwineElement m) where
  safeConvert = Right . TwineString 

instance (Monad m) => Convertible Bool (TwineElement m) where
  safeConvert = Right . TwineBool

instance (Monad m) => Convertible ([TwineElement m] -> m (TwineElement m)) (TwineElement m) where
  safeConvert = Right . TwineFunction

instance (Monad m, Convertible a (TwineElement m)) => Convertible (Maybe a) (TwineElement m) where
  safeConvert (Just a) = Right $ bind a
  safeConvert Nothing  = Right $ TwineNull

instance (Monad m) => TemplateInterface m (TwineElement m) where  
  makeString = return . show

instance (Monad m) => TemplateInterface m EmptyContext 

instance (Monad m, TemplateInterface m a, Convertible a (TwineElement m)) => Convertible [a] (TwineElement m) where
  safeConvert = Right . bind . CXListLike . map bind 

instance (Monad m) => Convertible Int (TwineElement m) where
  safeConvert = Right . bind . CXInteger . fromIntegral
  
instance (Monad m) => Convertible Integer (TwineElement m) where
  safeConvert = Right . bind . CXInteger

-- instance (Monad m) => Convertible (TwineElement m) (TwineElement m) where
--   safeConvert = Right 

instance (Monad m) => TemplateInterface m [(ByteString,TwineElement m)] where
  property k = return . bind . lookup k

instance (Monad m) => TemplateInterface m (M.Map ByteString (TwineElement m)) where
  property k m = 
    case M.lookup k m of
      Just x -> return x
      Nothing -> return TwineNull

instance (Monad m) => TemplateInterface m ByteString where
  makeString = return . C.unpack

------------------------------------------------------------------------
instance (Monad m) => TemplateInterface m Int
  
instance (Monad m) => TemplateInterface m (CXListLike m) where
  property "length" = return . bind . length . unCXListLike
  property "head"   = mbind . head . unCXListLike
  property "tail"   = return . bind . tail . unCXListLike
  property "init"   = return . bind . init . unCXListLike
  property "last"   = mbind . last . unCXListLike
  property "enum"   = \x -> return $ convert [0..(length $ unCXListLike x) - 1]
  property "elem?"  = \x -> return $ method (\[y] -> do
                        return $ bind (y `elem` unCXListLike x)
                      )
  property "item"   = \x -> return $ method (\[n] -> do
                                              i <- unbind n
                                              mbind (unCXListLike x !! i))
                           
  property "take"   = \x -> return $ method (\[n] -> do
                                              i <- unbind n
                                              mbind . take i $ unCXListLike x)
  
  property "drop"   = \x -> return $ method (\[n] -> do
                                              i <- unbind n
                                              mbind . drop i $ unCXListLike x)
  property x = \_ -> error $ C.unpack x ++ " is not a valid property of a list."
  makeIterable = return . unCXListLike
  makeString   = \_-> return "<list>"


instance (Monad m) => TemplateInterface m CXInteger where
  makeString = return . show . unCXInteger
  property "toInteger" = return . TwineInteger . unCXInteger  
  property "even?" = mbind . even . unCXInteger
  property "odd?"  = mbind . odd  . unCXInteger
  property "add"   = \a-> return $ method (\[n]-> do 
                                              i <- unbind n
                                              mbind $ (unCXInteger a + i)
                                          )
  property "subtract" = \a-> return $ method (\[n]-> do 
                                                 i <- unbind n
                                                 mbind $ (unCXInteger a - i)
                                             )

  property "gt?" = \a-> return $ method (\[n]-> do
                                            i <- unbind n
                                            mbind $ (unCXInteger a > i))

mbind :: (Convertible a (TwineElement m), Monad m) => a -> m (TwineElement m)
mbind = return . bind
  
emptyContext :: (Monad m) => TwineElement m
emptyContext = bind EmptyContext

instance (Monad m) => Convertible (TwineElement m) (m Int) where
  safeConvert = Right . cxToInteger
  
instance (Monad m) => Convertible (TwineElement m) (m Integer) where
  safeConvert = Right . cxToInteger

instance (Monad m) => Convertible (TwineElement m) (m Bool) where
  safeConvert = Right . cxToBool
  
instance (Monad m) => Convertible (TwineElement m) (m String) where
  safeConvert = Right . cxToString
  
cxToInteger (TwineInteger i) = 
  return (fromIntegral i)
cxToInteger cm@(TwineObject obj) = 
  signal "toInteger" cm >>= convert

signal :: ByteString -> TwineElement t -> t (TwineElement t)
signal sig (TwineObject obj) = (getContext obj) sig 

cxToBool (TwineBool b) = return b
cxToBool (TwineString "") = return False
cxToBool (TwineNull) = return False
cxToBool (TwineList []) = return False
cxToBool _ = return True

cxToString (TwineString s) = return (C.unpack s)
cxToString (TwineObject c) = getString c 
cxToString (TwineInteger n) = return $ show n
cxToString (TwineBool b) = return $ show b