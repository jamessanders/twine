{-#LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , FlexibleContexts   
  , NoMonomorphismRestriction
  , OverlappingInstances
  , OverloadedStrings
  , UndecidableInstances
 #-}
module Text.Twine.Interpreter.InternalInterfaces (emptyContext) where

import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Text.Twine.Interpreter.Interface
import Text.Twine.Interpreter.Types
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M

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
  property k = return . fromMaybe TwineNull . lookup k

instance (Monad m) => TemplateInterface m String where
  bind = TwineString . C.pack

instance (Monad m) => TemplateInterface m ByteString where
  bind a = TwineString a

instance (Monad m) => TemplateInterface m Bool where
  bind = TwineBool

instance (Monad m) => TemplateInterface m (M.Map ByteString (TwineElement m)) where
  property k = return . fromMaybe (TwineNull) . M.lookup k

------------------------------------------------------------------------
  
instance (TemplateInterface m a) => TemplateInterface m [a] where
  bind = bind . CXListLike . map bind 

instance (Monad m) => TemplateInterface m Int where
  bind = bind . CXInteger . fromIntegral
  
instance (Monad m) => TemplateInterface m Integer where
  bind = bind . CXInteger
 
  
instance (Monad m) => TemplateInterface m (CXListLike m) where
  property "length" = mbind . length . unCXListLike
  property "head"   = mbind . head . unCXListLike
  property "tail"   = mbind . tail . unCXListLike
  property "init"   = mbind . init . unCXListLike
  property "last"   = mbind . last . unCXListLike
  property "item"   = \x -> return $ TwineFunction (\[n] -> do
                                                       i <- cxToInteger n
                                                       mbind (unCXListLike x !! fromIntegral i))
                           
  property "take"   = \x -> return $ TwineFunction (\[n] -> do
                                                        i <- cxToInteger n
                                                        mbind . take (fromIntegral i) $ unCXListLike x)
  
  property "drop"   = \x -> return $ TwineFunction (\[n] -> do
                                                        i <- cxToInteger n
                                                        mbind . drop (fromIntegral i) $ unCXListLike x)
  
  makeIterable = return . unCXListLike
  makeString   = \_-> return "<list>"


instance (Monad m) => TemplateInterface m CXInteger where
  makeString = return . show . unCXInteger
  property "toInteger" = return . TwineInteger . unCXInteger  
  property "even?" = mbind . even . unCXInteger
  property "odd?"  = mbind . odd  . unCXInteger
  property "add"   = \a-> return $ TwineFunction (\[n]-> do 
                                                      i <- cxToInteger n
                                                      mbind $ (unCXInteger a + i)
                                                  )
  property "subtract" = \a-> return $ TwineFunction (\[n]-> do 
                                                      i <- cxToInteger n
                                                      mbind $ (unCXInteger a - i)
                                                     )

mbind = return . bind
  
signal :: ByteString -> TwineElement t -> t (TwineElement t)
signal sig (TwineObject obj) = (getContext obj) sig 

cxToInteger (TwineInteger i) = return i
cxToInteger cm@(TwineObject obj) = do
  v <- signal "toInteger" cm
  case v of
    TwineInteger i -> return i
    TwineNull -> error "expected number but got null value"
    _ -> error ("'" ++ show v ++ "' is not a number")


emptyContext :: (Monad m) => TwineElement m
emptyContext = bind EmptyContext
