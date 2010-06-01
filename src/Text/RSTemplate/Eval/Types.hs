{-# LANGUAGE ExistentialQuantification, 
  TypeSynonymInstances, 
  IncoherentInstances,
  FlexibleInstances, 
  OverlappingInstances, 
  OverloadedStrings,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  FunctionalDependencies,
  UndecidableInstances  #-}

module Text.RSTemplate.Eval.Types where

import qualified Data.ByteString.Char8 as C
import Control.Monad.Writer
import Data.Monoid

data ContextItem a = ContextPairs [a]
                   | ContextValue C.ByteString
                   | ContextList [ContextItem a]
                     deriving (Show,Eq)

instance Monoid (ContextItem a) where
    mappend = (<+>)
    mempty  = ContextList []

data EmptyContext = EmptyContext

class (Monad m) => ContextLookup m a | a -> m where
    cxLookup :: C.ByteString -> a -> m (Maybe (ContextItem a))
    cxLookup _ _ = return Nothing

instance (Monad m) => ContextLookup m EmptyContext where
    cxLookup _ _ = return Nothing

instance (Monad m1,Monad m2) => ContextLookup m1 (CX m2) where
    cxLookup k (CX a) = cxLookup k a

data CX m = forall a. (ContextLookup m a) => CX a

-- simpleContext

mergeCXP (ContextPairs a) (ContextPairs b) = ContextPairs (a ++ b)
mergeCXP (ContextList a) (ContextList b) = ContextList (a ++ b)
(<+>) = mergeCXP

-- emptyContext :: (Monad m) => m (ContextItem EmptyContext)
-- emptyContext = toContext EmptyContext

-- foldCX = foldl (<+>) emptyContext

-- justcx = Just . toContext

justcx :: (Monad m) => C.ByteString -> m (Maybe (ContextItem a))
justcx = return . Just . ContextValue 

-- Context Writer Monad

newtype ContextWriter a b = CW { 
      runContextWriter :: Writer (ContextItem a) b
    } deriving (Monad,MonadWriter (ContextItem a))

execCXW = execWriter . runContextWriter
cxw = execCXW

--set :: ToContext a => String -> a -> ContextWriter a ()
--set k v = tell $ toContext (C.pack k, v)

------------------------------------------------------------------------

data User = User { getName :: String 
                 , getAge  :: Int }
            deriving (Show,Read)

instance (Monad m) => ContextLookup m User where
    cxLookup "name" = justcx . C.pack . getName
    cxLookup "age"  = justcx . C.pack . show . getAge
    cxLookup _      = return . const Nothing

