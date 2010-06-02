{-#LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , ExistentialQuantification
  , GeneralizedNewtypeDeriving 
  , UndecidableInstances
  , FunctionalDependencies
  , FlexibleContexts
  , OverloadedStrings #-}

module Text.RSTemplate.Eval.Types where

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Char8 (ByteString)
import Control.Monad.Writer
import Data.Monoid
import Control.Monad.Identity 

data ContextItem m = ContextPairs [Context m]
                   | ContextValue ByteString
                   | ContextList [ContextItem m]

instance (Monad m) => Show (ContextItem m) where
    show (ContextPairs _) = "< ContextPairs >"
    show (ContextValue x) = C.unpack x
    show (ContextList  x) = "< ContextList >"

instance Monoid (ContextItem a) where
    mappend = (<+>)
    mempty  = ContextList []

data EmptyContext = EmptyContext

newtype Context m = Context { lookupInContext :: ByteString -> m (Maybe (ContextItem m)) }

class (Monad m) => ContextLookup m a  where
    cxLookup  :: ByteString -> a -> m (Maybe (ContextItem m))
    toContext :: a -> ContextItem m
    toContext a = ContextPairs [Context (flip cxLookup a)]

instance (Monad m) => ContextLookup m EmptyContext where
    cxLookup _ _ = return Nothing

instance (Monad m) => ContextLookup m ByteString where
    cxLookup _ _ = return Nothing
    toContext a = ContextValue a

doLookup _ (ContextPairs []) = return Nothing
doLookup k (ContextPairs (x:xs)) = do
  s <- cxLookup k x
  case s of
    Just x  -> return $ Just x
    Nothing -> doLookup k (ContextPairs xs)
               
-- simpleContext

mergeCXP (ContextPairs a) (ContextPairs b) = ContextPairs (a ++ b)
mergeCXP (ContextList a) (ContextList b) = ContextList (a ++ b)
(<+>) = mergeCXP

emptyContext :: (Monad m) => ContextItem m
emptyContext = toContext EmptyContext

foldCX :: (Monad m) => [ContextItem m] -> ContextItem m
foldCX = foldl (<+>) emptyContext

justcx :: (Monad m, ContextLookup m a) => a -> m (Maybe (ContextItem m))
justcx = return . Just . toContext


-- Context Writer Monad

newtype ContextWriter m a = CW { 
      runContextWriter :: Writer (ContextItem m) a
    } deriving (Monad,MonadWriter (ContextItem m))

execCXW = execWriter . runContextWriter
cxw = execCXW

set :: (MonadWriter (ContextItem m1) m,
        ContextLookup m1 (ByteString, t)) =>
       String -> t -> m ()
set k v = tell $ toContext (C.pack k, v)

------------------------------------------------------------------------

data User = User { getName :: String 
                 , getAge  :: Int }
            deriving (Show,Read)

instance (Monad m) => ContextLookup m User where
    cxLookup "name" = justcx . C.pack . getName
    cxLookup "age"  = justcx . C.pack . show . getAge
    cxLookup _      = return . const Nothing

