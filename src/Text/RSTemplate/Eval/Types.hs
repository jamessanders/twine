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

instance (Monad m) => Eq (ContextItem m) where
    (ContextValue x) == (ContextValue y) = x == y
    (ContextList  x) == (ContextList y)  = x == y
    _ == _ = error "Unable to determine equality."

instance (Monad m) => Show (ContextItem m) where
    show (ContextPairs _) = "<ContextPairs>"
    show (ContextValue x) = C.unpack x
    show (ContextList  _) = "<ContextList>"

data EmptyContext = EmptyContext

newtype Context m = Context { getContext :: ByteString -> m (Maybe (ContextItem m)) }

class (Monad m) => ContextLookup m a  where
    cxLookup  :: ByteString -> a -> m (Maybe (ContextItem m))
    toContext :: a -> ContextItem m
    toContext a = ContextPairs [Context (flip cxLookup a)]

instance (Monad m) => ContextLookup m EmptyContext where
    cxLookup _ _ = return Nothing

instance (Monad m) => ContextLookup m ByteString where
    cxLookup _ _ = return Nothing
    toContext a = ContextValue a

-- simpleContext

foldCX :: (Monad m) => [ContextItem m] -> ContextItem m
foldCX = foldl (<+>) emptyContext

justcx :: (Monad m, ContextLookup m a) => a -> (Maybe (ContextItem m))
justcx = Just . toContext


-- Context Writer Monad --

mergeCXP (ContextPairs a) (ContextPairs b) = ContextPairs (a ++ b)
mergeCXP (ContextList a) (ContextList b) = ContextList (a ++ b)
(<+>) = mergeCXP

emptyContext :: (Monad m) => ContextItem m
emptyContext = toContext EmptyContext

instance Monoid (ContextItem a) where
    mappend = (<+>)
    mempty  = ContextList []

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

-- data User = User { getName :: String 
--                  , getAge  :: Int }
--             deriving (Show,Read)

-- instance (Monad m) => ContextLookup m User where
--     cxLookup "name" = return . justcx . C.pack . getName
--     cxLookup "age"  = return . justcx . C.pack . show . getAge
--     cxLookup _      = return . const Nothing

instance (Monad m) => ContextLookup m [(String,String)] where
    cxLookup k = return . fmap (ContextValue . C.pack) . lookup (C.unpack k)

instance (Monad m) => ContextLookup m [(String,ContextItem m)] where
    cxLookup k = return . lookup (C.unpack k) 

                     