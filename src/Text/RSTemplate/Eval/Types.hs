{-#LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , UndecidableInstances
  , FlexibleContexts
  , OverlappingInstances
  , OverloadedStrings
 #-}

module Text.RSTemplate.Eval.Types where

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Char8 (ByteString)
import Control.Monad.Writer
import Data.Monoid
import Control.Monad.Identity 

data ContextItem m = ContextPairs [Context m]
                   | ContextValue ByteString
                   | ContextBool Bool
                   | ContextList [ContextItem m]

instance (Monad m) => Eq (ContextItem m) where
    (ContextValue x) == (ContextValue y) = x == y
    (ContextList  x) == (ContextList y)  = x == y
    _ == _ = error "Unable to determine equality."

instance (Monad m) => Show (ContextItem m) where
    show (ContextValue x) = C.unpack x
    show (ContextPairs _) = "((ContextMap))"
    show (ContextList  _) = "((ContextList))"
    show (ContextBool x)  = show x

data EmptyContext = EmptyContext

newtype Context m = Context { getContext :: ByteString -> m (Maybe (ContextItem m)) }

class (Monad m) => ContextLookup m a where
    cxLookup  :: ByteString -> a -> m (Maybe (ContextItem m))
    toContext :: a -> ContextItem m

    toContext a = ContextPairs [Context (flip cxLookup a)]

instance (Monad m) => ContextLookup m EmptyContext where
    cxLookup _ _ = return Nothing

instance (Monad m) => ContextLookup m ByteString where
    cxLookup _ _ = return Nothing
    toContext a = ContextValue a

instance (Monad m) => ContextLookup m Bool where
    toContext = ContextBool

instance (Monad m) => ContextLookup m [(String,String)] where
    cxLookup k = return . fmap (ContextValue . C.pack) . lookup (C.unpack k)

instance (Monad m) => ContextLookup m [(String,ContextItem m)] where
    cxLookup k = return . lookup (C.unpack k) 

instance (Monad m) => ContextLookup m [(ByteString,ContextItem m)] where
    cxLookup k = return . lookup k

instance (Monad m) => ContextLookup m String where
    toContext = ContextValue . C.pack
    cxLookup _ _ = return Nothing

instance (Monad m,ContextLookup m a) => ContextLookup m [a] where
    cxLookup _ _ = return Nothing
    toContext a  = ContextList (map toContext a)

instance (Monad m) => ContextLookup m [Context m] where
    cxLookup k (x:xs) = do
      x <- (getContext x) k 
      case x of 
        Just a -> return (Just a)
        Nothing  -> cxLookup k xs

instance (Monad m) => ContextLookup m (ContextItem m) where
    cxLookup k (ContextPairs a) = cxLookup k a
    cxLookup k _ = return Nothing


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

type ContextWriter m a = Writer (ContextItem m) a

execCXW = execWriter 
cxw     = execCXW
set k v = tell $ ContextPairs [Context aux]
    where aux x = if x == (C.pack k) then return . justcx $ v else return Nothing

