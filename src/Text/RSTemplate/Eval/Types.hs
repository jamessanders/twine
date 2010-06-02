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
import Control.Monad.Writer
import Data.Monoid
import Control.Monad.Identity 

data ContextItem m = ContextPairs [Context m]
                   | ContextValue (Context m)
                   | ContextList [ContextItem m]

instance (Monad m) => Show (ContextItem m) where
    show (ContextPairs _) = "< ContextPairs >"
    show (ContextValue (Final x)) = C.unpack x
    show (ContextList  x) = "< ContextList >"

instance Monoid (ContextItem a) where
    mappend = (<+>)
    mempty  = ContextList []

data EmptyContext = EmptyContext


-- TODO: Just some notes, delete this later.

-- data User = User { getUserName :: String, getUserAge :: Int }
-- data Pet  = Pet  { getPetName :: String, getPetWeight :: Int }

-- data Context m = Context { cxLookup :: String -> m (Maybe String) }

-- class ContextLookup m a where 
--     mkCX :: String -> a -> m (Maybe String)
--     cx :: a -> Context m
--     cx a = Context (flip mkCX a)

-- instance (Monad m) => ContextLookup m User where
--     mkCX "name" = return . Just . getUserName

-- instance (Monad m) => ContextLookup m Pet where
--     mkCX "name" = return . Just . getPetName 

data Context m = Context { lookupInContext :: C.ByteString -> m (Maybe (ContextItem m)) }
               | Final C.ByteString

class (Monad m) => ContextLookup m a  where
    cxLookup  :: C.ByteString -> a -> m (Maybe (ContextItem m))
    toContext :: a -> Context m
    toContext a = Context (flip cxLookup a)

instance (Monad m) => ContextLookup m EmptyContext where
    cxLookup _ _ = return Nothing

instance (Monad m) => ContextLookup m C.ByteString where
    cxLookup _ _ = return Nothing
    toContext a = Final a

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

emptyContext :: (Monad m) => Context m
emptyContext = toContext EmptyContext

--foldCX = foldl (<+>) emptyContext

--justcx :: (Monad m) => m (ContextItem m)
--justcx = return . Just . ContextValue . toContext


-- Context Writer Monad

-- newtype ContextWriter a b = CW { 
--       runContextWriter :: Writer ContextItem b
--     } deriving (Monad,MonadWriter ContextItem)

-- execCXW = execWriter . runContextWriter
-- cxw = execCXW

--set :: ToContext a => String -> a -> ContextWriter a ()
--set k v = tell $ toContext (C.pack k, v)

------------------------------------------------------------------------

data User = User { getName :: String 
                 , getAge  :: Int }
            deriving (Show,Read)

instance (Monad m) => ContextLookup m User where
    cxLookup "name" = return . Just . ContextValue . Final . C.pack . getName
    cxLookup "age"  = return . Just . ContextValue . Final . C.pack . show . getAge
    cxLookup _      = return . const Nothing

