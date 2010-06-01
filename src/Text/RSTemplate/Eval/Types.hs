{-#LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , ExistentialQuantification
  , GeneralizedNewtypeDeriving 
  , OverloadedStrings #-}

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

class (Monad m) => ContextLookup m a  where
    cxLookup :: C.ByteString -> a -> m (Maybe (ContextItem (CX m)))
    cxLookup _ _ = return Nothing

instance (Monad m) => ContextLookup m EmptyContext where
    cxLookup _ _ = return Nothing

instance (Monad m) => ContextLookup m (CX m) where
    cxLookup k (CX a) = cxLookup k a

data CX m = forall a. (ContextLookup m a) => CX a
instance (Monad m) => Show (CX m) where
    show = const "!CX!"
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

