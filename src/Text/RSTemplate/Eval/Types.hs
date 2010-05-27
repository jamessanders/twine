{-# LANGUAGE ExistentialQuantification, 
  TypeSynonymInstances, 
  IncoherentInstances,
  FlexibleInstances, 
  OverlappingInstances, 
  OverloadedStrings,
  GeneralizedNewtypeDeriving,
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

data EvalState = EvalState { getDisplay :: C.ByteString }

class ContextLookup a where
    cxLookup   :: C.ByteString -> a -> Maybe (ContextItem CX)
    ioCxLookup :: C.ByteString -> a -> IO (Maybe (ContextItem CX))

    cxLookup k a   = Nothing
    ioCxLookup k a = return (cxLookup k a)

instance ContextLookup CX where
    cxLookup k (CX a)   = cxLookup k a
    ioCxLookup k (CX a) = ioCxLookup k a

instance ContextLookup a => ContextLookup [a] where
    cxLookup k []     = Nothing
    cxLookup k (x:xs) = case cxLookup k x of
                          Just a  -> Just a
                          Nothing -> cxLookup k xs

    ioCxLookup k []     = return $ Nothing
    ioCxLookup k (x:xs) = do look <- ioCxLookup k x
                             case look of
                               Just a  -> return $ Just a
                               Nothing -> ioCxLookup k xs
    
instance ContextLookup (ContextItem CX) where
    cxLookup k (ContextPairs a)   = cxLookup k a
    ioCxLookup k (ContextPairs a) = ioCxLookup k a


instance ContextLookup (C.ByteString,ContextItem CX) where
    cxLookup k a | k == fst a = Just (snd a)
                 | otherwise = Nothing

 
instance ContextLookup [(C.ByteString,ContextItem CX)] where
    cxLookup k a   = lookup k a
    ioCxLookup k a = return (cxLookup k a)

instance ContextLookup EmptyContext where
    cxLookup _ _ = Nothing

data CX = forall a. (ContextLookup a) => CX a
instance Show CX where
    show _ = "!CX!"
instance Eq CX where
    a == b = False

class ToContext a where
    toContext :: a -> ContextItem CX

instance ToContext C.ByteString where
    toContext s = ContextValue s

instance ToContext String where
    toContext s = toContext (C.pack s)

instance ContextLookup a => ToContext a where
    toContext x = ContextPairs [CX x]

instance ToContext a => ToContext [a] where
    toContext x = ContextList $ map toContext x

instance ToContext a => ToContext (C.ByteString,a) where
      toContext (k,v) = ContextPairs [CX [(k,toContext v)]]

instance ToContext a => ToContext [(C.ByteString,a)] where
      toContext ls = foldl (<+>) (ContextPairs []) $ map toContext ls


context :: (ToContext a) => a -> ContextItem CX
context = toContext

--simpleContext

mergeCXP (ContextPairs a) (ContextPairs b) = ContextPairs (a ++ b)
mergeCXP (ContextList a) (ContextList b) = ContextList (a ++ b)
(<+>) = mergeCXP
emptyContext = toContext EmptyContext

foldCX = foldl (<+>) emptyContext

justcx :: (ToContext a) => a -> Maybe (ContextItem CX)
justcx = Just . toContext

-- Context Writer Monad

newtype ContextWriter a b = CW { 
      runContextWriter :: Writer (ContextItem a) b
    } deriving (Monad,MonadWriter (ContextItem a))

execCXW = execWriter . runContextWriter
cxw = execCXW

set :: ToContext a => String -> a -> ContextWriter CX ()
set k v = tell $ context (C.pack k, v)