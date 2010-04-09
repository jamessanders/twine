{-# LANGUAGE ExistentialQuantification, 
  TypeSynonymInstances, 
  FlexibleInstances, 
  OverlappingInstances, 
  UndecidableInstances  #-}

module Text.RSTemplate.EvalIO.Types where

import Text.Rstemplate.Eval.Types

data IOCX = forall a. (IOContextLookup a) => IOCX a

instance Show IOCX where
    show _ = "<IOCX>"

class IOContextLookup a where 
    ioCxLookup :: String -> a -> IO (Maybe (ContextItem IOCX))

instance ContextLookup a => IOContextLookup a where
    ioCxLookup k v = case (cxLookup k v) of
                       Just y -> return $ Just (toIOContext y)
                       Nothing-> return Nothing

instance IOContextLookup (String,ContextItem IOCX) where
    ioCxLookup k (v1,v2) = return (if v1 == k then Just v2 else Nothing)

instance IOContextLookup [(String,ContextItem IOCX)] where
    ioCxLookup k a = return (lookup k a)


instance IOContextLookup a => IOContextLookup [a] where
    ioCxLookup k []     = return Nothing
    ioCxLookup k (x:xs) = do l <- ioCxLookup k x
                             case l of
                               Just a  -> return (Just a)
                               Nothing -> ioCxLookup k xs


instance IOContextLookup IOCX where
    ioCxLookup k (IOCX a) = ioCxLookup k a

instance IOContextLookup (ContextItem IOCX) where
    ioCxLookup k (ContextPairs v) = ioCxLookup k v
    ioCxLookup k x = return (Just x)

------------------------------------------------------------------------

class ToIOContext a where
    toIOContext :: a -> ContextItem IOCX

instance ToIOContext (ContextItem CX) where
    toIOContext (ContextPairs a) = ContextPairs (map (\(CX x)->IOCX x) a)
    toIOContext (ContextValue a) = ContextValue a
    toIOContext (ContextList  a) = ContextList (map toIOContext a)

instance IOContextLookup a => ToIOContext a where
     toIOContext x = ContextPairs [IOCX x]

instance ToIOContext a => ToIOContext (String,a) where
    toIOContext (k,v) = toIOContext [(k,toIOContext v)]

liftCX a = toIOContext $ toContext a

