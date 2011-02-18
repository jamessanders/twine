{-#LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , FlexibleContexts   
  , NoMonomorphismRestriction
  , OverlappingInstances
  , OverloadedStrings
  , UndecidableInstances
 #-}


module Text.Twine.Eval.FancyContext where
import Text.Twine.Eval.Types
import Text.Twine.Eval.Context

instance (ContextBinding m a) => ContextBinding m [a] where
  bind = bind . CXListLike . map bind 

instance (Monad m) => ContextBinding m (CXListLike m) where
  binding "length" = mbind . length . unCXListLike
  binding "head"   = mbind . head . unCXListLike
  binding "tail"   = mbind . tail . unCXListLike
  binding "init"   = mbind . init . unCXListLike
  binding "last"   = mbind . last . unCXListLike
  binding "item"   = \x -> return $ ContextFunction (\[n] -> do
                                                       i <- cxToInteger n
                                                       mbind (unCXListLike x !! fromIntegral i))
                           
  binding "take"   = \x -> return $ ContextFunction (\[n] -> do
                                                        i <- cxToInteger n
                                                        mbind . take (fromIntegral i) $ unCXListLike x)
  
  binding "drop"   = \x -> return $ ContextFunction (\[n] -> do
                                                        i <- cxToInteger n
                                                        mbind . drop (fromIntegral i) $ unCXListLike x)
  
  makeIterable = return . unCXListLike
  makeString   = \_-> return "<list>"

------------------------------------------------------------------------

instance (Monad m) => ContextBinding m Int where
  bind = bind . CXInteger . fromIntegral
  
instance (Monad m) => ContextBinding m Integer where
  bind = bind . CXInteger

instance (Monad m) => ContextBinding m CXInteger where
  makeString = return . show . unCXInteger
  binding "toInteger" = return . ContextInteger . unCXInteger  
  binding "even?" = mbind . even . unCXInteger
  binding "odd?"  = mbind . odd  . unCXInteger
  binding "add"   = \a-> return $ ContextFunction (\[n]-> do 
                                                      i <- cxToInteger n
                                                      mbind $ (unCXInteger a + i)
                                                  )
  binding "subtract" = \a-> return $ ContextFunction (\[n]-> do 
                                                      i <- cxToInteger n
                                                      mbind $ (unCXInteger a - i)
                                                     )

mbind = return . bind
  
signal sig (ContextMap obj) = (getContext obj) sig 

cxToInteger (ContextInteger i) = return i
cxToInteger cm@(ContextMap obj) = do
  v <- signal "toInteger" cm
  case v of
    ContextInteger i -> return i
    ContextNull -> error "expected number but got null value"
    _ -> error ("'" ++ show v ++ "' is not a number")

  
  