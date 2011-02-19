{-#LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , FlexibleContexts   
  , NoMonomorphismRestriction
  , OverlappingInstances
  , OverloadedStrings
  , UndecidableInstances
 #-}


module Text.Twine.Interpreter.FancyContext where
import Text.Twine.Interpreter.Types
import Text.Twine.Interpreter.Context

instance (TemplateInterface m a) => TemplateInterface m [a] where
  bind = bind . CXListLike . map bind 

instance (Monad m) => TemplateInterface m (CXListLike m) where
  binding "length" = mbind . length . unCXListLike
  binding "head"   = mbind . head . unCXListLike
  binding "tail"   = mbind . tail . unCXListLike
  binding "init"   = mbind . init . unCXListLike
  binding "last"   = mbind . last . unCXListLike
  binding "item"   = \x -> return $ TwineFunction (\[n] -> do
                                                       i <- cxToInteger n
                                                       mbind (unCXListLike x !! fromIntegral i))
                           
  binding "take"   = \x -> return $ TwineFunction (\[n] -> do
                                                        i <- cxToInteger n
                                                        mbind . take (fromIntegral i) $ unCXListLike x)
  
  binding "drop"   = \x -> return $ TwineFunction (\[n] -> do
                                                        i <- cxToInteger n
                                                        mbind . drop (fromIntegral i) $ unCXListLike x)
  
  makeIterable = return . unCXListLike
  makeString   = \_-> return "<list>"

------------------------------------------------------------------------

instance (Monad m) => TemplateInterface m Int where
  bind = bind . CXInteger . fromIntegral
  
instance (Monad m) => TemplateInterface m Integer where
  bind = bind . CXInteger

instance (Monad m) => TemplateInterface m CXInteger where
  makeString = return . show . unCXInteger
  binding "toInteger" = return . TwineInteger . unCXInteger  
  binding "even?" = mbind . even . unCXInteger
  binding "odd?"  = mbind . odd  . unCXInteger
  binding "add"   = \a-> return $ TwineFunction (\[n]-> do 
                                                      i <- cxToInteger n
                                                      mbind $ (unCXInteger a + i)
                                                  )
  binding "subtract" = \a-> return $ TwineFunction (\[n]-> do 
                                                      i <- cxToInteger n
                                                      mbind $ (unCXInteger a - i)
                                                     )

mbind = return . bind
  
signal sig (TwineObject obj) = (getContext obj) sig 

cxToInteger (TwineInteger i) = return i
cxToInteger cm@(TwineObject obj) = do
  v <- signal "toInteger" cm
  case v of
    TwineInteger i -> return i
    TwineNull -> error "expected number but got null value"
    _ -> error ("'" ++ show v ++ "' is not a number")

  
  