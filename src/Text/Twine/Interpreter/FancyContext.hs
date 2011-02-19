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
  property "length" = mbind . length . unCXListLike
  property "head"   = mbind . head . unCXListLike
  property "tail"   = mbind . tail . unCXListLike
  property "init"   = mbind . init . unCXListLike
  property "last"   = mbind . last . unCXListLike
  property "item"   = \x -> return $ TwineFunction (\[n] -> do
                                                       i <- cxToInteger n
                                                       mbind (unCXListLike x !! fromIntegral i))
                           
  property "take"   = \x -> return $ TwineFunction (\[n] -> do
                                                        i <- cxToInteger n
                                                        mbind . take (fromIntegral i) $ unCXListLike x)
  
  property "drop"   = \x -> return $ TwineFunction (\[n] -> do
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
  property "toInteger" = return . TwineInteger . unCXInteger  
  property "even?" = mbind . even . unCXInteger
  property "odd?"  = mbind . odd  . unCXInteger
  property "add"   = \a-> return $ TwineFunction (\[n]-> do 
                                                      i <- cxToInteger n
                                                      mbind $ (unCXInteger a + i)
                                                  )
  property "subtract" = \a-> return $ TwineFunction (\[n]-> do 
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

  
  