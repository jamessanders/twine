{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.RSTemplate.EvalM where

--import Text.RSTemplate

import Control.Monad.State
import Control.Monad.Identity

import Text.RSTemplate.Eval.Types
import Text.RSTemplate.Eval.Builtins
import Text.RSTemplate.Parser.Types
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C

type Stack m a = StateT (ContextItem m) m a 

runStack = runStateT 

runEval tm cx = do 
  (r,_) <- runStack (eval' tm) cx
  return $ C.concat r

eval' :: (Monad m) => [TemplateCode] -> Stack m [ByteString] 
eval' = mapM eval

eval :: (Monad m) => TemplateCode -> Stack m ByteString
eval (Text x) = return x 
eval (Slot x) = do
  ee  <- evalExpr x
  case ee of 
    Just (ContextValue x) -> return x
    Nothing -> return (C.pack "")

evalExpr :: (Monad m) => Expr -> Stack m (Maybe (ContextItem m))
evalExpr (Func n a) = do 
  cx <- get
  case lookup n builtins of
    Just f  -> fmap f $ mapM (evalExpr) a
    Nothing -> error $ (C.unpack n) ++ " is not a builtin function."
evalExpr (Var n) = do g <- get 
                      r <- lift $ doLookup n g
                      return r
evalExpr (NumberLiteral n) = return . justcx . C.pack . show  $ n
evalExpr (StringLiteral n) = return . justcx $ n

doLookup _ (ContextPairs []) = return Nothing
doLookup st (ContextPairs (x:xs)) = do
    let cx = getContext x
    s <- cx st
    case s of
      Just a  -> return (Just a)
      Nothing -> doLookup st (ContextPairs xs)
doLookup _ _ = error "Context not searchable"

