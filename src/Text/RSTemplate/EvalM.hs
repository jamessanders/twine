{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EvalM where

import Text.RSTemplate

import Control.Monad.State
import Text.RSTemplate.Eval.Types
import Text.RSTemplate.Eval.Builtins
import Text.RSTemplate.Parser.Types
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C

newtype Stack a = Stack { unStack :: State (ContextItem CX) a }
    deriving (Monad,Functor,MonadState (ContextItem CX))

runStack = runState . unStack

runEval tm cx = C.concat . fst $ runStack (eval' tm) cx

eval' :: [TemplateCode] -> Stack [ByteString]
eval' = mapM eval

eval :: TemplateCode -> Stack ByteString
eval (Text x) = return x 
eval (Slot x) = do
  ee   <- evalExpr x
  case ee of 
    Just (ContextValue x) -> return x
    Nothing -> return (C.pack "")

evalExpr :: Expr -> Stack (Maybe (ContextItem CX))
evalExpr (Func n a) = do 
  cx <- get
  case lookup n builtins of
    Just f  -> fmap f $ mapM (evalExpr) a
    Nothing -> error $ (C.unpack n) ++ " is not a builtin function."
evalExpr (Var n) = get >>= return . cxpLookup n 
evalExpr (NumberLiteral n)  = return . justcx . C.pack . show  $ n
evalExpr (StringLiteral n) = return . justcx $ n

cxpLookup k a = cxpLookup' (C.split '.' k) a
cxpLookup' (x:xs) (ContextPairs c) = 
    case cxLookup x c of
      Just a -> cxpLookup' xs a
      Nothing -> Nothing
cxpLookup' (x:xs) _ = Nothing
cxpLookup' []     a = Just a