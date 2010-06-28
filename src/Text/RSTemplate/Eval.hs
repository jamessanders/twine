module Text.RSTemplate.Eval (runEval) where

--import Text.RSTemplate

import Control.Monad.State
import Control.Monad.Identity

import Text.RSTemplate.Eval.Types
import Text.RSTemplate.Eval.Builtins
import Text.RSTemplate.Parser.Types
import Data.ByteString.Char8 (ByteString,pack,unpack)
import qualified Data.ByteString.Char8 as C
import Debug.Trace

type Stack m a = StateT (ContextItem m) m a 

runStack = runStateT 

runEval tm cx = do 
  (r,_) <- runStack (eval' tm) cx
  return $ C.concat r

eval' :: (Monad m, Functor m) => [TemplateCode] -> Stack m [ByteString] 
eval' = mapM eval

eval :: (Monad m, Functor m) => TemplateCode -> Stack m ByteString
eval (Text x) = return x 

eval (Slot x) = do
  ee  <- evalExpr x
  case ee of 
    Just (ContextValue x) -> return x
    Just x -> return (C.pack . show $ x)
    Nothing -> return (C.pack "")

eval (Assign k e) = do 
  Just ee <- evalExpr e
  st <- get
  put (toContext [(k,ee)] <+> st)
  return (C.pack "")

eval (Cond e bls) = do
  ee <- evalExpr e
  st <- get
  case ee of
    Just x  -> case x of 
                 ContextBool False -> return (C.pack "")
                 _ -> lift $ runEval bls st
    Nothing -> return (C.pack "") 

eval (Loop e as bls) = do
  ee <- evalExpr e
  case ee of 
    Just a  -> runLoop a
    Nothing -> return (C.pack "")
  where runLoop (ContextList ls) = fmap (C.concat) $ mapM inner ls
        inner v = do cx <- get
                     lift $ runEval bls (toContext [(as,v)] <+> cx)

eval x = error $ "Cannot eval: '" ++ (show x) ++ "'"

evalExpr :: (Monad m, Functor m) => Expr -> Stack m (Maybe (ContextItem m))
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


doLookup k v = let parts = C.split '.' k
               in aux parts v
    where 
      aux [] t = return (Just t)
      aux (x:xs) t = do ll <- doLookup' x t
                        case ll of
                          Just a  -> aux xs a
                          Nothing -> return Nothing

doLookup' _ (ContextPairs []) = return Nothing
doLookup' st (ContextPairs (x:xs)) = do
    let cx = getContext x
    s <- cx st
    case s of
      Just a  -> return (Just a)
      Nothing -> doLookup' st (ContextPairs xs)
doLookup' st (ContextBool True) = return (Just $ ContextValue $ pack "True")
doLookup' st (ContextBool False) = return Nothing
doLookup' st x = error $ "Context not searchable when looking up '" ++ unpack st ++ "' in " ++ show x

------------------------------------------------------------------------

split :: Char -> [Char] -> [[Char]]
split delim s
    | [] == rest = [token]
    | otherwise = token : split delim (tail rest)
    where (token,rest) = span (/= delim) s
