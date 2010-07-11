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
import qualified Data.Map as M

type Stack m a = StateT (ContextState m) m a 

runStack = runStateT 

runEval :: (Monad m, Functor m) => [TemplateCode] -> ContextItem m -> m ByteString
runEval tm cx = do 
  (r,_) <- runStack (eval' tm) (ContextState cx builtins)
  return $ C.concat r

getCX :: (Monad m) => Stack m (ContextItem m)
getCX = do s <- get 
           return (getContextState s)

putCX cx = do s <- get
              put $ s { getContextState = cx }

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
  st <- getCX
  putCX (toContext [(k,ee)] <+> st)
  return (C.pack "")

eval (Cond e bls) = do
  ee <- evalExpr e
  st <- getCX
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
        inner v = do cx <- getCX
                     lift $ runEval bls (toContext [(as,v)] <+> cx)

eval x = error $ "Cannot eval: '" ++ (show x) ++ "'"

evalExpr :: (Monad m, Functor m) => Expr -> Stack m (Maybe (ContextItem m))
evalExpr (Func n a) = do 
  cx <- getCX
  case M.lookup n builtins of
    Just f  -> do args <- mapM evalExpr a
                  lift $ f args
    Nothing -> do ll <- lift $ doLookup n cx
                  case ll of 
                    Just (ContextFunction f) -> do 
                      args <- mapM evalExpr a
                      lift $ f args
                    _ -> error $ (C.unpack n) ++ " is not a function. "

                    
evalExpr (Var n) = do g <- getCX 
                      r <- lift $ doLookup n g
                      case r of
                        Just a -> return r
                        Nothing-> case M.lookup n builtins of
                                    Just f  -> lift $ f []
                                    Nothing -> return Nothing
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
