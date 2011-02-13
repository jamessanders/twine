module Text.RSTemplate.Eval (runEval) where

--import Text.RSTemplate

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity

import Text.RSTemplate.Eval.Types
import Text.RSTemplate.Eval.Builtins
import Text.RSTemplate.Parser.Types
import Data.ByteString.Char8 (ByteString,pack,unpack)
import qualified Data.ByteString.Char8 as C
import Debug.Trace
import qualified Data.Map as M

type Stack m a = StateT (ContextState m) (WriterT [String] m) a

runStack run state = runWriterT (runStateT run state)

lift2 f = lift $ lift $ f
debug _ fn = fn 

runEval :: (Monad m, Functor m) => [TemplateCode] -> ContextItem m -> m ByteString
runEval tm cx = do 
  ((r,log),_) <- runStack (eval' tm) (ContextState cx builtins)
  debug (show r) $ do
    return $ C.concat r

getCX :: (Monad m) => Stack m (ContextItem m)
getCX = do s <- get 
           return (getContextState s)

putCX :: (Monad m) => ContextItem m -> Stack m ()
putCX cx = do s <- get
              put $ s { getContextState = cx }

eval' :: (Monad m, Functor m) => [TemplateCode] -> Stack m [ByteString] 
eval' = mapM eval

eval :: (Monad m, Functor m) => TemplateCode -> Stack m ByteString
eval (Text x) = return x 

eval (Slot x) = debug ("evaluating slot: " ++ show x) $ do
  ee  <- evalExpr x
  case ee of 
    (ContextValue x) -> return x
    (ContextNull)    -> return (C.pack "")
    x                -> return (C.pack . show $ x)
    

eval (Assign k e) = debug ("evaluating assign " ++ show k ++ " = " ++ show e) $ do 
  ee <- evalExpr e
  st <- getCX
  putCX (bind [(k,ee)] <+> st)
  return (C.pack "")

eval (Cond e bls) = do
  ee <- evalExpr e
  st <- getCX
  case ee of
    (ContextNull) -> return (C.pack "") 
    (ContextBool False) -> return (C.pack "")
    _  ->  lift2 $ runEval bls st


eval (Loop e as bls) = do
  ee <- evalExpr e
  case ee of 
    ContextNull -> return (C.pack "")
    a -> runLoop a
  where runLoop (ContextList ls) = fmap (C.concat) $ mapM inner ls
        runLoop (ContextPairs x) = do
          it <- lift2 $ getIterable (head x)
          runLoop (ContextList it)
        runLoop x = error $ "Not iterable: " ++ show x
        inner v = do cx <- getCX
                     lift2 $ runEval bls (bind [(as,v)] <+> cx)

eval x = error $ "Cannot eval: '" ++ (show x) ++ "'"

fromMaybeToContext (Just a)  = a
fromMaybeToContext Nothing = ContextNull


evalExpr :: (Monad m, Functor m) => Expr -> Stack m (ContextItem m)
evalExpr (Func n a) = do 
  cx <- getCX
  case M.lookup n builtins of
    Just f  -> do args <- mapM evalExpr a
                  lift2 $ f args
    Nothing -> do ll <- lift2 $ doLookup n cx
                  case ll of 
                    Just (ContextFunction f) -> do 
                      args <- mapM evalExpr a
                      lift2 $ f args
                    _ -> error $ (C.unpack n) ++ " is not a function. "

                    
evalExpr (Var n) = do g <- getCX 
                      r <- lift2 $ doLookup n g
                      case r of
                        Just a -> return a
                        Nothing-> case M.lookup n builtins of
                                    Just f  -> lift2 $ f []
                                    Nothing -> return ContextNull

evalExpr (NumberLiteral n) = return . ContextInteger $ n
evalExpr (StringLiteral n) = return . justcx $ n

evalExpr acc@(Accessor n expr) = do
  g <- getCX
  accessObjectInContext g acc
  
accessObjectInContext :: (Monad m, Functor m) => ContextItem m -> Expr -> Stack m (ContextItem m)
accessObjectInContext context (Accessor (Var n) expr) = do
  cx <- lift2 $ doLookup' n context
  case cx of
    Nothing -> error "ERROR"
    Just cx' -> accessObjectInContext cx' expr

accessObjectInContext context (Accessor (Func n a) expr) = do
  cx <- lift2 $ doLookup' n context
  case cx of 
    Nothing -> error "ERROR"
    Just cx' ->
      case cx' of
        ContextFunction f -> do
          args <- mapM evalExpr a
          z <- lift2 $ f args
          accessObjectInContext z expr
        _ -> accessObjectInContext cx' expr

accessObjectInContext context (Var n) = do
  cx <- lift2 $ doLookup' n context
  case cx of
    Just x -> return x
    Nothing -> error "ERROR"
  

accessObjectInContext context (Func n args) = do
  cx <- lift2 $ doLookup' n context
  case cx of 
    Nothing -> error "Error"
    Just cx' -> do
      case cx' of
        ContextFunction f -> do 
          args' <- mapM evalExpr args
          lift2 $ f args'
        _ -> error "Not a callable method"



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
      ContextNull -> doLookup' st (ContextPairs xs)
      a  -> return (Just a)

doLookup' st (ContextBool True) = return (Just $ ContextValue $ pack "True")
doLookup' st (ContextBool False) = return Nothing
doLookup' st x = error $ "Context not searchable when looking up '" ++ unpack st ++ "' in " ++ show x

------------------------------------------------------------------------

split :: Char -> [Char] -> [[Char]]
split delim s
    | [] == rest = [token]
    | otherwise = token : split delim (tail rest)
    where (token,rest) = span (/= delim) s
