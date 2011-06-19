{-# LANGUAGE  NoMonomorphismRestriction, OverloadedStrings, FlexibleContexts #-}
module Text.Twine.Interpreter (runEval,getMacros) where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import Data.ByteString.Char8 (ByteString,pack,unpack)
import Debug.Trace

import Text.Twine.Interpreter.Builtins
import Text.Twine.Interpreter.Interface
import Text.Twine.Interpreter.Types
import Text.Twine.Interpreter.ContextWriter
import Text.Twine.Parser (loadTemplateFromString)
import Text.Twine.Parser.Types
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M

-- TODO: Most of the interpreter is all wrong, and there is no error
--       checking.  Eventually this all needs to be fixed up.

type Stack m a = StateT (ContextState m) (WriterT [String] m) a


getMacros = Macros . M.fromList . getMacros'
  where
    getMacros' ((Macro name params blocks):xs)  = (name, evalMacro params blocks) : getMacros' xs
    getMacros' (_:xs) = getMacros' xs
    getMacros' [] = []

evalMacro params blocks args preContext macros = do
  let localContext = M.fromList (zip params args)
  let context = (bind $ localContext) <+> preContext
  runEval' blocks context macros

foldCX :: (Monad m) => [TwineElement m] -> TwineElement m
foldCX = foldl (<+>) emptyContext

mergeCXP (TwineObjectList a) (TwineObjectList b) = TwineObjectList (a ++ b)
mergeCXP (TwineList a)  (TwineList b)  = TwineList (a ++ b)
mergeCXP (TwineObject a)   x   = TwineObjectList [a] `mergeCXP` x
mergeCXP x (TwineObject a)   = x `mergeCXP` TwineObjectList [a]
mergeCXP a b = error $ "Cannot merge " ++ show a ++ " and " ++ show b

(<+>) = mergeCXP

runStack run state = runWriterT (runStateT run state)

lift2 f = lift $ lift $ f

debug _ fn = fn 
--debug = trace

runEval :: (Monad m, Functor m) => Template -> Context m -> m ByteString
runEval tm cx = runEval' tm (bind $ unContext cx) (getMacros tm)

runEval' :: (Monad m, Functor m) => Template -> TwineElement m -> Macros m -> m ByteString
runEval' tm cx macros = do 
  ((r,log),_) <- runStack (eval' tm) (ContextState cx macros)
  debug (show r) $ do
    return $ C.concat r

getCX :: (Monad m) => Stack m (TwineElement m)
getCX = do s <- get 
           return (getContextState s)

putCX :: (Monad m) => TwineElement m -> Stack m ()
putCX cx = do s <- get
              put $ s { getContextState = cx }

eval' :: (Monad m, Functor m) => [TemplateCode] -> Stack m [ByteString] 
eval' = mapM eval

eval :: (Monad m, Functor m) => TemplateCode -> Stack m ByteString
eval (Text x) = return x 

eval (Macro _ _ _) = return (C.empty)

eval (Slot x) = debug ("evaluating slot: " ++ show x) $ do
  ee  <- evalExpr x
  st  <- case ee of
        (TwineObjectList [x]) -> lift2 $ getString x
        (TwineObject x)     -> lift2 $ getString x
        x -> lift2 $ makeString x
  return (C.pack st)
    

eval (Assign k e) = debug ("evaluating assign " ++ show k ++ " = " ++ show e) $ do 
  ee <- evalExpr e
  st <- getCX
  putCX (bind (M.fromList [(k,ee)]) <+> st)
  return (C.pack "")

eval (Cond e bls) = do
  g <- get
  ee <- evalExpr e
  st <- getCX
  case ee of
    (TwineString "") -> return C.empty
    (TwineNull) -> return (C.pack "") 
    (TwineBool False) -> return (C.pack "")
    _  -> lift2 $ runEval' bls st (getContextMacros g)


eval (Loop e as bls) = do
  ee <- evalExpr e
  s <- lift2 $ makeString ee
  case ee of 
    TwineNull -> return (C.pack "")
    a -> runLoop a
  where  
    --runLoop :: (Monad m) => TwineElement m -> m C.ByteString
    runLoop (TwineList ls) = C.concat <$> mapM inner ls  
                                                              
    runLoop (TwineObjectList x) = do                          
      it <- lift2 $ getIterable (head x)                      
      runLoop (TwineList it)                                  
                                                              
    runLoop (TwineObject x) = do
      it <- lift2 $ getIterable x                             
      trace (show it) $ runLoop (TwineList it)                                  
                                                              
    runLoop x = error $ "Not iterable: " ++ show x            
    
    inner v = do 
      g  <- get
      cx <- getCX                                  
      s <- lift2 $ makeString v
      trace (show as) $ trace s $ lift2 $ runEval' bls (bind (M.fromList [(as,v)]) <+> cx) (getContextMacros g)

eval x = error $ "Cannot eval: '" ++ (show x) ++ "'"

fromMaybeToContext (Just a)  = a
fromMaybeToContext Nothing = TwineNull


evalExpr :: (Monad m, Functor m) => Expr -> Stack m (TwineElement m)
evalExpr (Func n a) = do 
  cx <- getCX
  ll <- lift2 $ doLookup n cx
  trace (show ll) $ case ll of 
    Just (TwineFunction f) -> do 
      args <- mapM evalExpr a
      lift2 $ f args
    _ -> error $ (C.unpack n) ++ " is not a function. "

                    
evalExpr (Var n) = do g <- getCX 
                      r <- lift2 $ doLookup n g
                      case r of
                        Just a -> return a
                        Nothing-> return TwineNull

evalExpr (NumberLiteral n) = return . bind $ n
evalExpr (StringLiteral n) = do
  g  <- get
  cx <- getCX
  let tmpl = loadTemplateFromString (C.unpack n)
  nn <- lift2 $ runEval' tmpl cx (getContextMacros g)
  return . bind $ nn

evalExpr (Accessor (Var "macros") expr) = runMacro expr

evalExpr acc@(Accessor n expr) = do
  g <- getCX
  accessObjectInContext g acc
  
runMacro :: (Monad m, Functor m) => Expr -> Stack m (TwineElement m)
runMacro (Func name args) = do
  runMacro' name args
runMacro (Var name) = do
  runMacro' name []
runMacro' name args = do
  g <- get
  let macros = getContextMacros g
  case M.lookup name (unMacros macros) of
    Nothing    -> error ("Unknown macro: " ++ C.unpack name)
    Just macro -> do
      args' <- mapM evalExpr args
      x <- lift2 $ macro args' (getContextState g) macros
      return (TwineString x)
  


accessObjectInContext :: (Monad m, Functor m) => TwineElement m -> Expr -> Stack m (TwineElement m)
accessObjectInContext context (Accessor (Var n) expr) = do
  cx <- lift2 $ doLookup' n context
  case cx of
    Nothing -> error $ "Could not find " ++ (show n) ++ " in context " ++ show context 
    Just cx' -> accessObjectInContext cx' expr

accessObjectInContext context (Accessor (Func n a) expr) = do
  cx <- lift2 $ doLookup' n context
  case cx of 
    Nothing -> error $ "Could not find " ++ (show n) ++ " in context " ++ show context 
    Just cx' ->
      case cx' of
        TwineFunction f -> do
          args <- mapM evalExpr a
          z <- lift2 $ f args
          accessObjectInContext z expr
        _ -> accessObjectInContext cx' expr

accessObjectInContext context (Var n) = do
  cx <- lift2 $ doLookup' n context
  case cx of
    Nothing -> error $ "Could not find " ++ (show n) ++ " in context " ++ show context 
    Just x -> return x

  

accessObjectInContext context (Func n args) = do
  cx <- lift2 $ doLookup' n context
  case cx of 
    Nothing -> error $ "Could not find " ++ (show n) ++ " in context " ++ show context 
    Just cx' -> do
      case cx' of
        TwineFunction f -> do 
          args' <- mapM evalExpr args
          lift2 $ f args'
        _ -> error $ "Not a callable method: " ++ show cx'



doLookup k v = let parts = C.split '.' k
               in aux parts v
    where 
      aux [] t = return (Just t)
      aux (x:xs) t = do 
        ll <- doLookup' x t
        case ll of
          Just a  -> aux xs a
          Nothing -> return Nothing

doLookup' _ (TwineObjectList []) = return Nothing
doLookup' st (TwineObjectList (x:xs)) = do
    let cx = getContext x
    s <- cx st
    case s of
      TwineNull -> doLookup' st (TwineObjectList xs)
      a  -> return (Just a)

doLookup' st (TwineBool True) = return (Just $ TwineString $ pack "True")
doLookup' st (TwineBool False) = return Nothing
doLookup' st (TwineObject m) = (getContext m) st >>= return . Just
      
doLookup' st x = error $ "Context not searchable when looking up '" ++ unpack st ++ "' in " ++ show x

------------------------------------------------------------------------

split :: Char -> [Char] -> [[Char]]
split delim s
    | [] == rest = [token]
    | otherwise = token : split delim (tail rest)
    where (token,rest) = span (/= delim) s
