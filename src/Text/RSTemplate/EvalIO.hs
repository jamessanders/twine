module Text.RSTemplate.EvalIO where

import Data.Maybe
import Text.RSTemplate.Eval
import Text.RSTemplate.Eval.Types
import Text.RSTemplate.Parser.Types
import Text.RSTemplate.Eval.Builtins
import qualified Data.ByteString.Char8 as C

ioCxpLookup k a = ioCxpLookup' (split '.' k) a

ioCxpLookup' (x:xs) (ContextPairs c) = do l <- ioCxLookup x c
                                          case l of
                                            Just a  -> ioCxpLookup' xs a
                                            Nothing -> return Nothing
ioCxpLookup' (x:xs) _ = return $ Nothing
ioCxpLookup' []     a = return $ Just a


ievalExpr cx (Func n a) = case lookup n builtins of
                           Just f  -> do x <- mapM (ievalExpr cx) a
                                         return (f x)
                           Nothing -> error $ n ++ " is not a builtin function."

ievalExpr cx (Var n) = ioCxpLookup n cx
ievalExpr cx (NumberLiteral n) = return $ justcx $ show n
ievalExpr cx (StringLiteral n) = return $ justcx n


evalIOTemplate tc cx = mapM (evalIOTemplateBlock cx) tc >>= return . C.concat

evalIOTemplateBlock cx (Text t) = return t

evalIOTemplateBlock cx (Slot k) = do x <- ievalExpr cx k
                                     return (showCX $ fromMaybe (ContextValue C.empty) x)

evalIOTemplateBlock cx (Cond k bls) = do x <- ievalExpr cx k
                                         case  x of
                                           Just _ -> evalIOTemplate bls cx
                                           Nothing-> return C.empty

evalIOTemplateBlock cx (Loop k as bls) = do x <- ievalExpr cx k
                                            case x of
                                              Just val -> do x <- sequence (mapCL runLoop val) 
                                                             return . C.concat $ x
                                              Nothing  -> return C.empty
    where runLoop n ls = let ncx = ContextPairs [(CX [(as,ls),("#",ContextValue $ C.pack $ show n)])] <+> cx  
                         in evalIOTemplate bls ncx

