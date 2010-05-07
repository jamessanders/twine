{-# LANGUAGE OverloadedStrings #-}
module Text.RSTemplate.EvalIO (evalIOTemplate) where

import Data.Maybe
import Text.RSTemplate.Eval
import Text.RSTemplate.Eval.Types
import Text.RSTemplate.Parser.Types
import Text.RSTemplate.Eval.Builtins
import qualified Data.ByteString.Char8 as C

ioCxpLookup k a = ioCxpLookup' (C.split '.' k) a

ioCxpLookup' (x:xs) (ContextPairs c) = do l <- ioCxLookup x c
                                          case l of
                                            Just a  -> ioCxpLookup' xs a
                                            Nothing -> return Nothing
ioCxpLookup' (x:xs) _ = return $ Nothing
ioCxpLookup' []     a = return $ Just a


ievalExpr cx (Func n a) = case lookup n builtins of
                           Just f  -> do x <- mapM (ievalExpr cx) a
                                         return (f x)
                           Nothing -> error $ (C.unpack n) ++ " is not a builtin function."

ievalExpr cx (Var n) = ioCxpLookup n cx
ievalExpr cx (NumberLiteral n) = return . justcx . C.pack . show $ n
ievalExpr cx (StringLiteral n) = return $ justcx n


evalIOTemplate tc cx = walk cx tc [] >>= return . C.concat . reverse

walk _  []     nl = return nl
walk cx (x:xs) nl = do (c,str) <- evalIOTemplateBlock cx x
                       walk c xs (str : nl)

evalIOTemplateBlock cx (Text t) = return (cx,t)

evalIOTemplateBlock cx (Slot k) = do x <- ievalExpr cx k
                                     return (cx,(showCX $ fromMaybe (ContextValue C.empty) x))

evalIOTemplateBlock cx (Assign k e) = do ev <- ievalExpr cx e
                                         case ev of
                                           Just a  -> return (cx <+> ContextPairs [CX [(k,a)]] 
                                                             ,C.empty)
                                           Nothing -> return (cx,C.empty)


evalIOTemplateBlock cx (Cond k bls) = do x <- ievalExpr cx k
                                         case  x of
                                           Just _ -> do n <- evalIOTemplate bls cx
                                                        return (cx,n)
                                           Nothing-> return (cx,C.empty)

evalIOTemplateBlock cx (Loop k as bls) = do x <- ievalExpr cx k
                                            case x of
                                              Just val -> do x <- sequence (mapCL runLoop val) 
                                                             return (cx,C.concat $ x)
                                              Nothing  -> return (cx,C.empty)
    where runLoop n ls = let ncx = ContextPairs [(CX [(as,ls),("#",ContextValue $ C.pack $ show n)])] <+> cx  
                         in evalIOTemplate bls ncx

