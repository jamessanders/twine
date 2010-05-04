module Text.RSTemplate.Eval (evalTemplate,split,mapCL,showCX) where

import Text.RSTemplate.Parser.Types
import Text.RSTemplate.Eval.Types
import Text.RSTemplate.Eval.Builtins

import Data.Maybe

import qualified Data.ByteString.Char8 as C

split :: Char -> [Char] -> [[Char]]
split delim s
    | [] == rest = [token]
    | otherwise = token : split delim (tail rest)
    where (token,rest) = span (/= delim) s

showCX (ContextValue x) = x
showCX (ContextList  x) = C.pack $ show x
showCX (ContextPairs x) = C.pack $ show x

toCX (ContextValue _) = error "not a context"
toCX (ContextPairs x) = x
toCX (ContextList  x) = toCX (head x)

mapCL f (ContextList x)  = map (\(a,b)->f b a) $ zip x [1..]
mapCL f a@(ContextValue x) = [f 1 a]
mapCL f _ = error "not a list of key/value pairs"

cxpLookup k a = cxpLookup' (split '.' k) a
cxpLookup' (x:xs) (ContextPairs c) = case cxLookup x c of
                                       Just a -> cxpLookup' xs a
                                       Nothing -> Nothing
cxpLookup' (x:xs) _ = Nothing
cxpLookup' []     a = Just a

evalExpr cx (Func n a) = case lookup n builtins of
                           Just f  -> f $ map (evalExpr cx) a
                           Nothing -> error $ n ++ " is not a builtin function."
evalExpr cx (Var n) = cxpLookup n cx
evalExpr cx (NumberLiteral n) = justcx $ show n
evalExpr cx (StringLiteral n) = justcx n

evalTemplate tc cx = C.concat . reverse $ walk cx tc []

walk _  []     nl = nl
walk cx (x:xs) nl = let (c,str) = evalTemplateBlock cx x
                    in walk c xs (str:nl)

evalTemplateBlock cx (Text t) = (cx,t)

evalTemplateBlock cx (Slot k) = (cx,showCX $ fromMaybe (ContextValue C.empty) (evalExpr cx k))

evalTemplateBlock cx (Assign k e) = let ev = evalExpr cx e
                                    in case ev of
                                         Just a  -> (cx <+> ContextPairs [CX [(k,a)]] ,C.empty)
                                         Nothing -> (cx,C.empty)

evalTemplateBlock cx (Cond k bls) = case evalExpr cx k of
                                      Just _ -> (cx,evalTemplate bls cx)
                                      Nothing-> (cx,C.empty)

evalTemplateBlock cx (Loop k as bls) = case evalExpr cx k of
                                         Just val -> (cx,C.concat $ mapCL runLoop val)
                                         Nothing  -> (cx,C.empty)
    where runLoop n ls = let ncx = ContextPairs [(CX [(as,ls),("#",ContextValue $ C.pack $ show n)])] <+> cx 
                         in evalTemplate bls ncx

--
