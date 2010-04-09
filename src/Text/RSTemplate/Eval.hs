module Text.RSTemplate.Eval where

import Text.RSTemplate.Parser.Types
import Text.RSTemplate.Eval.Types
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

evalTemplate tc cx = C.concat $ map (evalTemplateBlock cx) tc

evalTemplateBlock cx (Text t) = t
evalTemplateBlock cx (Slot k) = showCX $ fromMaybe (ContextValue C.empty) (cxpLookup k cx)
evalTemplateBlock cx (Cond k bls) = case cxpLookup k cx of
                                      Just _ -> evalTemplate bls cx
                                      Nothing-> C.empty
evalTemplateBlock cx (Loop k as bls) = case cxpLookup k cx of
                                         Just val -> C.concat $ mapCL runLoop val
                                         Nothing  -> C.empty
    where runLoop n ls = let ncx = ContextPairs [(CX [(as,ls),("#",ContextValue $ C.pack $ show n)])] <+> cx 
                         in evalTemplate bls ncx

--
