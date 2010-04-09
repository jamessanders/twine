module Text.RSTemplate.EvalIO where

import Data.Maybe
import Text.RSTemplate.Eval
import Text.RSTemplate.Eval.Types
import Text.RSTemplate.EvalIO.Types
import Text.RSTemplate.Parser.Types
import qualified Data.ByteString.Char8 as C

ioCxpLookup k a = ioCxpLookup' (split '.' k) a

ioCxpLookup' (x:xs) (ContextPairs c) = do l <- ioCxLookup x c
                                          case l of
                                            Just a  -> ioCxpLookup' xs a
                                            Nothing -> return Nothing
ioCxpLookup' (x:xs) _ = return $ Nothing
ioCxpLookup' []     a = return $ Just a

evalIOTemplate :: [TemplateCode] -> ContextItem IOCX -> IO C.ByteString
evalIOTemplate tc cx = mapM (evalIOTemplateBlock cx) tc >>= return . C.concat

evalIOTemplateBlock cx (Text t) = return t

evalIOTemplateBlock cx (Slot k) = do x <- ioCxpLookup k cx
                                     return (showCX $ fromMaybe (ContextValue C.empty) x)

evalIOTemplateBlock cx (Cond k bls) = do x <- ioCxpLookup k cx
                                         case  x of
                                           Just _ -> evalIOTemplate bls cx
                                           Nothing-> return C.empty

evalIOTemplateBlock cx (Loop k as bls) = do x <- ioCxpLookup k cx
                                            case x of
                                              Just val -> sequence (mapCL runLoop val) >>= return . C.concat
                                              Nothing  -> return C.empty
    where runLoop n ls = let ncx = toIOContext [(as,ls),("#",ContextValue $ C.pack $ show n)] :: ContextItem IOCX
                         in evalIOTemplate bls (ncx <+> cx) 

