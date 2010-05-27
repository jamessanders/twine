module Text.RSTemplate (module Text.RSTemplate.Parser
                       ,module Text.RSTemplate.Eval.Types
                       ,evalFile
                       ,evalTemplate
                       ,ioEvalFile
                       ,runRSTemplate )
where

import System.FilePath 
import Text.RSTemplate.Eval
import Text.RSTemplate.Eval.Types
import Text.RSTemplate.EvalIO
import Text.RSTemplate.Parser

evalFile' fp = do ps <- parseFile fp 
                  doInclude (takeDirectory fp) ps

evalFile fp = do 
  pi <- evalFile' fp
  return (\cx-> evalTemplate pi cx)

ioEvalFile fp = do
  pi <- evalFile' fp
  return (\cx-> evalIOTemplate pi cx)

runRSTemplate fp cx = do ps <- parseFile fp >>= doInclude (takeDirectory fp)
                         evalIOTemplate ps (cxw cx)