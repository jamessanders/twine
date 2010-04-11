module Text.RSTemplate (module Text.RSTemplate.Parser
                       ,module Text.RSTemplate.Eval.Types
                       ,evalFile
                       ,ioEvalFile )
where

import Text.RSTemplate.Parser

import Text.RSTemplate.Eval
import Text.RSTemplate.Eval.Types

import Text.RSTemplate.EvalIO

import System.FilePath 

evalFile   fp = parseFile fp >>= \ps -> return (\cx-> evalTemplate ps cx)
ioEvalFile fp = do
  ps <- parseFile fp 
  pi <- doInclude (takeDirectory fp) ps  
  return (\cx-> evalIOTemplate pi cx)