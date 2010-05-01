module Text.RSTemplate (module Text.RSTemplate.Parser
                       ,module Text.RSTemplate.Eval.Types
                       ,evalFile
                       ,evalTemplate
                       ,ioEvalFile )
where

import Text.RSTemplate.Parser

import Text.RSTemplate.Eval
import Text.RSTemplate.Eval.Types

import Text.RSTemplate.EvalIO

import System.FilePath 

evalFile' fp = do ps <- parseFile fp 
                  doInclude (takeDirectory fp) ps

evalFile fp = do 
  pi <- evalFile' fp
  return (\cx-> evalTemplate pi cx)

ioEvalFile fp = do
  pi <- evalFile' fp
  return (\cx-> evalIOTemplate pi cx)