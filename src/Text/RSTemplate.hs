module Text.RSTemplate (module Text.RSTemplate.Parser
                       ,module Text.RSTemplate.Eval
                       ,module Text.RSTemplate.Eval.Types
                       ,module Text.RSTemplate.Eval.Context
                       ,module Text.RSTemplate.Eval.FancyContext
                       ,module Text.RSTemplate.Eval.ContextWriter
                       ,evalTemplate)
where

import Text.RSTemplate.Parser
import Text.RSTemplate.Eval.Types
import Text.RSTemplate.Eval.Context
import Text.RSTemplate.Eval.FancyContext
import Text.RSTemplate.Eval.ContextWriter
import Text.RSTemplate.Eval

evalTemplate template context = do 
  templ <- loadTemplate template
  runEval templ context