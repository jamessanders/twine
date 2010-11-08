module Text.RSTemplate (module Text.RSTemplate.Parser
                       ,module Text.RSTemplate.Eval
                       ,module Text.RSTemplate.Eval.Types
                       ,evalTemplate)
where

import Text.RSTemplate.Parser
import Text.RSTemplate.Eval.Types
import Text.RSTemplate.Eval

evalTemplate template context = do 
  templ <- loadTemplate template
  runEval templ context