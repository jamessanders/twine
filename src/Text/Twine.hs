module Text.Twine (module Text.Twine.Parser
                       ,module Text.Twine.Eval
                       ,module Text.Twine.Eval.Types
                       ,module Text.Twine.Eval.Context
                       ,module Text.Twine.Eval.FancyContext
                       ,module Text.Twine.Eval.ContextWriter
                       ,evalTemplate)
where

import Text.Twine.Parser
import Text.Twine.Eval.Types
import Text.Twine.Eval.Context
import Text.Twine.Eval.FancyContext
import Text.Twine.Eval.ContextWriter
import Text.Twine.Eval

evalTemplate template context = do 
  templ <- loadTemplate template
  runEval templ context