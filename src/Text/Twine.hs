module Text.Twine (module Text.Twine.Parser
                  ,module Text.Twine.Interpreter
                  ,module Text.Twine.Interpreter.Context
                  ,module Text.Twine.Interpreter.FancyContext
                  ,module Text.Twine.Interpreter.ContextWriter
                  ,evalTemplate)
where

import Text.Twine.Parser
import Text.Twine.Interpreter.Types
import Text.Twine.Interpreter.Context
import Text.Twine.Interpreter.FancyContext
import Text.Twine.Interpreter.ContextWriter
import Text.Twine.Interpreter

evalTemplate template context = do 
  templ <- loadTemplateFromFile template
  runEval templ context