module Text.Twine (
  module Text.Twine.Parser
  ,module Text.Twine.Interpreter
  ,module Text.Twine.Interpreter.Interface
  ,module Text.Twine.Interpreter.ContextWriter
  ,evalTemplate)
where

import Text.Twine.Interpreter
import Text.Twine.Interpreter.Interface
import Text.Twine.Interpreter.ContextWriter
import Text.Twine.Interpreter.Types
import Text.Twine.Parser

evalTemplate template context = do 
  templ <- loadTemplateFromFile template
  runEval templ context