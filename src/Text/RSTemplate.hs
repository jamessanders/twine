module Text.RSTemplate (module Text.RSTemplate.Parser, evalFile )
where

import Text.RSTemplate.Parser
import Text.RSTemplate.Eval

evalFile  fp = parseFile fp >>= \ps -> return (\cx-> evalTemplate ps cx)