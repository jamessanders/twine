{-# LANGUAGE OverloadedStrings #-}
module Text.RSTemplate.Eval.Builtins (builtins) where

import Data.Char
import Data.List
import Text.RSTemplate.Eval.Types
import Text.RSTemplate.Eval.Context
import Control.Exception (assert)
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M

builtins :: (Monad m) => M.Map C.ByteString (BuiltinFunc m)
builtins = M.fromList []
