{-# LANGUAGE OverloadedStrings #-}
module Text.Twine.Interpreter.Builtins (builtins) where

import Data.Char
import Data.List
import Text.Twine.Interpreter.Types
import Text.Twine.Interpreter.Interface
import Text.Twine.Interpreter.InternalInterfaces
import Control.Exception (assert)
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M

myNot [TwineBool b] = return . bind $ not b

builtins :: (Monad m) => M.Map C.ByteString (TwineElement m)
builtins = M.fromList [("not", TwineFunction myNot)]
