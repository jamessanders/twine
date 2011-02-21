{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Text.Twine.Interpreter.Builtins (builtins) where

import Data.Char
import Data.List
import Text.Twine.Interpreter.Types
import Text.Twine.Interpreter.Interface
import Control.Exception (assert)
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M

myNot [b] = unbind b >>= return . bind . not
myList = return . bind

myEq [a,b] = do
  a' <- unbind a 
  b' <- unbind b 
  return $ bind ((a' :: String) == (b' :: String))

builtins :: (Monad m) => M.Map C.ByteString (TwineElement m)
builtins = M.fromList [("not", method myNot),
                       ("list", method myList),
                       ("eq?" , method myEq)]
