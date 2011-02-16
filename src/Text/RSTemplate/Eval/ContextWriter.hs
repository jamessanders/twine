{-#LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , FlexibleContexts   
  , NoMonomorphismRestriction
  , OverlappingInstances
  , OverloadedStrings
  , UndecidableInstances
 #-}
module Text.RSTemplate.Eval.ContextWriter where

import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Monoid
import Text.RSTemplate.Eval.Types
import Text.RSTemplate.Eval.Context
import Text.RSTemplate.Eval.FancyContext
import Text.RSTemplate.Eval.Builtins
import qualified Data.ByteString.Char8 as C
import Control.Monad.Writer
import qualified Data.Map as M


type ContextWriter m = WriterT (M.Map C.ByteString (ContextItem m)) m () 

makeContext :: (Monad m) => ContextWriter m -> m (ContextItem m) 
makeContext cw = do
  mp <- execWriterT cw 
  return $ bind (mp `M.union` builtins)

set k v = tell $ M.fromList [(C.pack k, bind v)]
