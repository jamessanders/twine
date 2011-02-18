{-#LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , FlexibleContexts   
  , NoMonomorphismRestriction
  , OverlappingInstances
  , OverloadedStrings
  , UndecidableInstances
 #-}
module Text.Twine.Eval.ContextWriter (makeContext, (=:)) where

import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Monoid
import Text.Twine.Eval.Types
import Text.Twine.Eval.Context
import Text.Twine.Eval.FancyContext
import Text.Twine.Eval.Builtins
import qualified Data.ByteString.Char8 as C
import Control.Monad.Writer
import qualified Data.Map as M


type ContextWriter m = WriterT (M.Map C.ByteString (ContextItem m)) m () 

makeContext :: (Monad m) => ContextWriter m -> m (ContextItem m) 
makeContext cw = do
  mp <- execWriterT cw 
  return $ bind (mp `M.union` builtins)

k =: v = tell $ M.fromList [(C.pack k, bind v)]
