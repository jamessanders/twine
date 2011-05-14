{-#LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , FlexibleContexts   
  , NoMonomorphismRestriction
  , OverlappingInstances
  , OverloadedStrings
  , UndecidableInstances
 #-}
module Text.Twine.Interpreter.ContextWriter (mapToContext, makeContext, (=:), merge) where

import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Monoid
import Text.Twine.Interpreter.Types
import Text.Twine.Interpreter.Interface
import Text.Twine.Interpreter.Builtins
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Control.Monad.Writer
import Data.Map (Map)
import Data.Convertible
import qualified Data.Map as M

type ContextWriter m = WriterT (Context m)  m () 

makeContext :: (Monad m) => ContextWriter m -> m (Context m) 
makeContext cw = do
  mp <- execWriterT cw 
  return $ Context (unContext mp `M.union` builtins)

--(=:) :: (MonadWriter (Map ByteString (TwineElement m)) m, Convertible a (TwineElement m)) => String -> a -> m ()
k =: v = tell $ Context (M.fromList [(C.pack k, bind v)])

merge a b = Context (unContext a `M.union` unContext b)

mapToContext = Context