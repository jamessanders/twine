{-#LANGUAGE MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  , FlexibleContexts   
  , NoMonomorphismRestriction
  , OverlappingInstances
  , OverloadedStrings
  , UndecidableInstances
 #-}
module Text.Twine.Interpreter.ContextWriter (makeContext, (=:)) where

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

type TwineObjectper m = Map ByteString (TwineElement m)
type ContextWriter m = WriterT (TwineObjectper m)  m () 

makeContext :: (Monad m) => ContextWriter m -> m (TwineElement m) 
makeContext cw = do
  mp <- execWriterT cw 
  return $ bind (mp `M.union` builtins)

--(=:) :: (MonadWriter (Map ByteString (TwineElement m)) m, Convertible a (TwineElement m)) => String -> a -> m ()
k =: v = tell $ M.fromList [(C.pack k, bind v)]
