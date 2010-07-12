{-# LANGUAGE OverloadedStrings #-}
module Text.RSTemplate.Eval.Builtins (builtins) where

import Data.Char
import Data.List
import Text.RSTemplate.Eval.Types
import Text.RSTemplate.Eval.Context
import Control.Exception (assert)
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M

myId [x] = return . id $ x
myId x   = return . justcx . C.pack . show $ x

myUpper [ContextValue x] = return . justcx $ (C.map toUpper x)
myUpper _ = return ContextNull

myLower [ContextValue x] = return $ justcx (C.map toLower x)
myLower _ = return ContextNull

myCapitalize [ContextValue x] = return $ justcx (toUpper (C.head x) `C.cons` C.map toLower (C.tail x))

myLength [ContextValue x] = return . justcx . C.pack . show . C.length $ x
myLength [ContextList x]  = return . justcx . C.pack . show . length $ x
myLength [ContextPairs x] = return . justcx . C.pack . show . length $ x
myLength _ = return $ justcx ("0" :: C.ByteString)

myEven [ContextValue x] = return . boolcx . even . read . C.unpack $ x
myOdd  [ContextValue x] = return . boolcx . odd  . read . C.unpack $ x

myNot [ContextBool True]  = return $ boolcx False
myNot [ContextBool False] = return $ boolcx True

myEq [ContextValue a
     ,ContextValue b] = if a == b 
                           then return $ boolcx True 
                           else return $ boolcx False

myZip [ContextList a
      ,ContextList b] = return $ justcx [[x,y] | x <- a , y <- b]

myRange [ContextValue a ,ContextValue b] = 
    return $ justcx $ map (C.pack . show) [read (C.unpack a) :: Int .. read (C.unpack b) - 1 :: Int]

myEnum [ContextList a] = return . justcx $ map (C.pack . show) $ [0..length a - 1]
myEnum _ = error "enum: not a list"

myElem [x, ContextList y] = return $ boolcx (x `elem` y)

myGetItem [ContextList a
          ,ContextValue b] = let b' = read $ C.unpack b in
                             if b' < length a 
                                then return $ a !! b' 
                                else return ContextNull

mySucc [ContextValue a] = let str = C.unpack a
                          in if not (number str) 
                             then error "succ: Argument is not a number"
                             else return $ justcx . C.pack . show $ succ (read str :: Integer)

mathOn f [ContextValue a
         ,ContextValue b] = let a' = C.unpack $ a
                                b' = C.unpack $ b in
                            if number a' && number b'
                              then return . justcx . C.pack $ show (read a' `f` read b')
                              else error "math: Argumuent is not a number"

myHead [ContextList a] = return $ head a
myHead _ = return $ ContextNull

myType [ContextList _]  = return . justcx $ C.pack "<List>"
myType [ContextPairs _] = return . justcx $ C.pack "<Dict>"
myType [ContextValue _] = return . justcx $ C.pack "<Value>"

number = and . map isNumber

boolcx = ContextBool


builtins :: (Monad m) => M.Map C.ByteString (BuiltinFunc m)
builtins = M.fromList [("id",myId)
                      ,("type",myType)
                      ,("not",myNot)
                      ,("upper",myUpper)
                      ,("lower",myLower)
                      ,("capitalize",myCapitalize)
                      ,("length",myLength)
                      ,("eq", myEq)             
                      ,("eq?", myEq)             
                      ,("even",myEven)
                      ,("even?",myEven)
                      ,("odd",myOdd)
                      ,("odd?",myOdd)
                      ,("range",myRange)
                      ,("zip",myZip)
                      ,("$",myGetItem)
                      ,("item",myGetItem)
                      ,("subtract",mathOn (-))
                      ,("add",mathOn (+))
                      ,("enum",myEnum)
                      ,("head",myHead)
                      ,("elem?",myElem)
                      ,("succ",mySucc)]
