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

myUpper [Just (ContextValue x)] = return . justcx $ (C.map toUpper x)
myUpper [Nothing] = return Nothing

myLower [Just (ContextValue x)] = return $ justcx (C.map toLower x)
myLower [Nothing] = return Nothing

myCapitalize [Just (ContextValue x)] = return $ justcx (toUpper (C.head x) `C.cons` C.map toLower (C.tail x))

myLength [Just (ContextValue x)] = return . justcx . C.pack . show . C.length $ x
myLength [Just (ContextList x)]  = return . justcx . C.pack . show . length $ x
myLength [Just (ContextPairs x)] = return . justcx . C.pack . show . length $ x
myLength [Nothing] = return $ justcx ("0" :: C.ByteString)

myEven [Just (ContextValue x)] = return . boolcx . even . read . C.unpack $ x
myOdd  [Just (ContextValue x)] = return . boolcx . odd  . read . C.unpack $ x

myNot [Just _]  = return Nothing
myNot [Nothing] = return $ boolcx True

myEq [Just (ContextValue a)
     ,Just (ContextValue b)] = if a == b 
                               then return $ boolcx True 
                               else return $ boolcx False

myZip [Just (ContextList a),Just (ContextList b)] = 
    return $ justcx [[x,y] | x <- a , y <- b]

myRange [Just (ContextValue a),Just (ContextValue b)] = 
    return $ justcx $ map (C.pack . show) [read (C.unpack a) :: Int .. read (C.unpack b) - 1 :: Int]

myEnum [Just (ContextList a)] = return . justcx $ map (C.pack . show) $ [0..length a - 1]
myEnum _ = error "enum: not a list"

myElem [Just x, Just (ContextList y)] = return $ boolcx (x `elem` y)

myGetItem [Just (ContextList a)
          ,Just (ContextValue b)] = let b' = read $ C.unpack b in
                                    if b' < length a 
                                    then return $ Just $ a !! b' 
                                    else return Nothing

mySucc [Just (ContextValue a)] = let str = C.unpack a
                                           in if not (number str) 
                                                 then error "succ: Argument is not a number"
                                                 else return $ justcx . C.pack . show $ succ (read str :: Integer)

mathOn f [Just (ContextValue a)
         ,Just (ContextValue b)] = let a' = C.unpack $ a
                                       b' = C.unpack $ b in
                                   if number a' && number b'
                                     then return . justcx . C.pack $ show (read a' `f` read b')
                                     else error "math: Argumuent is not a number"

myHead [Just (ContextList a)] = return $ Just (head a)
myHead _ = return $ Nothing

myType [Just (ContextList _)]  = return . justcx $ C.pack "<List>"
myType [Just (ContextPairs _)] = return . justcx $ C.pack "<Dict>"
myType [Just (ContextValue _)] = return . justcx $ C.pack "<Value>"

number = and . map isNumber

boolcx True  = justcx ("True" :: C.ByteString)
boolcx False = Nothing

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
