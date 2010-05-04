module Text.RSTemplate.Eval.Builtins where

import Data.Char
import Text.RSTemplate.Eval.Types
import Control.Exception (assert)
import qualified Data.ByteString.Char8 as C

myId [x] = id x
myId x   = justcx . show $ x

myUpper [Just (ContextValue x)] = justcx (C.map toUpper x)
myUpper [Nothing] = Nothing

myLower [Just (ContextValue x)] = justcx (C.map toLower x)
myLower [Nothing] = Nothing

myLength [Just (ContextValue x)] = justcx . show . C.length $ x
myLength [Just (ContextList x)]  = justcx . show . length $ x
myLength [Just (ContextPairs x)] = justcx . show . length $ x
myLength [Nothing] = justcx "0"

myEven [Just (ContextValue x)] = boolcx . even . read . C.unpack $ x
myOdd  [Just (ContextValue x)] = boolcx . odd  . read . C.unpack $ x

myNot [Just _]  = Nothing
myNot [Nothing] = boolcx True

myEq [a,b] = if a == b then boolcx True else boolcx False

myZip [Just (ContextList a),Just (ContextList b)] = justcx [[x,y] | x <- a , y <- b]

myRange [Just (ContextValue a),Just (ContextValue b)] = justcx $ map (C.pack . show) [read (C.unpack a) :: Int .. read (C.unpack b) - 1 :: Int]
myEnum [Just (ContextList a)] = justcx $ map (C.pack . show) $ [0..length a - 1]

myGetItem [Just (ContextList a)
          ,Just (ContextValue b)] = let b' = read $ C.unpack b in
                                    if b' < length a then Just $ a !! b' else Nothing

mySucc [Just (ContextValue a)] = let str = C.unpack a
                                           in if not (number str) 
                                                 then error "succ: Argument is not a number"
                                                 else justcx . show $ succ (read str :: Integer)

mathOn f [Just (ContextValue a)
         ,Just (ContextValue b)] = let a' = C.unpack $ a
                                       b' = C.unpack $ b in
                                   if number a' && number b'
                                     then justcx $ show (read a' `f` read b')
                                     else error "math: Argumuent is not a number"

myHead [Just (ContextList a)] = Just (head a)

number = and . map isNumber

boolcx True  = justcx "True"
boolcx False = Nothing



builtins = [("id",myId)
           ,("not",myNot)
           ,("upper",myUpper)
           ,("lower",myLower)
           ,("length",myLength)
           ,("eq", myEq)             
           ,("even",myEven)
           ,("odd",myOdd)
           ,("range",myRange)
           ,("zip",myZip)
           ,("$",myGetItem)
           ,("subtract",mathOn (-))
           ,("add",mathOn (+))
           ,("enum",myEnum)
           ,("head",myHead)
           ,("succ",mySucc)]