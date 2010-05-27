{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (putStr)
import Text.RSTemplate
import Data.ByteString.Char8 (ByteString,putStr)

-- Sometime string literals will present ambiguity when
-- OverloadedStrings is used.  The below function is a 
-- somewhat shorter way to hint that we want a ByteString
bs :: ByteString -> ByteString
bs = id

-- Create some example types that represent pets 
data Species = Bird | Cat | Dog | Fish 
               deriving (Show)

data Pet = Pet { getPetName :: String
               , getPetAge  :: Float
               , getPetSpecies :: Species }
           deriving (Show)

mkPet a b c = Pet a b c

-- Make Pet an instance of ContextLookup so that we can use it in out templates
instance ContextLookup Pet where
    cxLookup "name" = justcx . getPetName
    cxLookup "age"  = justcx . show . getPetAge
    cxLookup "species" = justcx . show . getPetSpecies
    cxLookup _ = const Nothing

-- Create a context using the ContextWriter monad
cx = do 
  set "author" . cxw $ do set "fname" (bs "James")
                          set "lname" (bs "Sanders")
  set "pets" [mkPet "Samson"  1 Dog
             ,mkPet "Mango"   1 Bird
             ,mkPet "Simon"   1.5 Cat
             ,mkPet "Spikey"  1.5 Fish]

-- Run our template using the cx function for its context
main = runRSTemplate "example.tpl" cx >>= putStr

