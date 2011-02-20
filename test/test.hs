{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
import Test.HUnit
import Text.Twine
import Control.Monad.Identity
import Data.ByteString.Char8 (ByteString)

mcx m = runIdentity $ makeContext m
runI x y = runIdentity $ runEval x y

bs :: ByteString -> ByteString
bs = id

data User = User {
  getUserName :: String,
  getUserAge  :: Int
  }

instance TemplateInterface Identity User where
  property "name" = return . bind . getUserName
  property "age"  = return . bind . getUserAge
  property _ = const undefined
  makeString = return . const "<user>"

template1 = loadTemplateFromString "Hello {{name}}"
template2 = loadTemplateFromString "Hello {@|a <- list|{{a}}@}"
template3 = loadTemplateFromString "Hello{?|test| World?}"
template4 = loadTemplateFromString "Hello {?|user.age.gt?(18)|{{user.name}} - {{user.age}}?}"
template5 = loadTemplateFromString "Hello {?|user.age.gt?(25)|{{user.name}} - {{user.age}}?}"


render1 = runI template1 (mcx $ do "name" =: bs "World")
render2 = runI template2 (mcx $ do "list" =: ([1..4] :: [Int]))
render3 = runI template3 (mcx $ do "test" =: True)
render4 = runI template3 (mcx $ do "test" =: False)
render5 = runI template4 (mcx $ do "user" =: (User "James" 21))
render6 = runI template5 (mcx $ do "user" =: (User "James" 21))

          
tests = TestList [
  "test1" ~: "Hello World" ~=? render1,
  "test2" ~: "Hello 1234" ~=? render2,
  "test3" ~: "Hello World" ~=? render3,
  "test4" ~: "Hello" ~=? render4,
  "test5" ~: "Hello James - 21" ~=? render5,
  "test6" ~: "Hello " ~=? render6
  ]

main = do
  runTestTT tests
