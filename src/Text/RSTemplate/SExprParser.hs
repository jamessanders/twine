module Text.RSTemplate.SExprParser (parseExpr) where
import Text.Parsec
import qualified Data.Char as C
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS
import Text.RSTemplate.Parser.Types

mkNumber = NumberLiteral . read
mkVar    = Var . BS.pack

getName (Var n) = n
             
openP = char '('
closeP = char ')' 

parser = many block 

block = do manyTill space (lookAhead openP)
           sexpr

valid = letter <|> oneOf "+-*$/?" <|> digit

sexpr = do openP 
           x <- filter (/= Var (BS.pack "")) <$> manyTill (atom <|> sexpr) closeP
           return (Func (getName $ head x) (tail x))

atom = do n <- lookAhead $ choice [char '"'
                                  ,digit
                                  ,valid]
          next n
    where
      next x | C.isNumber x = var mkNumber
             | x == '"' = sstring
      next _ = var mkVar
            
sstring = do char '"'
             x <- manyTill (anyChar) (char '"') 
             return . StringLiteral. BS.pack $ x

var f = do x <- manyTill (valid) (lookAhead closeP <|> space)
           return (f x)

parseSexpr :: String -> Either ParseError [Expr]
parseSexpr x = parse parser "unknown" x

parseExpr x = case parseSexpr (BS.unpack x) of
                Left a  -> error (show a)
                Right a -> head a
