{-# LANGUAGE ExistentialQuantification, 
  TypeSynonymInstances, 
  FlexibleInstances, 
  OverlappingInstances, 
  UndecidableInstances  #-}

module Text.RSTemplate.Types where
import System.Directory
import qualified Data.ByteString.Char8 as C

type Key = String
data TemplateCode = Text C.ByteString 
                  | Slot Key
                  | Loop Key Key [TemplateCode]
                  | Cond Key     [TemplateCode]
                  | Incl FilePath
                  deriving (Show)


data ContextItem a = ContextPairs [a]
                   | ContextValue C.ByteString
                   | ContextList [ContextItem a]
                     deriving (Show)

data ParserState = ParserState { getBlocks   :: [TemplateCode] 
                               , getTextQ    :: C.ByteString
                               , getTemplate :: C.ByteString } deriving (Show)

data EvalState = EvalState { getDisplay :: C.ByteString }

class ContextLookup a where
    cxLookup :: String -> a -> Maybe (ContextItem CX)

instance ContextLookup CX where
    cxLookup k (CX a) = cxLookup k a

instance ContextLookup [CX] where
    cxLookup k []     = Nothing
    cxLookup k (x:xs) = case cxLookup k x of
                          Just a  -> Just a
                          Nothing -> cxLookup k xs

instance ContextLookup a => ContextLookup [a] where
    cxLookup k []     = Nothing
    cxLookup k (x:xs) = case cxLookup k x of
                          Just a  -> Just a
                          Nothing -> cxLookup k xs
    

instance ContextLookup (ContextItem CX) where
    cxLookup k (ContextPairs a) = cxLookup k a
 
instance ContextLookup [(String,ContextItem CX)] where
    cxLookup k a = lookup k a


data CX = forall a. (ContextLookup a) => CX a
instance Show CX where
    show _ = "<CX>"


class ToContext a where
    toContext :: a -> ContextItem CX

instance ToContext C.ByteString where
    toContext s = ContextValue s

instance ToContext String where
    toContext s = toContext (C.pack s)

instance ContextLookup a => ToContext a where
    toContext x = ContextPairs [CX x]

instance ToContext a => ToContext [a] where
    toContext x = ContextList $ map toContext x

instance ToContext a => ToContext (String,a) where
    toContext (k,v) = ContextPairs [CX [(k,toContext v)]]

instance ToContext a => ToContext [(String,a)] where
    toContext ls = foldl (<+>) (ContextPairs []) $ map toContext ls

mergeCXP (ContextPairs a) (ContextPairs b) = ContextPairs (a ++ b)
(<+>) = mergeCXP

justcx :: (ToContext a) => a -> Maybe (ContextItem CX)
justcx = Just . toContext

------------------------------------------------------------------------

data IOCX = forall a. (IOContextLookup a) => IOCX a

instance Show IOCX where
    show _ = "<IOCX>"

class IOContextLookup a where 
    ioCxLookup :: String -> a -> IO (Maybe (ContextItem IOCX))

instance ContextLookup a => IOContextLookup a where
    ioCxLookup k v = case (cxLookup k v) of
                       Just y -> return $ Just (toIOContext y)
                       Nothing-> return Nothing


instance IOContextLookup [(String,ContextItem IOCX)] where
    ioCxLookup k a = return (lookup k a)


instance IOContextLookup [IOCX] where
    ioCxLookup k []     = return Nothing
    ioCxLookup k (x:xs) = do l <- ioCxLookup k x
                             case l of
                               Just a  -> return (Just a)
                               Nothing -> ioCxLookup k xs

instance IOContextLookup a => IOContextLookup [a] where
    ioCxLookup k []     = return Nothing
    ioCxLookup k (x:xs) = do l <- ioCxLookup k x
                             case l of
                               Just a  -> return (Just a)
                               Nothing -> ioCxLookup k xs


instance IOContextLookup IOCX where
    ioCxLookup k (IOCX a) = ioCxLookup k a

instance IOContextLookup (ContextItem IOCX) where
    ioCxLookup k v = return (Just v)

instance IOContextLookup CX where
    ioCxLookup k (CX v) = case cxLookup k v of
                            Just y  -> return $ Just (toIOContext y)
                            Nothing -> return Nothing

------------------------------------------------------------------------

class ToIOContext a where
    toIOContext :: a -> ContextItem IOCX

instance ToIOContext (ContextItem CX) where
    toIOContext (ContextPairs a) = ContextPairs (map (\x->IOCX x) a)
    toIOContext a = toIOContext a

instance IOContextLookup a => ToIOContext a where
     toIOContext x = ContextPairs [IOCX x]

instance ToIOContext a => ToIOContext (String,a) where
    toIOContext (k,v) = toIOContext [(k,toIOContext v)]

instance ToIOContext a => ToIOContext [a] where
    toIOContext ls = ContextPairs (map (IOCX . toIOContext) ls)



------------------------------------------------------------------------
-- TEST
------------------------------------------------------------------------

data PetType = Dog | Cat | Bird deriving (Show)
data Pet = Pet {  getPetsType :: PetType, getPetsName :: String , getPetsMother :: Maybe Pet } deriving (Show)

instance ContextLookup Pet where
    cxLookup k pet = case k of
                       "name" -> justcx $ getPetsName pet
                       "type" -> justcx . show $ getPetsType pet
                       "mother" -> case getPetsMother pet of
                                     Just a  -> justcx a
                                     Nothing -> Nothing
                       otherwise -> Nothing



roxy  = Pet Cat "Roxy" Nothing
simon = Pet Cat "Simon" (Just roxy)

template = C.pack "Pets: {@|p <- pets| {{p.name}} \n@}"

data Directory = Directory String deriving (Show)

instance IOContextLookup Directory where
    ioCxLookup k (Directory a) = return $ Just (ContextValue $ C.pack a)