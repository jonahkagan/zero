{-# LANGUAGE TypeSynonymInstances #-}
module Text where

import Data.List
import Data.Tuple

class (Eq c, Show c) => Chr c

data Alphabet c = Alphabet { chars :: [c], seps :: [c] }
type Table ic oc = [(ic, oc)]

type Text c = [c]
instance (Chr c) => Chr (Text c)

alphabet :: [c] -> [c] -> Alphabet c
alphabet chars seps = Alphabet { chars = chars, seps = seps }

instance Chr Char
defaultSeps = " .,;:-?!"
abc :: Alphabet Char
abc = alphabet ['a'..'z'] defaultSeps
one23 :: Alphabet Char
one23 = alphabet ['1'..'9'] defaultSeps
two34 :: Alphabet Char
two34 = alphabet ['2'..'9'] defaultSeps

mapWords :: Eq ic =>
  Alphabet ic -> (Text ic -> Text oc) -> (Text ic -> Text oc)
  -> Text ic -> Text oc
mapWords alpha fword fseps text = helper text
  where
    isSep = flip elem $ seps alpha
    helper [] = []
    helper text =
      let (seps, rest) = span isSep text
          (word, rest') = span (not . isSep) rest
      in fseps seps ++ fword word ++ helper rest'

substChar :: (Chr ic, Chr oc) => Table ic oc -> ic -> oc
substChar table c =
  case lookup c table of
    Just c' -> c'
    Nothing -> error ("char not in alphabet: " ++ show c)

subst :: (Chr ic, Chr oc) => Table ic oc -> Text ic -> Text oc
subst table = map $ substChar table

flipTable :: Table ic oc -> Table oc ic
flipTable = uncurry zip . swap . unzip
