module Cipher where

import Text

type Cipher ic oc = Alphabet ic -> Text ic -> Text oc

applyCipher :: (Chr ic, Chr oc) =>
  Alphabet ic -> Cipher ic oc -> (Text ic -> Text oc)
  -> Text ic -> Text oc
applyCipher alpha cipher fseps = mapWords alpha (cipher alpha) fseps

applySameCharCipher :: (Chr c) =>
  Alphabet c -> Cipher c c -> Text c -> Text c
applySameCharCipher alpha cipher = applyCipher alpha cipher id

fromAbc :: Cipher Char Char -> Text Char -> Text Char
fromAbc = applySameCharCipher abc

