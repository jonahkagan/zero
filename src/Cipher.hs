module Cipher where

import Text

type Cipher ic oc = Alphabet ic -> Text ic -> Text oc

applyCipher :: (Chr ic, Chr oc) =>
  Alphabet ic -> Cipher ic oc -> (Text ic -> Text oc)
  -> Text ic -> Text oc
applyCipher alpha cipher fseps = mapWords alpha (cipher alpha) fseps

cipherFrom :: (Chr c) => Alphabet c -> Cipher c c -> Text c -> Text c
cipherFrom alpha cipher = applyCipher alpha cipher id
