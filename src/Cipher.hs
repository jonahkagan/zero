module Cipher where

import Text

type Cipher c = Alphabet c -> Text c -> Text c

applyCipher :: (Chr c) => Alphabet c -> Cipher c -> Text c -> Text c
applyCipher alpha f = mapWords alpha $ f alpha

abc :: Cipher Char -> Text Char -> Text Char
abc = applyCipher basicAlpha
