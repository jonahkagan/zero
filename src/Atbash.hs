module Atbash (encipher, decipher) where

import Text
import Cipher

encipher :: (Chr c) => Cipher c c
encipher = subst . buildTable

decipher :: (Chr c) => Cipher c c
decipher = encipher

buildTable :: Alphabet c -> Table c c
buildTable alpha = zip cs (reverse cs) where cs = chars alpha
