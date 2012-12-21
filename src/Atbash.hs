module Atbash (encode, decode) where

import Text
import Cipher

encode :: (Chr c) => Cipher c c
encode alpha = map (flipChar alpha)

decode :: (Chr c) => Cipher c c
decode = encode

flipChar :: (Chr c) => Alphabet c -> c -> c
flipChar alpha c =
  case lookup c $ zip charSet $ reverse charSet of
    Just c' -> c'
    Nothing -> error ("char not in alphabet: " ++ show c)
  where charSet = chars alpha 
