module Caesar (encipher, decipher, keys) where

import Text
import Cipher
import Data.List

encipher :: (Chr c) => Int -> Cipher c c
encipher key = rot key

decipher :: (Chr c) => Int -> Cipher c c
decipher key = rot (-key)

keys :: Alphabet c -> [Int]
keys alpha = [0..length $ chars alpha]

{-rotAll :: String -> [String]-}
{-rotAll ctext = [rot i ctext | i <- [0..alphaLen]]-}

rot :: (Chr c) => Int -> Alphabet c -> Text c -> Text c
rot n alpha = subst $ buildTable n alpha

buildTable :: Int -> Alphabet c -> Table c c
buildTable n alpha =
  zip charSet (drop shift $ cycle charSet)
  where charSet = chars alpha
        shift = n `mod` length charSet
