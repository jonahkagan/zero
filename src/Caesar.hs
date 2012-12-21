module Caesar (encode, decode, keys) where

import Text
import Cipher
import Data.List

encode :: (Chr c) => Int -> Cipher c c
encode key = rot key

decode :: (Chr c) => Int -> Cipher c c
decode key = rot (-key)

keys :: Alphabet c -> [Int]
keys alpha = [0..length $ chars alpha]

{-rotAll :: String -> [String]-}
{-rotAll ctext = [rot i ctext | i <- [0..alphaLen]]-}

rot :: (Chr c) => Int -> Alphabet c -> Text c -> Text c
rot n alpha = map $ rotChar n alpha

rotChar :: (Chr c) => Int -> Alphabet c -> c -> c
rotChar n alpha c =
  case elemIndex c charSet of
    Just i -> charSet !! mod (n + i) (length charSet)
    Nothing -> error ("char not in alphabet: " ++ show c)
  where charSet = chars alpha
