module Caesar (
    encode
  , decode
  , keys
) where

import Text
import Data.List

encode :: (Chr a) => Int -> Alphabet a -> Text a -> Text a
encode key = rot key

decode :: (Chr a) => Int -> Alphabet a -> Text a -> Text a
decode key = rot (-key)

keys :: Alphabet a -> [Int]
keys alpha = [0..length $ chars alpha]

{-rotAll :: String -> [String]-}
{-rotAll ctext = [rot i ctext | i <- [0..alphaLen]]-}

rot :: (Chr a) => Int -> Alphabet a -> Text a -> Text a
rot n alpha = map $ rotChar n alpha

rotChar :: (Chr a) => Int -> Alphabet a -> a -> a
rotChar n alpha c =
  case elemIndex c charSet of
    Nothing -> error ("char not in alphabet: " ++ show c)
    Just i -> charSet !! mod (n + i) (length charSet)
  where charSet = chars alpha

