module Keypad where

import Text
import Cipher
import Data.List

-- Decoding really only works if characters (groups of numbers) are separated,
-- otherwise you can't know how adjacent groups of the same number split up.

encipher :: (Chr ic, Chr oc) => Alphabet oc -> Cipher ic oc
encipher outAlpha inAlpha text = 
  concat $ subst (buildTable inAlpha outAlpha) text

decipher :: (Chr ic, Chr oc) => Alphabet ic -> Cipher oc ic
decipher inAlpha outAlpha text =
  subst (flipTable $ buildTable inAlpha outAlpha) (groupNums text)

groupNums :: Chr c => Text c -> Text [c]
groupNums text = group text

-- TODO parameterize over schemas
defaultSchema = [3,3,3,3,3,4,3,4]

buildTable :: Alphabet ic -> Alphabet oc -> Table ic (Text oc)
buildTable inAlpha outAlpha =
  zip (chars inAlpha)
      (buildGroups defaultSchema $ chars outAlpha)

buildGroups :: [Int] -> [a] -> [[a]]
buildGroups [] [] = []
buildGroups _ [] = error "schema cannot be shorter than alphabet"
buildGroups [] _ = error "alphabet cannot be shorter than schema"
buildGroups (n:schema) (c:chars) =
  [replicate i c | i <- [1..n]] ++ buildGroups schema chars
