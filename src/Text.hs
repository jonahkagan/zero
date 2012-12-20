module Text where

import Data.List

class (Eq c, Show c) => Chr c

data Alphabet c = Alphabet { chars :: [c], seps :: [c] }
type Text c = [c]

alphabet :: [c] -> [c] -> Alphabet c
alphabet chars seps = Alphabet { chars = chars, seps = seps }

instance Chr Char
basicAlpha :: Alphabet Char
basicAlpha = alphabet "abcdefghijklmnopqrstuvwxyz" " "

mapWords :: (Eq c) => Alphabet c -> (Text c -> Text c) -> Text c -> Text c
mapWords alpha f text = helper text
  where
    isSep = flip elem $ seps alpha
    helper [] = []
    helper text =
      let (seps, rest) = span isSep text
          (word, rest') = span (not . isSep) rest
      in seps ++ f word ++ helper rest'

