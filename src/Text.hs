module Text where

import Data.List

class (Eq c, Show c) => Chr c
instance Chr Char

data Alphabet a = Alphabet { chars :: [a], seps :: [a] }
type Text a = [a]

alphabet :: [a] -> [a] -> Alphabet a
alphabet chars seps = Alphabet { chars = chars, seps = seps }

mapWords :: (Eq a) => Alphabet a -> (Text a -> Text a) -> Text a -> Text a
mapWords alpha f text = helper text
  where
    isSep = flip elem $ seps alpha
    helper [] = []
    helper text =
      let (seps, rest) = span isSep text
          (word, rest') = span (not . isSep) rest
      in seps ++ f word ++ helper rest'
  {-join (head s) . map f $ split s text-}

basicAlpha = alphabet "abcdefghijklmnopqrstuvwxyz" " "

skipSeps alpha f = mapWords alpha $ f alpha

abc = skipSeps basicAlpha
