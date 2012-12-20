import qualified Caesar
import Text

basicAlpha = alphabet "abcdefghijklmnopqrstuvwxyz" " "

skipSeps alpha f = mapWords alpha $ f alpha

abc = skipSeps basicAlpha

main = do
  putStrLn $ abc (Caesar.encode 1) $ abc (Caesar.decode 1) "the"

  
