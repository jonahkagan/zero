import qualified Caesar
import Cipher

main = do
  putStrLn $ abc (Caesar.encode 1) $ abc (Caesar.decode 1) "the"

  
