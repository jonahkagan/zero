import qualified Caesar
import Text


main = do
  putStrLn $ abc (Caesar.encode 1) $ abc (Caesar.decode 1) "the"

  
