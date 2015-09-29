import System.Environment

import Parser

main = do
  (filename:_) <- getArgs
  contents <- readFile filename
  print $ parseJson filename contents
