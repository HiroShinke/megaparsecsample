
import System.Environment (getArgs)
import System.FilePath.Posix (replaceExtension)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as LT
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Compiler
import Parser

main :: IO ()
main = do
  args <- getArgs
  let srcPath = args !! 0
  let distPath = replaceExtension srcPath ".ll"
  src <- T.readFile srcPath
  let result = parse prog "example" src
  case result of
    Right ast -> LT.writeFile distPath (compile ast)
    Left e -> putStrLn $ errorBundlePretty e

