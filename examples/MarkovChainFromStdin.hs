import System.Random
import Data.MarkovChain
import Control.Applicative
import System.Environment

main = do
  sentenceLength <- read . head <$> getArgs
  contents <- getContents
  let mc = markovChain $ words contents
  let gen = mkStdGen (sentenceLength * 139)
  let sentence = traverse gen mc sentenceLength
  putStrLn $ unwords sentence
