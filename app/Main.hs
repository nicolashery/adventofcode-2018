module Main where

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (die)

import qualified Day01

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("01":_) -> TIO.interact Day01.solve
    _ ->  die "Must supply day to run as argument (01-25)"
