module Main where

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (die)

import qualified Day01

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("01":"p1":_) -> TIO.interact Day01.partOne
    ("01":"p2":_) -> TIO.interact Day01.partTwo
    _ ->  die "Must supply day (01-25) and part (p1, p2) as arguments"
