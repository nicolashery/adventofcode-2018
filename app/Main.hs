module Main where

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (die)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("01":"p1":_) -> TIO.interact Day01.partOne
    ("01":"p2":_) -> TIO.interact Day01.partTwo
    ("02":"p1":_) -> TIO.interact Day02.partOne
    ("02":"p2":_) -> TIO.interact Day02.partTwo
    ("03":"p1":_) -> TIO.interact $ Day03.partOne 1000
    ("03":"p2":_) -> TIO.interact $ Day03.partTwo 1000
    ("04":"p1":_) -> TIO.interact Day04.partOne
    ("04":"p2":_) -> TIO.interact Day04.partTwo
    ("05":"p1":_) -> TIO.interact Day05.partOne
    ("05":"p2":_) -> TIO.interact Day05.partTwo
    _ ->  die "Must supply day (01-25) and part (p1, p2) as arguments"
