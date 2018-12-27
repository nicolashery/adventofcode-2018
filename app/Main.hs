module Main where

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (die)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09

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
    ("06":"p1":_) -> TIO.interact $ Day06.partOne 100
    ("06":"p2":_) -> TIO.interact $ Day06.partTwo 300 10000
    ("07":"p1":_) -> TIO.interact Day07.partOne
    ("07":"p2":_) -> TIO.interact $ Day07.partTwo 5 60
    ("08":"p1":_) -> TIO.interact Day08.partOne
    ("08":"p2":_) -> TIO.interact Day08.partTwo
    ("09":"p1":_) -> TIO.interact Day09.partOne
    ("09":"p2":_) -> TIO.interact Day09.partTwo
    _ ->  die "Must supply day (01-25) and part (p1, p2) as arguments"
