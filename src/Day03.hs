module Day03 (partOne, partTwo) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.List (all, findIndex)
import Data.Maybe (maybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.Parsec (ParseError, many1, parse)
import Text.Parsec.Char (char, digit, string)
import Text.Parsec.Text (Parser)

type FabricSize = Int

type Fabric = Vector Int

data Claim = Claim
  { claimId :: Text
  , claimX :: Int
  , claimY :: Int
  , claimW :: Int
  , claimH :: Int
  } deriving (Eq, Show)

claimParser :: Parser Claim
claimParser = do
  _ <- char '#'
  cId <- pack <$> many1 digit
  _ <- string " @ "
  x <- int
  _ <- char ','
  y <- int
  _ <- string ": "
  w <- int
  _ <- char 'x'
  h <- int
  return $ Claim
    { claimId = cId
    , claimX = x
    , claimY = y
    , claimW = w
    , claimH = h
    }

  where
    int :: Parser Int
    int = read <$> many1 digit

parseClaim :: Text -> Claim
parseClaim input = onLeftError $ parse claimParser "" input
  where
    onLeftError :: Either ParseError Claim -> Claim
    onLeftError = either (error . showParseError) id

    showParseError :: ParseError -> String
    showParseError err = concat
      [ "Failed to parse claim '"
      , unpack input
      , "'\n"
      , show err
      ]

claimPoints :: Claim -> [(Int, Int)]
claimPoints c = [(x, y) | x <- [xMin..xMax], y <- [yMin..yMax]]
  where
    xMin = claimX c
    xMax = claimX c + claimW c - 1
    yMin = claimY c
    yMax = claimY c + claimH c - 1

claimFabricIndices :: FabricSize -> Claim -> [Int]
claimFabricIndices size = map (pointToFabricIndex size) . claimPoints

pointToFabricIndex :: FabricSize -> (Int, Int) -> Int
pointToFabricIndex size (x, y) = y * size + x

fabricWithOverlappedClaims :: FabricSize -> [[Int]] -> Fabric
fabricWithOverlappedClaims size claimsIndices = runST $ do
  let fabricLength = size * size
  fabric <- MV.replicate fabricLength 0
  forM_ claimsIndices $ \claimIndices ->
    forM_ claimIndices $ \i ->
      MV.modify fabric (+1) i
  forM_ [0 .. fabricLength - 1] $ \i ->
    MV.modify fabric (\n -> if n > 1 then 1 else 0) i
  V.freeze fabric

partOne' :: FabricSize -> [Claim] -> Int
partOne' size claims =
  V.foldl' (+) 0 $ fabricWithOverlappedClaims size claimsIndices

  where
    claimsIndices :: [[Int]]
    claimsIndices = map (claimFabricIndices size) claims

partTwo' :: FabricSize -> [Claim] -> Int
partTwo' size claims =
  maybe notFoundError indexToClaimId $ findIndex isClaimIntact claimsIndices

  where
    isClaimIntact :: [Int] -> Bool
    isClaimIntact = all (\i -> overlapped ! i == 0)

    overlapped :: Fabric
    overlapped = fabricWithOverlappedClaims size claimsIndices

    claimsIndices :: [[Int]]
    claimsIndices = map (claimFabricIndices size) claims

    indexToClaimId :: Int -> Int
    indexToClaimId i = i + 1

    notFoundError = error "could not find an intact claim"

partOne :: FabricSize -> Text -> Text
partOne fabricSize =
  pack . show . partOne' fabricSize . map parseClaim . T.lines

partTwo :: FabricSize -> Text -> Text
partTwo fabricSize =
  pack . show . partTwo' fabricSize . map parseClaim . T.lines
