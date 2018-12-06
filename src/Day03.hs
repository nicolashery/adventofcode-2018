module Day03 where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Vector (Vector)
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

pointBelongsToClaim :: Claim -> (Int, Int) -> Bool
pointBelongsToClaim c (x, y) =
  x >= cX && x < cX + cW &&
  y >= cY && y < cY + cH
  where
    cX = claimX c
    cY = claimY c
    cW = claimW c
    cH = claimH c

claimPoints :: Claim -> Vector (Int, Int)
claimPoints c = V.generate (cW * cH) f
  where
    f :: Int -> (Int, Int)
    f i = (cX + i `mod` cW, cY + i `mod` cH)

    cX = claimX c
    cY = claimY c
    cW = claimW c
    cH = claimH c

claimFabricIndices :: FabricSize -> Claim -> Vector Int
claimFabricIndices size = V.map (pointToFabricIndex size) . claimPoints

pointToFabricIndex :: FabricSize -> (Int, Int) -> Int
pointToFabricIndex size (x, y) = y * size + x

fabricWithOverlappedClaims :: FabricSize -> [Claim] -> Fabric
fabricWithOverlappedClaims size claims = runST $ do
  let fabricLength = size * size
  fabric <- MV.replicate fabricLength 0
  forM_ claims $ \claim ->
    forM_ (claimFabricIndices size claim) $ \i ->
      MV.modify fabric (+1) i
  forM_ [0 .. fabricLength - 1] $ \i ->
    MV.modify fabric (\n -> if n > 1 then 1 else 0) i
  V.freeze fabric

partOne' :: FabricSize -> [Claim] -> Int
partOne' size claims = V.foldl' (+) 0 $ fabricWithOverlappedClaims size claims

partOne :: FabricSize -> Text -> Text
partOne fabricSize =
  pack . show . partOne' fabricSize . map parseClaim . T.lines
