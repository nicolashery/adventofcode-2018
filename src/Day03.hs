module Day03 where

import Data.List (foldl')
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Parsec (ParseError, many1, parse)
import Text.Parsec.Char (char, digit, string)
import Text.Parsec.Text (Parser)

type FabricSize = Int

type Fabric = Vector (Vector Int)

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

initialFabric :: FabricSize -> Fabric
initialFabric size = V.replicate size (V.replicate size 0)

pointBelongsToClaim :: Claim -> (Int, Int) -> Bool
pointBelongsToClaim c (x, y) =
  x > cX && x <= cX + cW &&
  y > cY && y <= cY + cH
  where
    cX = claimX c
    cY = claimY c
    cW = claimW c
    cH = claimH c

applyClaim :: Fabric -> Claim -> Fabric
applyClaim fabric claim = V.imap (\x ys -> V.imap (f x) ys) fabric
  where
    f :: Int -> Int -> Int -> Int
    f x y count =
      if pointBelongsToClaim claim (x, y) then count + 1 else count

partOne' :: FabricSize -> [Claim] -> Int
partOne' size claims = V.foldl' (\z ys -> V.foldl' (+) z ys) 0 overlap
  where
    initial :: Fabric
    initial = initialFabric size

    overlayed :: Fabric
    overlayed = foldl' applyClaim initial claims

    overlap :: Fabric
    overlap = V.map (\ys -> V.map (\n -> if n > 1 then 1 else 0) ys) overlayed

partOne :: FabricSize -> Text -> Text
partOne fabricSize =
  pack . show . partOne' fabricSize . map parseClaim . T.lines
