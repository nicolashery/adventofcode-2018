module Day03 where

import Data.Bool (bool)
import Data.List (foldl')
import Data.Matrix (Matrix, elementwise, matrix, prettyMatrix, transpose, zero)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.Parsec (ParseError, many1, parse)
import Text.Parsec.Char (char, digit, string)
import Text.Parsec.Text (Parser)

type FabricSize = Int

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

putMatrixLn :: (Show a) => Matrix a -> IO ()
putMatrixLn = putStrLn . prettyMatrix . transpose

matrixAddition :: (Num a) => Matrix a -> Matrix a -> Matrix a
matrixAddition = elementwise (+)

initialFabric :: FabricSize -> Matrix Int
initialFabric size = zero size size

claimToMatrix :: FabricSize -> Claim -> Matrix Int
claimToMatrix size c = matrix size size generator
  where
    generator :: (Int, Int) -> Int
    generator (x, y) = bool 0 1 pointBelongsToClaim
      where
        pointBelongsToClaim =
          x > cX && x <= cX + cW &&
          y > cY && y <= cY + cH
        cX = claimX c
        cY = claimY c
        cW = claimW c
        cH = claimH c

partOne' :: FabricSize -> [Claim] -> Int
partOne' size claims = foldl' (+) 0 overlap
  where
    overlayed :: Matrix Int
    overlayed = foldl' f (initialFabric size) claims
      where f b a = matrixAddition b (claimToMatrix size a)

    overlap :: Matrix Int
    overlap = fmap (\n -> if n > 1 then 1 else 0) overlayed

partOne :: FabricSize -> Text -> Text
partOne fabricSize =
  pack . show . partOne' fabricSize . map parseClaim . T.lines
