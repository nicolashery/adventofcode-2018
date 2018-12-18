module Day07 (partOne) where

import Data.List (notElem, nub, sort)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Text.Parsec (ParseError, parse)
import Text.Parsec.Char (oneOf, string)
import Text.Parsec.Text (Parser)

data Graph a = Graph
  { graphVertices :: [a]
  , graphEdges :: [(a, a)]
  } deriving (Show)

type Step = Char
type Dependency = (Step, Step) -- second depends on first

validSteps :: [Step]
validSteps = ['A'..'Z']

dependencyParser :: Parser Dependency
dependencyParser = do
  _ <- string  "Step "
  first <- oneOf validSteps
  _ <- string " must be finished before step "
  second <- oneOf validSteps
  _ <- string " can begin."
  return (first, second)

parseDependency :: Text -> Dependency
parseDependency s = onLeftError $ parse dependencyParser "" s
  where
    onLeftError :: Either ParseError Dependency -> Dependency
    onLeftError = either (error . show) id

buildGraph :: [Dependency] -> Graph Step
buildGraph dependencies = Graph vertices dependencies
  where
    vertices :: [Step]
    vertices = nub $ map snd dependencies ++ map fst dependencies

noInbound :: (Eq a) => Graph a -> [a]
noInbound (Graph vertices edges) = filter noInbound' vertices
  where
    noInbound' x = notElem x $ map snd edges

removeVertex :: (Eq a) => a -> Graph a -> Graph a
removeVertex v (Graph vertices edges) = Graph vertices' edges'
  where
    vertices' = filter (/= v) vertices
    edges' = filter (\(v1, v2) -> v1 /= v && v2 /= v) edges

topologicalSort :: (Ord a, Show a) => Graph a -> [a]
topologicalSort graph = go [] (sort $ noInbound graph) graph
  where
    go r [] (Graph [] []) = reverse r
    go _ [] _ = error "could not perform topological sort on graph"
    go r (n:ns) g = go (n:r) ns' g'
      where
        ns' = sort $ nub $ ns ++ noInbound g'
        g' = removeVertex n g

partOne' :: [Dependency] -> [Step]
partOne' = topologicalSort . buildGraph

partOne :: Text -> Text
partOne = pack . partOne' . map parseDependency . T.lines
