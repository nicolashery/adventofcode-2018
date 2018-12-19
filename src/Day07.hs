module Day07 (partOne, partTwo) where

import Data.Char (ord)
import Data.List (notElem, nub, sort, sortOn)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
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

type Duration = Int
type GraphWithDurations = (Graph Step, Map Step Duration)

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

topologicalSort :: (Ord a) => Graph a -> [a]
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

stepInitialDuration :: Duration -> Step -> Duration
stepInitialDuration overhead c = ord c - ord 'A' + 1 + overhead

buildGraphWithDurations :: Duration -> [Dependency] -> GraphWithDurations
buildGraphWithDurations overhead dependencies = (graph, durations)
  where
    graph :: Graph Step
    graph = buildGraph dependencies

    durations :: Map Step Duration
    durations = M.fromList $ map stepWithInitialDuration vertices

    stepWithInitialDuration :: Step -> (Step, Duration)
    stepWithInitialDuration s = (s, stepInitialDuration overhead s)

    vertices :: [Step]
    vertices = graphVertices graph

timeToCompletion :: Int -> GraphWithDurations -> Duration
timeToCompletion workers (graph, durations) =
  go 0 (sort $ noInbound graph) graph durations
  where
    go :: Duration -> [Step] -> Graph Step -> Map Step Duration -> Duration
    go t [] (Graph [] []) _ = t
    go _ [] _ _ = error "could not compute time to completion for graph"
    go t ns g d = go t' ns' g' d'
      where
        t' = t + elapsed
        ns' = nub . (++) inProgress' . sort $ noInbound g'
        g' = removeVertex completed g
        d' = foldr (M.adjust $ flip (-) elapsed) d inProgress
        elapsed = d ! completed
        inProgress' = filter (/= completed) inProgress
        completed =
          fst . head . sortOn snd . map (\n -> (n, d ! n)) $ inProgress
        inProgress = take workers ns

partTwo' :: Int -> Duration -> [Dependency] -> Duration
partTwo' workers overhead =
  timeToCompletion workers . buildGraphWithDurations overhead

partOne :: Text -> Text
partOne = pack . partOne' . map parseDependency . T.lines

partTwo :: Int -> Duration -> Text -> Text
partTwo workers overhead =
  pack . show . partTwo' workers overhead . map parseDependency . T.lines
