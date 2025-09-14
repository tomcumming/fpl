module FPL.LoadData
  ( Team (..),
    Result (..),
    Loaded (..),
    loadData,
  )
where

import Control.Category ((>>>))
import Control.Monad.State (StateT, evalStateT, state)
import Data.Char (isDigit)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Text.Read (readMaybe)

newtype Team = Team {unTeam :: T.Text}
  deriving (Eq, Ord)
  deriving newtype (Show)

data Result = Result
  { resHome :: Team,
    resAway :: Team,
    resScore :: (Word, Word)
  }
  deriving (Show)

data Loaded = Loaded
  { ldNames :: M.Map T.Text Team,
    ldResults :: [(Day, Result)]
  }
  deriving (Show)

loadData :: IO Loaded
loadData = do
  ldNames <- loadTeamNames
  ldResults <- loadResults ldNames
  pure Loaded {..}

loadTeamNames :: IO (M.Map T.Text Team)
loadTeamNames =
  T.readFile "data/truth/team-names.tsv"
    >>= (T.lines >>> traverse parseLine >>> fmap M.fromList)
  where
    parseLine =
      T.splitOn "\t" >>> \case
        [code, name] -> pure (name, Team code)
        cells -> fail $ "Could not parse " <> show cells

loadResults :: M.Map T.Text Team -> IO [(Day, Result)]
loadResults teams =
  T.readFile "data/truth/results-and-fixtures.md"
    >>= (T.lines >>> parseResults teams)

parseResults :: M.Map T.Text Team -> [T.Text] -> IO [(Day, Result)]
parseResults teams = evalStateT $ do
  state (splitAt 1) >>= \case
    ["# Results"] -> pure ()
    _ -> fail "Expected results header"
  go
  where
    go :: StateT [T.Text] IO [(Day, Result)]
    go = do
      _ <- state $ span T.null
      state (splitAt 1) >>= \case
        [] -> pure []
        ["# Fixtures"] -> pure []
        [line] | Just dateStr <- T.stripPrefix "## " line -> do
          d :: Day <- iso8601ParseM $ T.unpack dateStr
          fs <- parseFixtures
          (((d,) <$> fs) <>) <$> go
        l -> fail $ "Failed parsing fixtures group: " <> show l

    parseFixtures :: StateT [T.Text] IO [Result]
    parseFixtures =
      state (splitAt 1) >>= \case
        [] -> pure []
        [""] -> pure []
        [line']
          | Just line <- T.stripPrefix "- " line',
            [homeStr, awayStr] <- T.splitOn " - " line,
            (homeName, homeScoreStr) <- T.span (isDigit >>> not) homeStr,
            (awayScoreStr, awayName) <- T.span isDigit awayStr,
            Just homeScore <- readMaybe $ T.unpack homeScoreStr,
            Just awayScore <- readMaybe $ T.unpack awayScoreStr,
            Just resHome <- teams M.!? T.strip homeName,
            Just resAway <- teams M.!? T.strip awayName,
            resScore <- (homeScore, awayScore) ->
              (Result {..} :) <$> parseFixtures
        line -> fail $ "Failed parsing fixture line: " <> show line
