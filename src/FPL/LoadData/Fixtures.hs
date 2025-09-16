module FPL.LoadData.Fixtures
  ( Team (..),
    Result (..),
    Fixture (..),
    Loaded (..),
    loadFixturesData,
  )
where

import Control.Category ((>>>))
import Control.Monad.State (StateT, evalStateT, modify, state)
import Control.Monad.Trans (lift)
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

data Fixture = Fixture
  { fixHome :: Team,
    fixAway :: Team
  }
  deriving (Show)

data Loaded = Loaded
  { ldNames :: M.Map T.Text Team,
    ldResults :: [(Day, Result)],
    ldFixtures :: [(Day, Fixture)]
  }
  deriving (Show)

loadFixturesData :: IO Loaded
loadFixturesData = do
  ldNames <- loadTeamNames
  ldResults <- loadResults ldNames
  ldFixtures <- loadFixtures ldNames
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

loadFixtures :: M.Map T.Text Team -> IO [(Day, Fixture)]
loadFixtures teams =
  T.readFile "data/truth/results-and-fixtures.md"
    >>= (T.lines >>> parseFixtures teams)

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
          fs <- parseResultsGroup
          (((d,) <$> fs) <>) <$> go
        l -> fail $ "Failed parsing fixtures group: " <> show l

    parseResultsGroup :: StateT [T.Text] IO [Result]
    parseResultsGroup =
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
              (Result {..} :) <$> parseResultsGroup
        line -> fail $ "Failed parsing fixture line: " <> show line

parseFixtures ::
  M.Map T.Text Team ->
  [T.Text] ->
  IO [(Day, Fixture)]
parseFixtures teams =
  dropWhile (/= "# Fixtures")
    >>> drop 1
    >>> evalStateT go
  where
    go :: StateT [T.Text] IO [(Day, Fixture)]
    go =
      modify (dropWhile T.null) >> state (splitAt 1) >>= \case
        [] -> pure []
        [line] | Just dateStr <- T.stripPrefix "## " line -> do
          d :: Day <- iso8601ParseM $ T.unpack dateStr
          fs <- state (span (T.null >>> not)) >>= (traverse parseFixture >>> lift)
          (((d,) <$> fs) <>) <$> go
        line -> fail $ "Parsing fixture group: " <> show line

    parseFixture :: T.Text -> IO Fixture
    parseFixture inLine = maybe
      (fail $ "Parsing fixture: " <> show inLine)
      pure
      $ do
        [home, away] <- T.splitOn " - " <$> T.stripPrefix "- " inLine
        fixHome <- teams M.!? home
        fixAway <- teams M.!? away
        Just Fixture {..}
