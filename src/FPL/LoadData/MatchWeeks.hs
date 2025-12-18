module FPL.LoadData.MatchWeeks
  ( loadMatchWeeks,
    unsafeMatchWeek,
    MatchWeek,
    Result (..),
    Fixture (..),
  )
where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Function ((&))
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Word (Word8)
import FPL.LoadData.TeamNames (Team, loadTeamNames)

newtype MatchWeek = MatchWeek Int
  deriving newtype (Eq, Ord, Show)

unsafeMatchWeek :: Int -> MatchWeek
unsafeMatchWeek = MatchWeek

data Result = Result
  { resHome :: (Team, Word8),
    resAway :: (Team, Word8)
  }
  deriving (Show)

data Fixture = Fixture
  { fixHome :: Team,
    fixAway :: Team
  }
  deriving (Show)

loadMatchWeeks :: IO (M.Map MatchWeek [Result], M.Map MatchWeek [Fixture])
loadMatchWeeks = do
  teamNames <- loadTeamNames
  vals :: [[Aeson.Value]] <-
    Aeson.eitherDecodeFileStrict "data/snapshot/matchweeks.json"
      >>= either fail pure
  Aeson.parseEither (traverse (readMatchWeek teamNames)) vals
    & either fail (combiner >>> pure)
  where
    readMatchWeek :: M.Map T.Text Team -> [Aeson.Value] -> Aeson.Parser (M.Map MatchWeek [Either Result Fixture])
    readMatchWeek teamNames = \case
      (Aeson.Object o : rows) -> do
        mw <- MatchWeek <$> o Aeson..: "matchWeek"
        M.singleton mw <$> traverse (parseResultOrFixture teamNames) rows
      mw -> fail $ show mw

    parseResultOrFixture :: M.Map T.Text Team -> Aeson.Value -> Aeson.Parser (Either Result Fixture)
    parseResultOrFixture teamNames =
      Aeson.parseJSON @[Aeson.Value] >=> \case
        [homeName, awayName] -> do
          fixHome <- lookupTeam teamNames =<< Aeson.parseJSON homeName
          fixAway <- lookupTeam teamNames =<< Aeson.parseJSON awayName
          pure $ Right $ Fixture {fixHome, fixAway}
        [homeName, homeScore, awayScore, awayName] -> do
          homeTeam <- lookupTeam teamNames =<< Aeson.parseJSON homeName
          awayTeam <- lookupTeam teamNames =<< Aeson.parseJSON awayName
          home <- Aeson.parseJSON homeScore
          away <- Aeson.parseJSON awayScore
          pure $ Left $ Result (homeTeam, home) (awayTeam, away)
        cs -> fail $ show cs

    lookupTeam teamNames name =
      M.lookup name teamNames
        & maybe (fail $ show name) pure

    combiner ::
      [M.Map MatchWeek [Either Result Fixture]] ->
      (M.Map MatchWeek [Result], M.Map MatchWeek [Fixture])
    combiner =
      M.unionsWith (<>) >>> \ins ->
        ( M.filter (null >>> not) $ mapMaybe (either Just (const Nothing)) <$> ins,
          M.filter (null >>> not) $ mapMaybe (either (const Nothing) Just) <$> ins
        )
