module FPL.LoadData.Players
  ( playerName,
    playerPosition,
    playerTeam,
    playerMinutes,
    playerTotalPoints,
    playerCost,
    playerGoals,
    playerAssists,
    playerCleanSheets,
    playerPointsPerGame,
  )
where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key (fromString)
import Data.Aeson.Types qualified as Aeson
import Data.Data (Typeable)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (traceM)
import FPL.Database.Types (Key (..), Player, Position (..), Team, teamShortName, unsafePlayer)
import FPL.LoadData.TeamNames (teamNames)
import Memo qualified
import Text.Read (readMaybe)

loadFirst :: (Typeable val) => Key val -> Memo.Memo Key -> IO val
loadFirst key memo =
  Memo.lookup
    key
    (insertPlayerData memo >> Memo.lookupUnsafe key memo)
    memo

playerName :: Memo.Memo Key -> IO (M.Map Player T.Text)
playerName = loadFirst PlayerName

playerPosition :: Memo.Memo Key -> IO (M.Map Player Position)
playerPosition = loadFirst PlayerPosition

playerTeam :: Memo.Memo Key -> IO (M.Map Player Team)
playerTeam = loadFirst PlayerTeam

playerMinutes :: Memo.Memo Key -> IO (M.Map Player Word)
playerMinutes = loadFirst PlayerMinutes

playerTotalPoints :: Memo.Memo Key -> IO (M.Map Player Int)
playerTotalPoints = loadFirst PlayerTotalPoints

playerCost :: Memo.Memo Key -> IO (M.Map Player Word)
playerCost = loadFirst PlayerCost

playerGoals :: Memo.Memo Key -> IO (M.Map Player Word)
playerGoals = loadFirst PlayerGoals

playerAssists :: Memo.Memo Key -> IO (M.Map Player Word)
playerAssists = loadFirst PlayerAssists

playerCleanSheets :: Memo.Memo Key -> IO (M.Map Player Word)
playerCleanSheets = loadFirst PlayerCleanSheets

playerPointsPerGame :: Memo.Memo Key -> IO (M.Map Player Float)
playerPointsPerGame = loadFirst PlayerPointsPerGame

insertPlayerData :: Memo.Memo Key -> IO ()
insertPlayerData memo = do
  traceM "Loading PlayerData"
  tns <- teamNames memo & fmap M.keysSet
  playersData <- liftIO $ loadPlayerData tns
  Memo.insert PlayerName (psName <$> playersData) memo
  Memo.insert PlayerPosition (psPosition <$> playersData) memo
  Memo.insert PlayerTeam (psTeam <$> playersData) memo
  Memo.insert PlayerMinutes (psMinutes <$> playersData) memo
  Memo.insert PlayerTotalPoints (psPoints <$> playersData) memo
  Memo.insert PlayerCost (psCost <$> playersData) memo
  Memo.insert PlayerGoals (psGoals <$> playersData) memo
  Memo.insert PlayerAssists (psAssists <$> playersData) memo
  Memo.insert PlayerCleanSheets (psCleanSheets <$> playersData) memo
  Memo.insert PlayerPointsPerGame (psPtsPerGame <$> playersData) memo

data PlayerStats = PlayerStats
  { psName :: T.Text,
    psTeam :: Team,
    psMinutes :: Word,
    psPosition :: Position,
    psPoints :: Int,
    psCost :: Word,
    psDefCon :: Word,
    psGoals :: Word,
    psAssists :: Word,
    psCleanSheets :: Word,
    psPtsPerGame :: Float
  }
  deriving (Show)

playerDataPath :: FilePath
playerDataPath = "data/snapshot/player-stats.json"

loadPlayerData :: S.Set Team -> IO (M.Map Player PlayerStats)
loadPlayerData teams =
  Aeson.decodeFileStrict playerDataPath
    >>= maybe (fail "Could not load player data") pure
    >>= (Aeson.parseEither (parsePlayerData teams) >>> pure)
    >>= either (("Invalid player data JSON: " <>) >>> fail) pure

parsePlayerData ::
  S.Set Team ->
  Aeson.Object ->
  Aeson.Parser
    (M.Map Player PlayerStats)
parsePlayerData teams root = do
  teamIds <- parseTeamIds teams =<< root Aeson..: "teams"
  positionIds <- parsePositionIds =<< root Aeson..: "element_types"
  root Aeson..: "elements"
    >>= traverse (parsePlayerStats teamIds positionIds)
    >>= (catMaybes >>> M.fromList >>> pure)

parsePositionIds :: [Aeson.Object] -> Aeson.Parser (M.Map Int Position)
parsePositionIds = traverse go >=> (M.fromList >>> pure)
  where
    go :: Aeson.Object -> Aeson.Parser (Int, Position)
    go obj = do
      posId <- obj Aeson..: "id"
      pos <-
        obj Aeson..: "singular_name_short" >>= \case
          ("GKP" :: T.Text) -> pure GK
          "DEF" -> pure Def
          "MID" -> pure Mid
          "FWD" -> pure Att
          name -> fail $ "Unknown position type: " <> show name
      pure (posId, pos)

parsePlayerStats ::
  M.Map Int Team ->
  M.Map Int Position ->
  Aeson.Object ->
  Aeson.Parser (Maybe (Player, PlayerStats))
parsePlayerStats teamIds posIds obj = do
  playerId <- unsafePlayer <$> obj Aeson..: "id"
  (Aeson.<?> Aeson.Key ("playerId=" <> fromString (show playerId))) $ do
    canSelect <- obj Aeson..: "can_select"
    psName <- obj Aeson..: "web_name"
    psMinutes <- obj Aeson..: "minutes"
    psPosition <-
      obj Aeson..: "element_type"
        >>= ((posIds M.!?) >>> maybe (fail "Invalid position ID") pure)
    psTeam <-
      obj Aeson..: "team"
        >>= ((teamIds M.!?) >>> maybe (fail "Invalid team ID") pure)
    psCost <- obj Aeson..: "now_cost"
    psPoints <- obj Aeson..: "total_points"
    psDefCon <- obj Aeson..: "defensive_contribution"
    psGoals <- obj Aeson..: "goals_scored"
    psAssists <- obj Aeson..: "assists"
    psCleanSheets <- obj Aeson..: "clean_sheets"
    psPtsPerGame <-
      obj Aeson..: "points_per_game"
        >>= (readMaybe >>> maybe (fail "Invalid pts / game") pure)
    pure $
      if canSelect
        then Just (playerId, PlayerStats {..})
        else Nothing

parseTeamIds :: S.Set Team -> [Aeson.Object] -> Aeson.Parser (M.Map Int Team)
parseTeamIds tns = traverse go >=> (M.fromList >>> pure)
  where
    teams =
      S.toList tns
        & fmap (\team -> (teamShortName team, team))
        & M.fromList

    go :: Aeson.Object -> Aeson.Parser (Int, Team)
    go obj = do
      teamId <- obj Aeson..: "id"
      shortName <- obj Aeson..: "short_name"
      team <-
        teams M.!? shortName
          & maybe
            (fail $ "Unrecognised team: " <> T.unpack shortName <> show teams)
            pure
      pure (teamId, team)
