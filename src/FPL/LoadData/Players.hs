module FPL.LoadData.Players
  ( PlayersData (..),
    loadPlayerData,
    PlayerId,
    PlayerStats (..),
    Position (..),
  )
where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Function ((&))
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text qualified as T
import FPL.LoadData.TeamNames (Team, teamFromShortName)
import Text.Read (readMaybe)

newtype PlayerId = PlayerId Int
  deriving (Eq, Ord, Enum)
  deriving newtype (Show)

newtype PlayersData = PlayersData
  { pdPlayers :: M.Map PlayerId PlayerStats
  }

data Position
  = GK
  | Def
  | Mid
  | Att
  deriving (Eq, Ord, Show)

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

loadPlayerData :: M.Map T.Text Team -> IO PlayersData
loadPlayerData teams =
  Aeson.decodeFileStrict playerDataPath
    >>= maybe (fail "Could not load player data") pure
    >>= (Aeson.parseEither (parsePlayerData teams) >>> pure)
    >>= either (("Invalid player data JSON: " <>) >>> fail) pure

parsePlayerData :: M.Map T.Text Team -> Aeson.Object -> Aeson.Parser PlayersData
parsePlayerData teams root = do
  teamIds <- parseTeamIds teams =<< root Aeson..: "teams"
  positionIds <- parsePositionIds =<< root Aeson..: "element_types"
  pdPlayers <-
    root Aeson..: "elements"
      >>= traverse (parsePlayerStats teamIds positionIds)
      >>= (catMaybes >>> M.fromList >>> pure)
  pure PlayersData {pdPlayers}

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
  Aeson.Parser (Maybe (PlayerId, PlayerStats))
parsePlayerStats teamIds posIds obj = do
  playerId <- PlayerId <$> obj Aeson..: "id"
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

parseTeamIds :: M.Map T.Text Team -> [Aeson.Object] -> Aeson.Parser (M.Map Int Team)
parseTeamIds teams = traverse go >=> (M.fromList >>> pure)
  where
    go :: Aeson.Object -> Aeson.Parser (Int, Team)
    go obj = do
      teamId <- obj Aeson..: "id"
      teamShortName <- obj Aeson..: "short_name"
      team <-
        teamFromShortName teams teamShortName
          & maybe
            (fail $ "Unrecognised team: " <> T.unpack teamShortName)
            pure
      pure (teamId, team)
