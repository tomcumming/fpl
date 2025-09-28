module FPL.LoadData.Players
  ( PlayersData (..),
    loadPlayerData,
    PlayerId,
    PlayerStats (..),
    Position (..),
  )
where

import Control.Category ((>>>))
import Control.Monad (unless, (>=>))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Data.String (fromString)
import Data.Text qualified as T
import FPL.LoadData.Fixtures (Team (Team))

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
    psCleanSheets :: Word
  }
  deriving (Show)

playerDataPath :: FilePath
playerDataPath = "data/truth/player-stats-snapshot.json"

loadPlayerData :: S.Set Team -> IO PlayersData
loadPlayerData teams =
  Aeson.decodeFileStrict playerDataPath
    >>= maybe (fail "Could not load player data") pure
    >>= (Aeson.parseEither (parsePlayerData teams) >>> pure)
    >>= either (("Invalid player data JSON: " <>) >>> fail) pure

parsePlayerData :: S.Set Team -> Aeson.Object -> Aeson.Parser PlayersData
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
    pure $
      if canSelect
        then Just (playerId, PlayerStats {..})
        else Nothing

parseTeamIds :: S.Set Team -> [Aeson.Object] -> Aeson.Parser (M.Map Int Team)
parseTeamIds teams = traverse go >=> (M.fromList >>> pure)
  where
    go :: Aeson.Object -> Aeson.Parser (Int, Team)
    go obj = do
      teamId <- obj Aeson..: "id"
      teamShortName <- obj Aeson..: "short_name"
      let team = Team teamShortName
      unless (team `S.member` teams) $
        fail $
          "Unrecognised team: " <> T.unpack teamShortName
      pure (teamId, team)
