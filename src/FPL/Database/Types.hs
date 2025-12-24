{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE Strict #-}

module FPL.Database.Types
  ( Fixture,
    unsafeFixture,
    MatchWeek,
    unsafeMatchWeek,
    Team,
    teamShortName,
    unsafeTeam,
    Player,
    unsafePlayer,
    Position (..),
    Database,
    Key (..),
  )
where

import Data.Kind (Type)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import DoubleMap qualified as DM
import Memo (Memo)

newtype Fixture = Fixture Word
  deriving newtype (Eq, Ord, Show)

unsafeFixture :: Word -> Fixture
unsafeFixture = Fixture

newtype MatchWeek = MatchWeek Word
  deriving newtype (Eq, Ord, Show)

unsafeMatchWeek :: Word -> MatchWeek
unsafeMatchWeek = MatchWeek

newtype Team = Team {teamShortName :: T.Text}
  deriving newtype (Eq, Ord, Show)

unsafeTeam :: T.Text -> Team
unsafeTeam = Team

newtype Player = Player Word
  deriving newtype (Eq, Ord, Show)

unsafePlayer :: Word -> Player
unsafePlayer = Player

data Position
  = GK
  | Def
  | Mid
  | Att
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

type Database = Memo Key

data Key :: Type -> Type where
  TeamNames :: Key (M.Map Team T.Text)
  FixtureMatchWeek :: Key (DM.DMap Fixture MatchWeek)
  FixtureIndex :: Key (M.Map Fixture Word)
  FixtureHome :: Key (M.Map Fixture Team)
  FixtureAway :: Key (M.Map Fixture Team)
  FixtureHomeScore :: Key (M.Map Fixture Word)
  FixtureAwayScore :: Key (M.Map Fixture Word)
  --
  MeanGoalsFor :: Key (M.Map Team Float)
  MeanGoalsConceded :: Key (M.Map Team Float)
  --
  PredictedHomeScore :: Key (M.Map Fixture Float)
  PredictedAwayScore :: Key (M.Map Fixture Float)
  --
  PlayerName :: Key (M.Map Player T.Text)
  PlayerPosition :: Key (M.Map Player Position)
  PlayerTeam :: Key (M.Map Player Team)
  PlayerMinutes :: Key (M.Map Player Word)
  PlayerTotalPoints :: Key (M.Map Player Int)
  PlayerCost :: Key (M.Map Player Word)
  PlayerGoals :: Key (M.Map Player Word)
  PlayerAssists :: Key (M.Map Player Word)
  PlayerCleanSheets :: Key (M.Map Player Word)
  PlayerPointsPerGame :: Key (M.Map Player Float)
  --
  PlayerCleanSheetPoints :: Key (M.Map Player Int)
  PlayerOtherPoints :: Key (M.Map Player Int)
  -- | Points from goal contributions
  PlayerGoalPoints :: Key (M.Map Player Int)
  --
  PlayerApps :: Key (M.Map Player Word)
  --
  PredictedPointsCleanSheet :: Key (M.Map (Player, Fixture) Float)
  PredictedPointsOther :: Key (M.Map (Player, Fixture) Float)
  PredictedPointsGoals :: Key (M.Map (Player, Fixture) Float)

deriving instance Eq (Key t)

deriving instance Ord (Key t)

deriving instance Show (Key t)
