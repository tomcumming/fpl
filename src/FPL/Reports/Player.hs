module FPL.Reports.Player
  ( playersOverview,
    playersExpectedPointsForGames,
  )
where

import Control.Category ((>>>))
import Data.Foldable (fold)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time (Day)
import FPL.LoadData.Fixtures (Fixture, Team (..))
import FPL.LoadData.Players (PlayerStats (..), PlayersData (..))
import FPL.Reports (TeamStats (..), chanceOfCleanSheet, pointsForAssist, pointsForCS, pointsForGoal, teamOpponents, tshow)

data FixtureExpected = FixtureExpected
  { fixtureExpectedGoals :: Double,
    fixtureExpectedConc :: Double
  }
  deriving (Show)

data ExpectedPoints = ExpectedPoints
  { xpOther :: Double,
    xpGoals :: Double,
    xpCS :: Double
  }
  deriving (Show)

instance Semigroup ExpectedPoints where
  e1 <> e2 =
    ExpectedPoints
      { xpOther = xpOther e1 + xpOther e2,
        xpGoals = xpGoals e1 + xpGoals e2,
        xpCS = xpCS e1 + xpCS e2
      }

instance Monoid ExpectedPoints where mempty = ExpectedPoints 0 0 0

xpTotal :: ExpectedPoints -> Double
xpTotal ExpectedPoints {..} = xpOther + xpGoals + xpCS

playerExpectedPoints :: TeamStats -> FixtureExpected -> PlayerStats -> ExpectedPoints
playerExpectedPoints TeamStats {..} FixtureExpected {..} PlayerStats {..} =
  ExpectedPoints
    { xpOther = perGame totalOtherPts,
      xpGoals = xgMult * perGame totalPtsFromGoals,
      xpCS =
        chanceOfCleanSheet fixtureExpectedConc
          * realToFrac (pointsForCS psPosition)
    }
  where
    teamXg :: Double = realToFrac tsScored / realToFrac tsPlayed
    xgMult =
      if teamXg > 0 -- villa lol
        then realToFrac fixtureExpectedGoals / teamXg
        else 1
    perGame x = realToFrac x / realToFrac tsPlayed
    totalPtsFromGoals =
      psGoals * pointsForGoal psPosition
        + psAssists * pointsForAssist
    totalPtsFromCS = psCleanSheets * pointsForCS psPosition
    totalOtherPts = psPoints - fromIntegral (totalPtsFromGoals + totalPtsFromCS)

playersOverview :: PlayersData -> [[T.Text]]
playersOverview = \PlayersData {..} ->
  ["Name", "Team", "Pos", "Minutes", "Pts", "Pts/g", "DefCon/g", "GI", "GI/g", "Cost"]
    : ( M.elems pdPlayers
          & filter (psMinutes >>> (> 0))
          & List.sortOn psPoints
          & fmap goRow
      )
  where
    goRow :: PlayerStats -> [T.Text]
    goRow PlayerStats {..} =
      [ psName,
        unTeam psTeam,
        tshow psPosition,
        tshow psMinutes,
        tshow psPoints,
        tshow @Double $ 90 * realToFrac psPoints / realToFrac psMinutes,
        tshow @Double $ 90 * realToFrac psDefCon / realToFrac psMinutes,
        tshow @Double goalInv,
        tshow @Double $ 90 * goalInv / realToFrac psMinutes,
        tshow @Double $ realToFrac psCost / 10
      ]
      where
        goalInv = realToFrac $ psGoals + psAssists

playersExpectedPointsForGames ::
  M.Map Team TeamStats ->
  [(Day, Fixture)] ->
  Word ->
  PlayersData ->
  [[T.Text]]
playersExpectedPointsForGames teamStats fixtures gameCount =
  pdPlayers
    >>> M.elems
    >>> fmap makeRow
    >>> List.sortOn (snd >>> xpTotal >>> negate)
    >>> fmap
      ( \(PlayerStats {..}, xp@ExpectedPoints {..}) ->
          [ psName,
            unTeam psTeam,
            tshow psPosition,
            tshow $ realToFrac psCost / (10 :: Double),
            tshow $ perGame xpCS,
            tshow $ perGame xpOther,
            tshow $ perGame xpGoals,
            tshow $ perGame $ xpTotal xp
          ]
      )
    >>> (["Name", "Team", "Pos", "Cost", "CS", "Other", "Att", "Total"] :)
  where
    perGame x = x / realToFrac gameCount

    fixtureExpected :: M.Map Team [FixtureExpected]
    fixtureExpected =
      teamStats
        & M.mapWithKey
          ( \team1 ts1 ->
              teamOpponents team1 fixtures
                & take (fromIntegral gameCount)
                & fmap
                  ( \team2 ->
                      let ts2 = teamStats M.! team2
                       in FixtureExpected
                            { fixtureExpectedGoals =
                                realToFrac (tsScored ts1 + tsConc ts2)
                                  / realToFrac (tsPlayed ts1 + tsPlayed ts2),
                              fixtureExpectedConc =
                                realToFrac (tsScored ts2 + tsConc ts1)
                                  / realToFrac (tsPlayed ts1 + tsPlayed ts2)
                            }
                  )
          )

    makeRow :: PlayerStats -> (PlayerStats, ExpectedPoints)
    makeRow ps@PlayerStats {psTeam} =
      ( ps,
        fixtureExpected M.! psTeam
          & fmap
            ( \fe ->
                playerExpectedPoints
                  (teamStats M.! psTeam)
                  fe
                  ps
            )
          & fold
      )
