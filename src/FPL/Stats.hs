module FPL.Stats
  ( TeamStats (..),
    teamStats,
    PlayerPoints (..),
    playerPoints,
    playerApps,
  )
where

import Control.Category ((>>>))
import Data.Foldable (fold)
import Data.Map qualified as M
import FPL.LoadData.MatchWeeks (MatchWeek, Result (..))
import FPL.LoadData.Players (PlayerStats (..))
import FPL.LoadData.TeamNames (Team)
import FPL.Rules (pointsForAssist, pointsForCS, pointsForGoal)

data TeamStats = TeamStats
  { tsPlayed :: Word,
    tsScored :: Word,
    tsConceded :: Word
  }
  deriving (Show)

teamStats :: M.Map MatchWeek [Result] -> M.Map Team TeamStats
teamStats = fold >>> concatMap goResult >>> M.fromListWith combineTs
  where
    combineTs ts1 ts2 =
      TeamStats
        { tsPlayed = tsPlayed ts1 + tsPlayed ts2,
          tsScored = tsScored ts1 + tsScored ts2,
          tsConceded = tsConceded ts1 + tsConceded ts2
        }

    goResult :: Result -> [(Team, TeamStats)]
    goResult Result {resHome = (home, homeGoals), resAway = (away, awayGoals)} =
      let homeGoals' = fromIntegral homeGoals
          awayGoals' = fromIntegral awayGoals
       in [ (home, TeamStats 1 homeGoals' awayGoals'),
            (away, TeamStats 1 awayGoals' homeGoals')
          ]

-- | Breakdown of points into 3 catagories
data PlayerPoints = PlayerPoints
  { ppCs :: Int,
    ppOther :: Int,
    ppGoals :: Int
  }

playerPoints :: PlayerStats -> PlayerPoints
playerPoints PlayerStats {..} = PlayerPoints {..}
  where
    ppCs = fromIntegral $ psCleanSheets * pointsForCS psPosition
    ppGoals =
      fromIntegral $
        psGoals * pointsForGoal psPosition
          + psAssists * pointsForAssist
    ppOther = psPoints - ppCs - ppGoals

playerApps :: PlayerStats -> Word
playerApps PlayerStats {psPtsPerGame, psPoints, psMinutes} =
  maximum
    [ 0,
      round $ realToFrac psPoints / psPtsPerGame,
      round @Float $ realToFrac psMinutes / 90
    ]
