module FPL.Reports
  ( tshow,
    pointsForAssist,
    pointsForGoal,
    pointsForCS,
    TeamStats (..),
    makeTeamStats,
    showPercentage,
    poissonPdf,
    chanceOfCleanSheet,
    runningSum,
    teamOpponents,
  )
where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time (Day)
import FPL.LoadData.Fixtures (Fixture (..), Result (..), Team)
import FPL.LoadData.Players (Position (..))

tshow :: (Show a) => a -> T.Text
tshow = show >>> T.pack

pointsForAssist :: Word
pointsForAssist = 3

pointsForGoal :: Position -> Word
pointsForGoal = \case
  GK -> 10
  Def -> 6
  Mid -> 5
  Att -> 4

pointsForCS :: Position -> Word
pointsForCS = \case
  GK -> 4
  Def -> 4
  Mid -> 1
  Att -> 0

data TeamStats = TeamStats
  { tsPlayed :: Word,
    tsScored :: Word,
    tsConc :: Word
  }
  deriving (Show)

instance Semigroup TeamStats where
  ts1 <> ts2 =
    TeamStats
      { tsPlayed = tsPlayed ts1 + tsPlayed ts2,
        tsScored = tsScored ts1 + tsScored ts2,
        tsConc = tsConc ts1 + tsConc ts2
      }

makeTeamStats :: [(Day, Result)] -> M.Map Team TeamStats
makeTeamStats = fmap (snd >>> asTeamStats) >>> M.unionsWith (<>)
  where
    asTeamStats :: Result -> M.Map Team TeamStats
    asTeamStats Result {..} =
      M.fromList
        [ (resHome, TeamStats {tsPlayed = 1, tsScored = hs, tsConc = as}),
          (resAway, TeamStats {tsPlayed = 1, tsScored = as, tsConc = hs})
        ]
      where
        (hs, as) = resScore

showPercentage :: Double -> T.Text
showPercentage p = p * 100 & round & tshow @Int & (<> "%")

factorial :: (Ord t, Num t) => t -> t
factorial n
  | n <= 1 = 1
  | otherwise = n * factorial (n - 1)

poissonPdf :: (Floating a, Ord a) => a -> Word -> a
poissonPdf lambda k = lambda ** k' * exp (negate lambda) / factorial k'
  where
    k' = realToFrac k

-- | Chance of clean sheet given the usual goals/game
chanceOfCleanSheet :: Double -> Double
chanceOfCleanSheet xg = poissonPdf xg 0

runningSum :: [Double] -> [Double]
runningSum = scanl (+) 0 >>> drop 1

teamOpponents :: Team -> [(Day, Fixture)] -> [Team]
teamOpponents team = concatMap $ \case
  (_, Fixture {..}) ->
    if
      | fixHome == team -> [fixAway]
      | fixAway == team -> [fixHome]
      | otherwise -> []
