module FPL.Model
  ( Score (..),
    predictScores,
  )
where

import Control.Category ((>>>))
import Data.Map qualified as M
import Data.Ratio ((%))
import FPL.LoadData.MatchWeeks (Fixture (..), MatchWeek)
import FPL.LoadData.TeamNames (Team)
import FPL.Stats (TeamStats (..))

factorial :: (Ord t, Num t) => t -> t
factorial n
  | n <= 1 = 1
  | otherwise = n * factorial (n - 1)

poissonPdf :: (Floating a, Ord a) => a -> Word -> a
poissonPdf lambda k = lambda ** k' * exp (negate lambda) / factorial k'
  where
    k' = realToFrac k

-- | Chance of clean sheet given the usual goals/game
chanceOfCleanSheet :: (Floating a, Ord a) => a -> a
chanceOfCleanSheet xg = poissonPdf xg 0

data Score a = Score
  { scoreFor :: a,
    scoreAgainst :: a
  }
  deriving (Show)

predictScores ::
  M.Map Team TeamStats ->
  M.Map MatchWeek [Fixture] ->
  M.Map Team (M.Map MatchWeek [(Team, Score Float)])
predictScores ts =
  M.assocs
    >>> concatMap (\(mw, fs) -> (mw,) <$> fs)
    >>> concatMap (uncurry goFixture)
    >>> M.fromListWith (M.unionWith (<>))
  where
    goFixture :: MatchWeek -> Fixture -> [(Team, M.Map MatchWeek [(Team, Score Float)])]
    goFixture mw Fixture {fixHome, fixAway} =
      [ (fixHome, M.singleton mw [(fixAway, Score homePred awayPred)]),
        (fixAway, M.singleton mw [(fixHome, Score awayPred homePred)])
      ]
      where
        tsHome = ts M.! fixHome
        tsAway = ts M.! fixAway

        homeGoals :: Float = realToFrac $ tsScored tsHome % tsPlayed tsHome
        homeConc :: Float = realToFrac $ tsConceded tsHome % tsPlayed tsHome
        awayGoals :: Float = realToFrac $ tsScored tsAway % tsPlayed tsAway
        awayConc :: Float = realToFrac $ tsConceded tsAway % tsPlayed tsAway

        homePred = (homeGoals + awayConc) / 2
        awayPred = (awayGoals + homeConc) / 2
