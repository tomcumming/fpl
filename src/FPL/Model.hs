module FPL.Model
  ( Score (..),
    PredictedPoints (..),
    predictScores,
    totalPredicted,
    predictPointsForMatchWeek,
    predictPoints,
  )
where

import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Function ((&))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import FPL.LoadData.MatchWeeks (Fixture (..), MatchWeek)
import FPL.LoadData.Players (PlayerId, PlayerStats (..))
import FPL.LoadData.TeamNames (Team)
import FPL.Rules (pointsForCS)
import FPL.Stats (PlayerPoints (..), TeamStats (..), playerApps, playerPoints, tsScored)

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

data PredictedPoints = PredictedPoints
  { predCs :: Float,
    predOther :: Float,
    predGoals :: Float
  }

totalPredicted :: PredictedPoints -> Float
totalPredicted PredictedPoints {..} = predCs + predOther + predGoals

sumPredictedPoints :: NE.NonEmpty PredictedPoints -> PredictedPoints
sumPredictedPoints = foldr1 $ \p1 p2 ->
  PredictedPoints
    { predCs = predCs p1 + predCs p2,
      predOther = predOther p1 + predOther p2,
      predGoals = predGoals p1 + predGoals p2
    }

predictPointsForMatchWeek ::
  M.Map Team TeamStats ->
  [Fixture] ->
  M.Map PlayerId PlayerStats ->
  M.Map PlayerId PredictedPoints
predictPointsForMatchWeek ts fs = M.mapMaybe goPlayer
  where
    fixtureMap :: M.Map Team [Team]
    fixtureMap = M.fromListWith (<>) $ do
      Fixture {..} <- fs
      [(fixHome, [fixAway]), (fixAway, [fixHome])]

    avgTeamConceded :: Float
    avgTeamConceded =
      sum (realToFrac . tsConceded <$> ts)
        / sum (realToFrac . tsPlayed <$> ts)

    goPlayer :: PlayerStats -> Maybe PredictedPoints
    goPlayer ps@PlayerStats {..} = fmap sumPredictedPoints $ NE.nonEmpty $ do
      oppTeam <- fromMaybe [] $ fixtureMap M.!? psTeam
      let ourStats = ts M.! psTeam
      let oppStats = ts M.! oppTeam

      let predictedConceded =
            realToFrac (tsConceded ourStats + tsScored oppStats)
              / realToFrac (tsPlayed ourStats + tsPlayed oppStats)

      let apps = realToFrac $ playerApps ps
      guard $ apps > 0

      let PlayerPoints {ppOther, ppGoals} = playerPoints ps
      let oppConcededPerGame =
            realToFrac (tsConceded oppStats)
              / realToFrac (tsPlayed oppStats)
      let goalsBonus = oppConcededPerGame / avgTeamConceded

      pure $
        PredictedPoints
          { -- Model clean-sheet points as expected
            predCs =
              chanceOfCleanSheet predictedConceded
                * realToFrac (pointsForCS psPosition),
            -- Presume other points are mostly constant
            predOther = realToFrac ppOther / apps,
            -- Scale player goal points per game by opp defence weakness
            predGoals = (realToFrac ppGoals / apps) * goalsBonus
          }

predictPoints ::
  M.Map Team TeamStats ->
  M.Map MatchWeek [Fixture] ->
  M.Map PlayerId PlayerStats ->
  M.Map PlayerId (M.Map MatchWeek PredictedPoints)
predictPoints ts fs pd =
  goWeek <$> fs
    & M.assocs
    & fmap (\(mw, ps) -> M.singleton mw <$> ps)
    & M.unionsWith (<>)
  where
    goWeek fs' = predictPointsForMatchWeek ts fs' pd
