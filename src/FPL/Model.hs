module FPL.Model
  ( meanGoalsFor,
    meanGoalsConceded,
    predictedHomeScore,
    predictedAwayScore,
    predictedPointsCleanSheet,
    predictedPointsOther,
    predictedPointsGoals,
  )
where

import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Data (Typeable)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Map.Strict qualified as M
import Data.Maybe (maybeToList)
import Debug.Trace (traceM)
import FPL.Database.Types (Fixture, Key (..), Player, Team)
import FPL.LoadData.MatchWeeks (fixtureAway, fixtureAwayScore, fixtureHome, fixtureHomeScore)
import FPL.LoadData.Players (playerPosition, playerTeam)
import FPL.Rules (pointsForCS)
import FPL.Stats (playerApps, playerGoalPoints, playerOtherPoints)
import Memo qualified

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / realToFrac (length xs)

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

meanGoalsFor :: Memo.Memo Key -> IO (M.Map Team Float)
meanGoalsFor memo =
  Memo.lookup
    MeanGoalsFor
    (calculateMeanGoals memo >> Memo.lookupUnsafe MeanGoalsFor memo)
    memo

meanGoalsConceded :: Memo.Memo Key -> IO (M.Map Team Float)
meanGoalsConceded memo =
  Memo.lookup
    MeanGoalsConceded
    (calculateMeanGoals memo >> Memo.lookupUnsafe MeanGoalsConceded memo)
    memo

predictedHomeScore :: Memo.Memo Key -> IO (M.Map Fixture Float)
predictedHomeScore memo =
  Memo.lookup
    PredictedHomeScore
    (calculatePredictedScores memo >> Memo.lookupUnsafe PredictedHomeScore memo)
    memo

predictedAwayScore :: Memo.Memo Key -> IO (M.Map Fixture Float)
predictedAwayScore memo =
  Memo.lookup
    PredictedAwayScore
    (calculatePredictedScores memo >> Memo.lookupUnsafe PredictedAwayScore memo)
    memo

firstLoadPredicted :: (Typeable val) => Key val -> Memo.Memo Key -> IO val
firstLoadPredicted key memo =
  Memo.lookup
    key
    (calculatePredictedPoints memo >> Memo.lookupUnsafe key memo)
    memo

predictedPointsCleanSheet :: Memo.Memo Key -> IO (M.Map (Player, Fixture) Float)
predictedPointsCleanSheet = firstLoadPredicted PredictedPointsCleanSheet

predictedPointsOther :: Memo.Memo Key -> IO (M.Map (Player, Fixture) Float)
predictedPointsOther = firstLoadPredicted PredictedPointsOther

predictedPointsGoals :: Memo.Memo Key -> IO (M.Map (Player, Fixture) Float)
predictedPointsGoals = firstLoadPredicted PredictedPointsGoals

calculateMeanGoals :: Memo.Memo Key -> IO ()
calculateMeanGoals memo = do
  traceM "Calculating mean goals"
  fhs <- fixtureHomeScore memo
  fas <- fixtureAwayScore memo
  fh <- fixtureHome memo
  fa <- fixtureAway memo
  let goalsFor = fmap (fmap realToFrac >>> mean) $
        M.fromListWith (<>) $ do
          (fixture, homeScore) <- M.assocs fhs
          awayScore <- M.lookup fixture fas & maybeToList
          home <- M.lookup fixture fh & maybeToList
          away <- M.lookup fixture fa & maybeToList
          [(home, [homeScore]), (away, [awayScore])]
  let goalsConceded = fmap (fmap realToFrac >>> mean) $
        M.fromListWith (<>) $ do
          (fixture, homeScore) <- M.assocs fhs
          awayScore <- M.lookup fixture fas & maybeToList
          home <- M.lookup fixture fh & maybeToList
          away <- M.lookup fixture fa & maybeToList
          [(home, [awayScore]), (away, [homeScore])]
  Memo.insert MeanGoalsFor goalsFor memo
  Memo.insert MeanGoalsConceded goalsConceded memo

calculatePredictedScores :: Memo.Memo Key -> IO ()
calculatePredictedScores memo = do
  traceM "Calculating predicted scores"
  fh <- fixtureHome memo
  fa <- fixtureAway memo
  fhs <- fixtureHomeScore memo
  mgf <- meanGoalsFor memo
  mgc <- meanGoalsConceded memo
  let homeGoals = M.fromList $ do
        (fixture, home) <- M.assocs fh
        guard $ M.notMember fixture fhs
        away <- M.lookup fixture fa & maybeToList
        hgf <- M.lookup home mgf & maybeToList
        agc <- M.lookup away mgc & maybeToList
        pure (fixture, (hgf + agc) / 2)
  let awayGoals = M.fromList $ do
        (fixture, home) <- M.assocs fh
        guard $ M.notMember fixture fhs
        away <- M.lookup fixture fa & maybeToList
        hgc <- M.lookup home mgc & maybeToList
        agf <- M.lookup away mgf & maybeToList
        pure (fixture, (agf + hgc) / 2)
  Memo.insert PredictedHomeScore homeGoals memo
  Memo.insert PredictedAwayScore awayGoals memo

calculatePredictedPoints :: Memo.Memo Key -> IO ()
calculatePredictedPoints memo = do
  traceM "Calculating predicted points"

  fixtureHome' <- fixtureHome memo
  fixtureAway' <- fixtureAway memo
  fixtureHomeScore' <- fixtureHomeScore memo
  predictedHomeScore' <- predictedHomeScore memo
  predictedAwayScore' <- predictedAwayScore memo
  playerPosition' <- playerPosition memo
  playerApps' <- playerApps memo
  playerTeam' <- playerTeam memo
  playerOtherPoints' <- playerOtherPoints memo
  playerGoalPoints' <- playerGoalPoints memo
  meanGoalsConceded' <- meanGoalsConceded memo

  let meanGoalsConcedes = mean meanGoalsConceded'
  traceM $ "meanGoalsConcedes: " <> show meanGoalsConcedes

  let teamFutureFixtures = M.fromListWith (<>) $ do
        (fixture, home) <-
          M.withoutKeys fixtureHome' (M.keysSet fixtureHomeScore')
            & M.assocs
        away <- M.lookup fixture fixtureAway' & maybeToList
        [(home, [fixture]), (away, [fixture])]

  let predictedOtherPoints = M.fromList $ do
        (player, otherPoints) <- M.assocs playerOtherPoints'
        team <- M.lookup player playerTeam' & maybeToList
        apps <- M.lookup player playerApps' & maybeToList
        guard $ apps > 0
        let avgPts :: Float = realToFrac otherPoints / realToFrac apps
        fixture <- M.lookup team teamFutureFixtures & fold
        pure ((player, fixture), avgPts)

  let predictedCleanSheetPoints = M.fromList $ do
        (player, team) <- M.assocs playerTeam'
        position <- M.lookup player playerPosition' & maybeToList
        fixture <- M.lookup team teamFutureFixtures & fold
        let isHome = M.lookup fixture fixtureHome' == Just team
        conceding <-
          maybeToList $
            if isHome
              then M.lookup fixture predictedAwayScore'
              else M.lookup fixture predictedHomeScore'
        pure
          ( (player, fixture),
            chanceOfCleanSheet conceding
              * realToFrac (pointsForCS position)
          )

  let predictedGoalPoints = M.fromList $ do
        (player, team) <- M.assocs playerTeam'
        apps <- M.lookup player playerApps' & maybeToList
        meanGoalPoints <-
          M.lookup player playerGoalPoints'
            & fmap (realToFrac >>> (/ realToFrac apps))
            & maybeToList
        fixture <- M.lookup team teamFutureFixtures & fold
        home <- M.lookup fixture fixtureHome' & maybeToList
        away <- M.lookup fixture fixtureAway' & maybeToList
        let opp = if team == home then away else home
        oppConcedes <- M.lookup opp meanGoalsConceded' & maybeToList
        let advantage = oppConcedes / meanGoalsConcedes
        pure ((player, fixture), advantage * meanGoalPoints)

  Memo.insert PredictedPointsCleanSheet predictedCleanSheetPoints memo
  Memo.insert PredictedPointsOther predictedOtherPoints memo
  Memo.insert PredictedPointsGoals predictedGoalPoints memo
