module FPL.Reports.Team
  ( absCleanSheets,
    sumCleanSheets,
    absDefWeakness,
    sumDefWeakness,
    absPointsConc,
    sumPointsConc,
  )
where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time (Day)
import FPL.LoadData.Fixtures (Fixture (..), Team (..))
import FPL.Reports (TeamStats (..), poissonPdf, runningSum, showPercentage, teamOpponents, tshow)

-- | Chance that t1 will keep a clean sheet against t2
cleanSheetProbs ::
  M.Map Team TeamStats ->
  [(Day, Fixture)] ->
  [(Team, [Double])]
cleanSheetProbs teamStats fixtures =
  flip fmap (M.keys teamStats) $ \team -> (team, row team)
  where
    row team = teamOpponents team fixtures & fmap (cleanSheetProb team)
    cleanSheetProb :: Team -> Team -> Double
    cleanSheetProb t1 t2 = poissonPdf avg 0
      where
        t1Conc :: Double =
          let TeamStats {..} = teamStats M.! t1
           in realToFrac tsConc / realToFrac tsPlayed
        t2Score :: Double =
          let TeamStats {..} = teamStats M.! t2
           in realToFrac tsScored / realToFrac tsPlayed
        avg = (t1Conc + t2Score) / 2

absCleanSheets :: M.Map Team TeamStats -> [(Day, Fixture)] -> [[T.Text]]
absCleanSheets teamStats =
  cleanSheetProbs teamStats
    >>> fmap (\(team, ps) -> unTeam team : fmap showPercentage ps)

sumCleanSheets :: M.Map Team TeamStats -> [(Day, Fixture)] -> [[T.Text]]
sumCleanSheets teamStats =
  cleanSheetProbs teamStats
    >>> fmap (\(team, ps) -> unTeam team : fmap tshow (runningSum ps))

-- | You lose 1 point for each 2 goals conceeded
concPoints ::
  M.Map Team TeamStats ->
  [(Day, Fixture)] ->
  [(Team, [Double])]
concPoints teamStats fixtures =
  flip fmap (M.keys teamStats) $ \team -> (team, row team)
  where
    row team = teamOpponents team fixtures & fmap (concPointSum team)
    concPointSum :: Team -> Team -> Double
    concPointSum t1 t2 = sum $ do
      k <- [2 .. 8]
      let pts = k `div` 2
      [poissonPdf avg k * realToFrac pts]
      where
        t1Conc :: Double =
          let TeamStats {..} = teamStats M.! t1
           in realToFrac tsConc / realToFrac tsPlayed
        t2Score :: Double =
          let TeamStats {..} = teamStats M.! t2
           in realToFrac tsScored / realToFrac tsPlayed
        avg = (t1Conc + t2Score) / 2

absPointsConc :: M.Map Team TeamStats -> [(Day, Fixture)] -> [[T.Text]]
absPointsConc teamStats =
  concPoints teamStats
    >>> fmap (\(team, ps) -> unTeam team : fmap tshow ps)

sumPointsConc :: M.Map Team TeamStats -> [(Day, Fixture)] -> [[T.Text]]
sumPointsConc teamStats =
  concPoints teamStats
    >>> fmap (\(team, ps) -> unTeam team : fmap tshow (runningSum ps))

defWeekness :: M.Map Team TeamStats -> [(Day, Fixture)] -> [(Team, [Double])]
defWeekness teamStats fixtures =
  flip fmap (M.keys teamStats) $ \team -> (team, row team)
  where
    row team =
      teamOpponents team fixtures & fmap teamDefWeakness

    teamDefWeakness :: Team -> Double
    teamDefWeakness =
      (teamStats M.!) >>> \TeamStats {..} ->
        realToFrac tsConc / realToFrac tsPlayed

absDefWeakness :: M.Map Team TeamStats -> [(Day, Fixture)] -> [[T.Text]]
absDefWeakness teamStats =
  defWeekness teamStats
    >>> fmap (\(team, ps) -> unTeam team : fmap tshow ps)

sumDefWeakness :: M.Map Team TeamStats -> [(Day, Fixture)] -> [[T.Text]]
sumDefWeakness teamStats =
  defWeekness teamStats
    >>> fmap (\(team, ps) -> unTeam team : fmap tshow (runningSum ps))
