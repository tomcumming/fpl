module Main (main) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (Day)
import FPL.LoadData
  ( Fixture (..),
    Loaded (..),
    Result (..),
    Team,
    loadData,
    unTeam,
  )
import System.FilePath ((<.>), (</>))

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

main :: IO ()
main = do
  Loaded {..} <- loadData
  let ts = makeTeamStats ldResults
  writeReport "abs-clean-sheets" $ absCleanSheets ts ldFixtures
  writeReport "sum-clean-sheets" $ sumCleanSheets ts ldFixtures
  writeReport "abs-conc-points" $ absPointsConc ts ldFixtures
  writeReport "sum-conc-points" $ sumPointsConc ts ldFixtures
  writeReport "abs-expected-goals" $ absExpectedGoals ts ldFixtures
  writeReport "sum-expected-goals" $ sumExpectedGoals ts ldFixtures

reportsDir :: FilePath
reportsDir = "reports"

writeReport :: FilePath -> [[T.Text]] -> IO ()
writeReport name =
  fmap (T.intercalate "\t")
    >>> T.unlines
    >>> T.writeFile (reportsDir </> name <.> "tsv")

tshow :: (Show a) => a -> T.Text
tshow = show >>> T.pack

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

runningSum :: [Double] -> [Double]
runningSum = scanl (+) 0 >>> drop 1

teamOpponents :: Team -> [(Day, Fixture)] -> [Team]
teamOpponents team = concatMap $ \case
  (_, Fixture {..}) ->
    if
      | fixHome == team -> [fixAway]
      | fixAway == team -> [fixHome]
      | otherwise -> []

-- | Chance that t1 will keep a clean sheet against t2
cleanSheetProb :: M.Map Team TeamStats -> Team -> Team -> Double
cleanSheetProb teamStats t1 t2 = poissonPdf avg 0
  where
    t1Conc :: Double =
      let TeamStats {..} = teamStats M.! t1
       in realToFrac tsConc / realToFrac tsPlayed
    t2Score :: Double =
      let TeamStats {..} = teamStats M.! t2
       in realToFrac tsScored / realToFrac tsPlayed
    avg = (t1Conc + t2Score) / 2

cleanSheetProbs ::
  M.Map Team TeamStats ->
  [(Day, Fixture)] ->
  [(Team, [Double])]
cleanSheetProbs teamStats fixtures =
  flip fmap (M.keys teamStats) $ \team -> (team, row team)
  where
    row team =
      teamOpponents team fixtures
        & fmap (cleanSheetProb teamStats team)

absCleanSheets :: M.Map Team TeamStats -> [(Day, Fixture)] -> [[T.Text]]
absCleanSheets teamStats =
  cleanSheetProbs teamStats
    >>> fmap (\(team, ps) -> unTeam team : fmap showPercentage ps)

sumCleanSheets :: M.Map Team TeamStats -> [(Day, Fixture)] -> [[T.Text]]
sumCleanSheets teamStats =
  cleanSheetProbs teamStats
    >>> fmap (\(team, ps) -> unTeam team : fmap tshow (runningSum ps))

-- | You lose 1 point for each 2 goals conceeded
concPointSum :: M.Map Team TeamStats -> Team -> Team -> Double
concPointSum teamStats t1 t2 = sum $ do
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

concPoints ::
  M.Map Team TeamStats ->
  [(Day, Fixture)] ->
  [(Team, [Double])]
concPoints teamStats fixtures =
  flip fmap (M.keys teamStats) $ \team -> (team, row team)
  where
    row team =
      teamOpponents team fixtures
        & fmap (concPointSum teamStats team)

absPointsConc :: M.Map Team TeamStats -> [(Day, Fixture)] -> [[T.Text]]
absPointsConc teamStats =
  concPoints teamStats
    >>> fmap (\(team, ps) -> unTeam team : fmap tshow ps)

sumPointsConc :: M.Map Team TeamStats -> [(Day, Fixture)] -> [[T.Text]]
sumPointsConc teamStats =
  concPoints teamStats
    >>> fmap (\(team, ps) -> unTeam team : fmap tshow (runningSum ps))

teamExpectedGoals :: M.Map Team TeamStats -> Team -> Team -> Double
teamExpectedGoals teamStats t1 t2 = (t1Score + t2Conc) / 2
  where
    t1Score :: Double =
      let TeamStats {..} = teamStats M.! t1
       in realToFrac tsScored / realToFrac tsPlayed
    t2Conc :: Double =
      let TeamStats {..} = teamStats M.! t2
       in realToFrac tsConc / realToFrac tsPlayed

expectedGoals :: M.Map Team TeamStats -> [(Day, Fixture)] -> [(Team, [Double])]
expectedGoals teamStats fixtures =
  flip fmap (M.keys teamStats) $ \team -> (team, row team)
  where
    row team =
      teamOpponents team fixtures
        & fmap (teamExpectedGoals teamStats team)

absExpectedGoals :: M.Map Team TeamStats -> [(Day, Fixture)] -> [[T.Text]]
absExpectedGoals teamStats =
  expectedGoals teamStats
    >>> fmap (\(team, ps) -> unTeam team : fmap tshow ps)

sumExpectedGoals :: M.Map Team TeamStats -> [(Day, Fixture)] -> [[T.Text]]
sumExpectedGoals teamStats =
  expectedGoals teamStats
    >>> fmap (\(team, ps) -> unTeam team : fmap tshow (runningSum ps))
