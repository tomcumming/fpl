module Main (main) where

import Control.Category ((>>>))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T
import FPL.LoadData.Fixtures (Loaded (..), loadFixturesData)
import FPL.LoadData.Players (loadPlayerData)
import FPL.Reports (makeTeamStats)
import FPL.Reports.Player (playersOverview)
import FPL.Reports.Team qualified as Team
import System.FilePath ((<.>), (</>))

main :: IO ()
main = do
  Loaded {..} <- loadFixturesData
  playersData <- loadPlayerData $ S.fromList $ M.elems ldNames
  let ts = makeTeamStats ldResults
  writeReport "players-overview" $ playersOverview playersData
  -- When picking defenders...
  writeReport "abs-clean-sheets" $ Team.absCleanSheets ts ldFixtures
  writeReport "sum-clean-sheets" $ Team.sumCleanSheets ts ldFixtures
  writeReport "abs-conc-points" $ Team.absPointsConc ts ldFixtures
  writeReport "sum-conc-points" $ Team.sumPointsConc ts ldFixtures
  -- When picking attacking players...
  writeReport "abs-def-weakness" $ Team.absDefWeakness ts ldFixtures
  writeReport "sum-def-weakness" $ Team.sumDefWeakness ts ldFixtures

reportsDir :: FilePath
reportsDir = "reports"

writeReport :: FilePath -> [[T.Text]] -> IO ()
writeReport name =
  fmap (T.intercalate "\t")
    >>> T.unlines
    >>> T.writeFile (reportsDir </> name <.> "tsv")
