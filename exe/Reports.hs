module Main (main) where

import Network.Wai.Handler.Warp qualified as Warp
import Reports.Server qualified as Server
import Servant.Server.Generic qualified as Sv

main :: IO ()
main = Warp.run 8080 $ Sv.genericServe Server.server

-- mainOld :: IO ()
-- mainOld = do
--   Loaded {..} <- loadFixturesData
--   playersData <- loadPlayerData $ S.fromList $ M.elems ldNames
--   let ts = makeTeamStats ldResults
--   writeReport "players-overview" $ playersOverview playersData
--   forM_ [1, 2, 3, 5, 8] $ \gameCount -> do
--     let fn = "players-points-" <> show gameCount
--     writeReport fn $
--       playersExpectedPointsForGames
--         ts
--         ldFixtures
--         gameCount
--         playersData
--   -- When picking defenders...
--   writeReport "abs-clean-sheets" $ Team.absCleanSheets ts ldFixtures
--   writeReport "sum-clean-sheets" $ Team.sumCleanSheets ts ldFixtures
--   writeReport "abs-conc-points" $ Team.absPointsConc ts ldFixtures
--   writeReport "sum-conc-points" $ Team.sumPointsConc ts ldFixtures
--   -- When picking attacking players...
--   writeReport "abs-def-weakness" $ Team.absDefWeakness ts ldFixtures
--   writeReport "sum-def-weakness" $ Team.sumDefWeakness ts ldFixtures

-- reportsDir :: FilePath
-- reportsDir = "reports"

-- writeReport :: FilePath -> [[T.Text]] -> IO ()
-- writeReport name =
--   fmap (T.intercalate "\t")
--     >>> T.unlines
--     >>> T.writeFile (reportsDir </> name <.> "tsv")
