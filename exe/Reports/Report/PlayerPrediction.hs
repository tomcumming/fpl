module Reports.Report.PlayerPrediction (playerPrediction) where

import Control.Category ((>>>))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map qualified as M
import Data.Ord (Down (Down))
import Data.Text qualified as T
import FPL.LoadData.MatchWeeks
  ( Fixture (..),
    MatchWeek,
    loadMatchWeeks,
    unsafeMatchWeek,
  )
import FPL.LoadData.Players (PlayerId, loadPlayerData, pdPlayers, psName, psPosition, psTeam)
import FPL.LoadData.TeamNames (Team, loadTeamNames, teamShortName)
import FPL.Model (PredictedPoints (..), predictPoints, totalPredicted)
import FPL.Stats (teamStats)
import Lucid qualified as L
import Reports.Markup (baseTemplate, greenToRed, showFloatPlaces)
import Servant qualified as Sv
import Servant.HTML.Lucid qualified as L

playerPrediction :: Maybe Int -> Sv.Server (Sv.Get '[L.HTML] (L.Html ()))
playerPrediction sortByMw = do
  teams <- liftIO loadTeamNames
  playersData <- liftIO $ loadPlayerData teams
  (results, fixtures) <- liftIO loadMatchWeeks
  let fixturesMap = makeFixturesMap fixtures
  let ts = teamStats results
  let predicted = predictPoints ts fixtures playersData

  let rowSort = case sortByMw of
        Nothing -> fmap totalPredicted >>> sum >>> Down
        Just mw -> (M.! unsafeMatchWeek mw) >>> totalPredicted >>> Down
  let rows = M.assocs predicted & List.sortOn (snd >>> rowSort)
  let cols = M.keys fixtures

  let ranges = makeRanges predicted

  pure $ baseTemplate $ do
    L.div_ $ L.a_ [L.href_ "./prediction"] "Sort by Total"
    L.table_ $ do
      L.thead_ $ L.tr_ $ do
        L.th_ "Rank"
        L.th_ "Player"
        L.th_ "Team"
        L.th_ "Pos"
        L.th_ ""
        forM_ cols $ \mw -> do
          let href = "?mw=" <> T.show mw
          L.th_ $ L.a_ [L.href_ href] $ L.toHtml $ T.show mw
      L.tbody_ $ forM_ (zip [1 :: Int ..] rows) $ \(rank, (pid, mws)) -> L.tr_ $ do
        let ps = pdPlayers playersData M.! pid
        L.td_ $ L.toHtml $ T.show rank
        L.td_ $ L.toHtml $ psName ps
        L.td_ $ L.toHtml $ teamShortName $ psTeam ps
        L.td_ $ L.toHtml $ T.show $ psPosition ps
        L.td_ ""
        forM_ cols $ \mw -> case mws M.!? mw of
          Nothing -> L.td_ "-"
          Just score -> do
            let (minScore, maxScore) = ranges M.! mw
            let x = (totalPredicted score - minScore) / (maxScore - minScore)
            let style = "background-color: " <> greenToRed x
            let opps = (fixturesMap M.! mw) M.! psTeam ps
            let title =
                  T.unlines
                    [ T.intercalate ", " $ teamShortName <$> opps,
                      T.intercalate " / " $
                        fmap
                          (showFloatPlaces 1)
                          [ predCs score,
                            predOther score,
                            predGoals score
                          ]
                    ]
            L.td_ [L.title_ title, L.style_ style] $
              L.toHtml $
                showFloatPlaces 1 $
                  totalPredicted score

makeFixturesMap :: M.Map MatchWeek [Fixture] -> M.Map MatchWeek (M.Map Team [Team])
makeFixturesMap = fmap $ \fs -> M.fromListWith (<>) $ do
  Fixture {..} <- fs
  [(fixHome, [fixAway]), (fixAway, [fixHome])]

makeRanges ::
  M.Map PlayerId (M.Map MatchWeek PredictedPoints) ->
  M.Map MatchWeek (Float, Float)
makeRanges predicted =
  M.intersectionWith
    (,)
    (minimum <$> grouped)
    (maximum <$> grouped)
  where
    grouped :: M.Map MatchWeek [Float] =
      predicted
        & M.elems
        & concatMap M.assocs
        & fmap (second (totalPredicted >>> pure))
        & M.fromListWith (<>)
