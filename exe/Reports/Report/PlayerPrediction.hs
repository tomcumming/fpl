module Reports.Report.PlayerPrediction
  ( playerPrediction,
  )
where

import Control.Category ((>>>))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Data.Foldable (fold, sequenceA_)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map qualified as M
import Data.Maybe (catMaybes, maybeToList)
import Data.Ord (Down (Down))
import Data.Text qualified as T
import DoubleMap qualified as DM
import FPL.Database.Types (Key, MatchWeek, Player, Position, Team, teamShortName)
import FPL.LoadData.MatchWeeks (fixtureAway, fixtureHome, fixtureHomeScore, fixtureMatchWeek)
import FPL.LoadData.Players (playerName, playerPosition, playerTeam)
import FPL.Model (predictedPointsCleanSheet, predictedPointsGoals, predictedPointsOther)
import FPL.Stats (playerApps)
import Lucid qualified as L
import Memo qualified
import Reports.Markup (baseTemplate, greenToRed, showFloatPlaces)
import Servant qualified as Sv
import Servant.HTML.Lucid qualified as L

playerPrediction :: Maybe MatchWeek -> Maybe Position -> Sv.Server (Sv.Get '[L.HTML] (L.Html ()))
playerPrediction sortByMw filterByPos = do
  db <- liftIO Memo.empty
  playerName' <- playerName db & liftIO
  playerTeam' <- playerTeam db & liftIO
  playerApps' <- playerApps db & liftIO
  playerPosition' <- playerPosition db & liftIO
  playerData <- makePlayerRows db & liftIO

  let filterPlayer = case filterByPos of
        Nothing -> const True
        Just mw -> \player -> M.lookup player playerPosition' & (== Just mw)
  let sortPlayer = case sortByMw of
        Nothing -> fmap ppTotal >>> sum
        Just mw -> M.lookup mw >>> maybe 0 ppTotal

  let rows =
        playerData
          & M.assocs
          & filter (fst >>> (playerApps' M.!?) >>> maybe False (> 0))
          & filter (fst >>> filterPlayer)
          & List.sortOn (snd >>> sortPlayer >>> Down)

  let ranges = fmap snd rows & makeRanges
  let cols = M.keysSet ranges

  pure $ baseTemplate $ do
    L.div_ $ do
      let links :: [L.Html ()] = (:) (L.a_ [L.href_ $ makeQuery sortByMw Nothing] "All") $
            flip map [minBound .. maxBound :: Position] $ \pos ->
              L.a_
                [L.href_ $ makeQuery sortByMw (Just pos)]
                $ L.toHtml
                $ T.show pos
      sequenceA_ $ List.intersperse " - " links
    L.div_ $ L.a_ [L.href_ $ makeQuery Nothing filterByPos] "Sort by Total"
    L.table_ $ do
      L.thead_ $ L.tr_ $ do
        L.th_ "Rank"
        L.th_ "Player"
        L.th_ "Team"
        L.th_ "Pos"
        L.th_ ""
        forM_ cols $ \mw -> do
          let href = makeQuery (Just mw) filterByPos
          L.th_ $ L.a_ [L.href_ href] $ L.toHtml $ T.show mw
      L.tbody_ $ forM_ (zip [1 :: Int ..] rows) $ \(rank, (player, mws)) -> L.tr_ $ do
        let name = playerName' M.! player
        let team = playerTeam' M.! player
        let position = playerPosition' M.! player
        L.td_ $ L.toHtml $ T.show rank
        L.td_ $ L.toHtml name
        L.td_ $ L.toHtml $ teamShortName team
        L.td_ $ L.toHtml $ T.show position
        L.td_ ""
        forM_ cols $ \mw -> case mws M.!? mw of
          Nothing -> L.td_ "-"
          Just score -> do
            let (minScore, maxScore) = ranges M.! mw
            let x = (ppTotal score - minScore) / (maxScore - minScore)
            let style = "background-color: " <> greenToRed x
            let title =
                  T.unlines
                    [ T.intercalate ", " $ teamShortName <$> ppOpps score,
                      T.intercalate " / " $
                        fmap
                          (showFloatPlaces 2)
                          [ ppCs score,
                            ppOther score,
                            ppGoals score
                          ]
                    ]
            L.td_ [L.title_ title, L.style_ style] $
              L.toHtml $
                showFloatPlaces 1 $
                  ppTotal score

makeQuery :: Maybe MatchWeek -> Maybe Position -> T.Text
makeQuery mw pos =
  [("mw=" <>) . T.show <$> mw, ("pos=" <>) . T.show <$> pos]
    & catMaybes
    & \case
      [] -> "?"
      parts -> (<>) "?" $ T.intercalate "&" parts

data PlayerPoints = PlayerPoints
  { ppCs :: Float,
    ppOther :: Float,
    ppGoals :: Float,
    ppTotal :: Float,
    ppOpps :: [Team]
  }

combinePlayerPoints :: PlayerPoints -> PlayerPoints -> PlayerPoints
combinePlayerPoints p1 p2 =
  PlayerPoints
    { ppCs = ppCs p1 + ppCs p2,
      ppOther = ppOther p1 + ppOther p2,
      ppGoals = ppGoals p1 + ppGoals p2,
      ppTotal = ppTotal p1 + ppTotal p2,
      ppOpps = ppOpps p1 <> ppOpps p2
    }

makePlayerRows :: Memo.Memo Key -> IO (M.Map Player (M.Map MatchWeek PlayerPoints))
makePlayerRows db = do
  playerTeam' <- playerTeam db
  fixtureMatchWeek' <- fixtureMatchWeek db
  fixtureHome' <- fixtureHome db
  fixtureAway' <- fixtureAway db
  fixtureHomeScore' <- fixtureHomeScore db
  predictedPointsCleanSheet' <- predictedPointsCleanSheet db
  predictedPointsOther' <- predictedPointsOther db
  predictedPointsGoals' <- predictedPointsGoals db

  let teamFutureFixtures = M.fromListWith (<>) $ do
        (fixture, home) <-
          M.withoutKeys fixtureHome' (M.keysSet fixtureHomeScore')
            & M.assocs
        away <- M.lookup fixture fixtureAway' & maybeToList
        [(home, [fixture]), (away, [fixture])]

  pure
    $ M.fromListWith
      (M.unionWith combinePlayerPoints)
    $ do
      (player, team) <- M.assocs playerTeam'
      fixture <- M.lookup team teamFutureFixtures & fold
      mw <- fixtureMatchWeek' & DM.forwards & M.lookup fixture & maybeToList
      opp <-
        maybeToList $
          if M.lookup fixture fixtureHome' == Just team
            then M.lookup fixture fixtureAway'
            else M.lookup fixture fixtureHome'
      ppCs <- M.lookup (player, fixture) predictedPointsCleanSheet' & maybeToList
      ppOther <- M.lookup (player, fixture) predictedPointsOther' & maybeToList
      ppGoals <- M.lookup (player, fixture) predictedPointsGoals' & maybeToList
      let ppTotal = ppCs + ppOther + ppGoals
      let ppOpps = [opp]
      pure (player, M.singleton mw PlayerPoints {..})

makeRanges ::
  [M.Map MatchWeek PlayerPoints] ->
  M.Map MatchWeek (Float, Float)
makeRanges predicted =
  M.intersectionWith
    (,)
    (minimum <$> grouped)
    (maximum <$> grouped)
  where
    grouped :: M.Map MatchWeek [Float] =
      predicted
        & concatMap M.assocs
        & fmap (second (ppTotal >>> pure))
        & M.fromListWith (<>)
