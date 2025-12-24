module Reports.Report.PlayerTotals
  ( playerTotals,
  )
where

import Control.Category ((>>>))
import Control.Monad (forM_, guard)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.List qualified as L
import Data.Map qualified as M
import Data.Ord (Down (Down))
import Data.Text qualified as T
import FPL.Database.Types (Player, Position, Team, teamShortName)
import FPL.LoadData.Players (playerCost, playerMinutes, playerName, playerPosition, playerTeam)
import FPL.Stats (playerApps, playerCleanSheetPoints, playerGoalPoints, playerOtherPoints)
import Lucid qualified as L
import Memo qualified
import Reports.Api qualified as Api
import Reports.Markup (baseTemplate, greenToRed, showFloatPlaces)
import Servant qualified as Sv
import Servant.Server.Generic qualified as Sv

data Focus
  = Totals
  | PerApp

data PlayerScores = PlayerScores
  { plrTotal :: Float,
    plrCs :: Float,
    plrOther :: Float,
    plrGoals :: Float
  }

playerTotals :: Api.PlayerTotalsApi Sv.AsServer
playerTotals =
  Api.PlayerTotalsApi
    { apiPlayerTotalsAbsolute = renderPlayerTotals Totals,
      apiPlayerTotalsPer90 = renderPlayerTotals PerApp
    }

scorePlayer ::
  Focus ->
  M.Map Player Int ->
  M.Map Player Int ->
  M.Map Player Int ->
  M.Map Player Word ->
  Player ->
  Maybe PlayerScores
scorePlayer
  focus
  pcsp
  pop
  pg
  pa
  player = do
    ppCs <- M.lookup player pcsp
    ppOther <- M.lookup player pop
    ppGoals <- M.lookup player pg
    let psPoints = sum [ppCs, ppOther, ppGoals]
    plrApps <- M.lookup player pa
    guard $ plrApps > 0
    pure $ case focus of
      Totals ->
        PlayerScores
          { plrTotal = realToFrac psPoints,
            plrCs = realToFrac ppCs,
            plrOther = realToFrac ppOther,
            plrGoals = realToFrac ppGoals
          }
      PerApp ->
        PlayerScores
          { plrTotal = realToFrac psPoints / realToFrac plrApps,
            plrCs = realToFrac ppCs / realToFrac plrApps,
            plrOther = realToFrac ppOther / realToFrac plrApps,
            plrGoals = realToFrac ppGoals / realToFrac plrApps
          }

scoreRanges :: [PlayerScores] -> (PlayerScores, PlayerScores)
scoreRanges ps =
  ( PlayerScores
      { plrTotal = minimum $ plrTotal <$> ps,
        plrCs = minimum $ plrCs <$> ps,
        plrOther = minimum $ plrOther <$> ps,
        plrGoals = minimum $ plrGoals <$> ps
      },
    PlayerScores
      { plrTotal = maximum $ plrTotal <$> ps,
        plrCs = maximum $ plrCs <$> ps,
        plrOther = maximum $ plrOther <$> ps,
        plrGoals = maximum $ plrGoals <$> ps
      }
  )

renderPlayerTotals :: Focus -> Sv.Handler (L.Html ())
renderPlayerTotals focus = do
  db <- liftIO Memo.empty
  players <- playerName db & fmap M.keysSet & liftIO

  playerScores <- liftIO $ do
    pcsp <- playerCleanSheetPoints db
    pop <- playerOtherPoints db
    pg <- playerGoalPoints db
    pa <- playerApps db
    M.fromSet (scorePlayer focus pcsp pop pg pa) players
      & M.mapMaybe id
      & pure
  let (mins, maxs) = playerScores & M.elems & scoreRanges
  let sorted =
        L.sortOn (snd >>> plrTotal >>> Down) (M.assocs playerScores)
          & zip [1 ..]

  playerName' <- liftIO $ playerName db
  playerTeam' <- liftIO $ playerTeam db
  playerPosition' <- liftIO $ playerPosition db
  playerCost' <- liftIO $ playerCost db
  playerMinutes' <- liftIO $ playerMinutes db
  playerApps' <- liftIO $ playerApps db

  pure $ baseTemplate $ L.table_ $ do
    L.thead_ $ L.tr_ $ do
      L.th_ "Rank"
      L.th_ "Name"
      L.th_ "Team"
      L.th_ "Pos"
      L.th_ "Cost"
      L.th_ "" -- spacer
      L.th_ "M/90"
      L.th_ "Apps"
      L.th_ ""
      L.th_ "CS"
      L.th_ "Other"
      L.th_ "Goals"
      L.th_ ""
      L.th_ "Total"
    forM_ sorted $
      renderRow
        playerName'
        playerTeam'
        playerPosition'
        playerCost'
        playerMinutes'
        playerApps'
        mins
        maxs
  where
    renderRow ::
      M.Map Player T.Text ->
      M.Map Player Team ->
      M.Map Player Position ->
      M.Map Player Word ->
      M.Map Player Word ->
      M.Map Player Word ->
      PlayerScores ->
      PlayerScores ->
      (Int, (Player, PlayerScores)) ->
      L.Html ()
    renderRow
      playerName'
      playerTeam'
      playerPosition'
      playerCost'
      playerMinutes'
      playerApps'
      mins
      maxs
      (rank, (player, ps)) = L.tr_ $ do
        (name, team, pos, cost, minutes, apps) <- maybe
          (error $ "Getting stats for player " <> show player)
          pure
          $ do
            name <- M.lookup player playerName'
            team <- M.lookup player playerTeam'
            pos <- M.lookup player playerPosition'
            cost <- M.lookup player playerCost'
            minutes <- M.lookup player playerMinutes'
            apps <- M.lookup player playerApps'
            pure (name, team, pos, cost, minutes, apps)

        L.td_ $ L.toHtml $ T.show rank
        L.th_ $ L.toHtml name
        L.td_ $ L.toHtml $ teamShortName team
        L.td_ $ L.toHtml $ T.show pos
        L.td_ $ L.toHtml $ showFloatPlaces @Float 1 $ realToFrac cost / 10
        L.td_ "" -- spacer
        L.td_ $ L.toHtml $ showFloatPlaces @Float 1 $ realToFrac minutes / 90
        L.td_ $ L.toHtml $ T.show apps
        L.td_ ""
        renderScore plrCs
        renderScore plrOther
        renderScore plrGoals
        L.td_ "" -- spacer
        renderScore plrTotal
        where
          renderScore :: (PlayerScores -> Float) -> L.Html ()
          renderScore selector =
            let score = selector ps
                min_ = selector mins
                max_ = selector maxs
                r = (score - min_) / (max_ - min_)
                places = case focus of
                  Totals -> 0
                  PerApp -> 1
                s = showFloatPlaces places score
                style = "background-color: " <> greenToRed r
             in L.td_ [L.style_ style] $ L.toHtml s
