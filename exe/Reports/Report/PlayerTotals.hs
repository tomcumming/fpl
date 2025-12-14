module Reports.Report.PlayerTotals (playerTotals) where

import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List qualified as L
import Data.Map qualified as M
import Data.Ord (Down (Down))
import Data.Text qualified as T
import FPL.LoadData.Players (PlayerStats (..), PlayersData, loadPlayerData, pdPlayers)
import FPL.LoadData.TeamNames (loadTeamNames, teamShortName)
import FPL.Stats (PlayerPoints (..), playerApps, playerPoints)
import Lucid qualified as L
import Reports.Api qualified as Api
import Reports.Markup (baseTemplate, greenToRed, showFloatPlaces)
import Servant qualified as Sv
import Servant.Server.Generic qualified as Sv

data Focus
  = Totals
  | PerApp

data PlayerDataRow = PlayerDataRow
  { plrStats :: PlayerStats,
    plrApps :: Word,
    plrPoints :: PlayerPoints
  }

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

playerDataRows :: PlayersData -> [PlayerDataRow]
playerDataRows =
  pdPlayers
    >>> M.elems
    >>> fmap goRow
    >>> filter (plrApps >>> (> 0))
  where
    goRow plrStats =
      PlayerDataRow
        { plrStats,
          plrApps = playerApps plrStats,
          plrPoints = playerPoints plrStats
        }

scorePlayer :: Focus -> PlayerDataRow -> (PlayerDataRow, PlayerScores)
scorePlayer focus pdr =
  ( pdr,
    case focus of
      Totals -> totalScores
      PerApp -> perAppScores
  )
  where
    PlayerStats {..} = plrStats pdr
    PlayerPoints {..} = plrPoints pdr

    totalScores =
      PlayerScores
        { plrTotal = realToFrac psPoints,
          plrCs = realToFrac ppCs,
          plrOther = realToFrac ppOther,
          plrGoals = realToFrac ppGoals
        }

    perAppScores =
      PlayerScores
        { plrTotal = realToFrac psPoints / realToFrac (plrApps pdr),
          plrCs = realToFrac ppCs / realToFrac (plrApps pdr),
          plrOther = realToFrac ppOther / realToFrac (plrApps pdr),
          plrGoals = realToFrac ppGoals / realToFrac (plrApps pdr)
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
  teams <- liftIO loadTeamNames
  playersData <- liftIO $ playerDataRows <$> loadPlayerData teams
  let dataAndScores = scorePlayer focus <$> playersData
  let (mins, maxs) = scoreRanges $ snd <$> dataAndScores
  let sorted =
        L.sortOn (snd >>> plrTotal >>> Down) dataAndScores
          & zip [1 ..]
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
    traverse_ (renderRow mins maxs) sorted
  where
    renderRow ::
      PlayerScores ->
      PlayerScores ->
      (Int, (PlayerDataRow, PlayerScores)) ->
      L.Html ()
    renderRow mins maxs (rank, (PlayerDataRow {..}, ps)) = L.tr_ $ do
      L.td_ $ L.toHtml $ T.show rank
      L.th_ $ L.toHtml $ psName plrStats
      L.td_ $ L.toHtml $ teamShortName $ psTeam plrStats
      L.td_ $ L.toHtml $ T.show $ psPosition plrStats
      L.td_ $ L.toHtml $ showFloatPlaces @Float 1 $ realToFrac (psCost plrStats) / 10
      L.td_ "" -- spacer
      L.td_ $ L.toHtml $ showFloatPlaces @Float 1 $ realToFrac (psMinutes plrStats) / 90
      L.td_ $ L.toHtml $ T.show plrApps
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
