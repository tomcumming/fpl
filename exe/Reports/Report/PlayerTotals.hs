module Reports.Report.PlayerTotals (playerTotals) where

import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import FPL.LoadData.Fixtures qualified as LD
import FPL.LoadData.Players qualified as LD
import FPL.Reports (pointsForAssist, pointsForCS, pointsForGoal)
import Lucid qualified as L
import Reports.Markup (baseTemplate)
import Servant qualified as Sv
import Servant.HTML.Lucid qualified as L

data PlayerTotals = PlayerTotals
  { ptsName :: T.Text,
    ptsTeam :: LD.Team,
    ptsPos :: LD.Position,
    ptsCost :: Double,
    ptsMinutes :: Word,
    ptsTotal :: Int,
    ptsGoals :: Int,
    ptsCs :: Int,
    ptsOther :: Int
  }

data ScoreMaxes = ScoreMaxes
  { smTotal :: Int,
    smGoals :: Int,
    smCs :: Int,
    smOther :: Int
  }

calcMaxes :: [PlayerTotals] -> ScoreMaxes
calcMaxes pts =
  ScoreMaxes
    { smTotal = pts & fmap ptsTotal & maximum,
      smGoals = pts & fmap ptsGoals & maximum,
      smCs = pts & fmap ptsCs & maximum,
      smOther = pts & fmap ptsOther & maximum
    }

calcPlayerTotal :: LD.PlayerStats -> PlayerTotals
calcPlayerTotal LD.PlayerStats {..} =
  PlayerTotals
    { ptsName = psName,
      ptsTeam = psTeam,
      ptsPos = psPosition,
      ptsCost = realToFrac psCost / 10,
      ptsTotal = psPoints,
      ptsMinutes = psMinutes,
      ..
    }
  where
    ptsGoals =
      fromIntegral $
        psGoals * pointsForGoal psPosition
          + psAssists * pointsForAssist
    ptsCs = fromIntegral $ psCleanSheets * pointsForCS psPosition
    ptsOther = psPoints - (ptsGoals + ptsCs)

playerTotals :: Sv.Server (Sv.Get '[L.HTML] (L.Html ()))
playerTotals = do
  LD.Loaded {ldNames} <- liftIO LD.loadFixturesData
  LD.PlayersData {..} <-
    liftIO $
      LD.loadPlayerData $
        S.fromList $
          M.elems ldNames
  let dataRows =
        M.elems pdPlayers
          & fmap calcPlayerTotal
          & filter (ptsMinutes >>> (> 0))
  let maxes = calcMaxes dataRows
  let rows =
        List.sortOn ptsTotal dataRows
          & reverse
          & fmap (renderRow maxes)
  pure $ baseTemplate $ L.table_ $ do
    L.thead_ $ L.tr_ $ do
      L.th_ "Name"
      L.th_ "Team"
      L.th_ "Pos"
      L.th_ "Cost"
      L.th_ "" -- spacer
      L.th_ "CS"
      L.th_ "Other"
      L.th_ "Goals"
      L.th_ ""
      L.th_ "Total"
    L.tbody_ $ sequence_ rows
  where
    renderRow :: ScoreMaxes -> PlayerTotals -> L.Html ()
    renderRow ScoreMaxes {..} PlayerTotals {..} = L.tr_ $ do
      L.th_ $ L.toHtml ptsName
      L.td_ $ L.toHtml $ LD.unTeam ptsTeam
      L.td_ $ L.toHtml $ T.show ptsPos
      L.td_ $ L.toHtml $ T.show ptsCost
      L.td_ "" -- spacer
      renderScore smCs ptsCs
      renderScore smOther ptsOther
      renderScore smGoals ptsGoals
      L.td_ "" -- spacer
      renderScore smTotal ptsTotal

    renderScore :: Int -> Int -> L.Html ()
    renderScore sm s =
      L.td_
        [L.style_ style]
        $ L.toHtml
        $ T.show s
      where
        hue =
          realToFrac s / realToFrac sm
            & (* 120)
            & T.show @Double
        bgColor = "hsl(" <> hue <> " 100% 50% / 0.25)"
        style = "background-color: " <> bgColor
