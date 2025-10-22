module Reports.Report.PlayerTotals (playerTotals) where

import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map qualified as M
import Data.Ratio ((%))
import Data.Set qualified as S
import Data.Text qualified as T
import FPL.LoadData.Fixtures qualified as LD
import FPL.LoadData.Players qualified as LD
import FPL.Reports (pointsForAssist, pointsForCS, pointsForGoal)
import Lucid qualified as L
import Reports.Api qualified as Api
import Reports.Markup (baseTemplate, showFloatPlaces)
import Servant qualified as Sv
import Servant.HTML.Lucid qualified as L
import Servant.Server.Generic qualified as Sv

minMinutes :: Word
minMinutes = 90

playerTotals :: Api.PlayerTotalsApi Sv.AsServer
playerTotals =
  Api.PlayerTotalsApi
    { apiPlayerTotalsAbsolute = playerTotalsAbsolute,
      apiPlayerTotalsPer90 = playerTotalsPer90
    }

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

data ScoreMaxes a = ScoreMaxes
  { smTotal :: a,
    smGoals :: a,
    smCs :: a,
    smOther :: a,
    smMins :: a
  }

playerTotalsAbsolute :: Sv.Server (Sv.Get '[L.HTML] (L.Html ()))
playerTotalsAbsolute = do
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
          & zip [1 ..]
          & fmap (uncurry (renderRow maxes))
  pure $ baseTemplate $ L.table_ $ do
    L.thead_ $ L.tr_ $ do
      L.th_ "Rank"
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
    renderRow :: ScoreMaxes Int -> Int -> PlayerTotals -> L.Html ()
    renderRow ScoreMaxes {..} rank PlayerTotals {..} = L.tr_ $ do
      L.td_ $ L.toHtml $ T.show rank
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

    calcMaxes :: [PlayerTotals] -> ScoreMaxes Int
    calcMaxes pts =
      ScoreMaxes
        { smTotal = pts & fmap ptsTotal & maximum,
          smGoals = pts & fmap ptsGoals & maximum,
          smCs = pts & fmap ptsCs & maximum,
          smOther = pts & fmap ptsOther & maximum,
          smMins = pts & fmap (ptsMinutes >>> fromIntegral) & maximum
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

playerTotalsPer90 :: Sv.Server (Sv.Get '[L.HTML] (L.Html ()))
playerTotalsPer90 = do
  LD.Loaded {ldNames} <- liftIO LD.loadFixturesData
  LD.PlayersData {..} <-
    liftIO $
      LD.loadPlayerData $
        S.fromList $
          M.elems ldNames
  let dataRows =
        M.elems pdPlayers
          & fmap calcPlayerTotal
          & filter (ptsMinutes >>> (> minMinutes))
  let maxes = calcMaxes dataRows
  let rows =
        dataRows
          & List.sortOn (\p -> ptsTotal p & perMin p)
          & reverse
          & zip [1 ..]
          & fmap (uncurry (renderRow maxes))
  pure $ baseTemplate $ L.table_ $ do
    L.thead_ $ L.tr_ $ do
      L.th_ "Rank"
      L.th_ "Name"
      L.th_ "Team"
      L.th_ "Pos"
      L.th_ "Cost"
      L.th_ "" -- spacer
      L.th_ "Mins"
      L.td_ "" -- spacer
      L.th_ "CS"
      L.th_ "Other"
      L.th_ "Goals"
      L.th_ ""
      L.th_ "Total"
    L.tbody_ $ sequence_ rows
  where
    renderRow :: ScoreMaxes Rational -> Int -> PlayerTotals -> L.Html ()
    renderRow ScoreMaxes {..} rank PlayerTotals {..} = L.tr_ $ do
      L.td_ $ L.toHtml $ T.show rank
      L.th_ $ L.toHtml ptsName
      L.td_ $ L.toHtml $ LD.unTeam ptsTeam
      L.td_ $ L.toHtml $ T.show ptsPos
      L.td_ $ L.toHtml $ T.show ptsCost
      L.td_ "" -- spacer
      renderMinutes smMins ptsMinutes
      L.td_ "" -- spacer
      renderScore smCs ptsCs ptsMinutes
      renderScore smOther ptsOther ptsMinutes
      renderScore smGoals ptsGoals ptsMinutes
      L.td_ "" -- spacer
      renderScore smTotal ptsTotal ptsMinutes

    renderScore :: Rational -> Int -> Word -> L.Html ()
    renderScore sm s ms =
      L.td_
        [L.style_ style]
        $ L.toHtml
        $ showFloatPlaces 1
        $ realToFrac
        $ score * 90
      where
        score :: Rational = fromIntegral s % fromIntegral ms
        hue =
          realToFrac score / realToFrac sm
            & (* 120)
            & T.show @Double
        bgColor = "hsl(" <> hue <> " 100% 50% / 0.25)"
        style = "background-color: " <> bgColor

    renderMinutes :: Rational -> Word -> L.Html ()
    renderMinutes sm s =
      L.td_
        [L.style_ style]
        $ L.toHtml
        $ T.show s
      where
        score :: Double = fromIntegral s
        scoreMax = realToFrac sm
        hue =
          score / scoreMax
            & (* 120)
            & T.show @Double
        bgColor = "hsl(" <> hue <> " 100% 50% / 0.25)"
        style = "background-color: " <> bgColor

    calcMaxes :: [PlayerTotals] -> ScoreMaxes Rational
    calcMaxes pts =
      ScoreMaxes
        { smTotal = pts & fmap (\p -> ptsTotal p & perMin p) & maximum,
          smGoals = pts & fmap (\p -> ptsGoals p & perMin p) & maximum,
          smCs = pts & fmap (\p -> ptsCs p & perMin p) & maximum,
          smOther = pts & fmap (\p -> ptsOther p & perMin p) & maximum,
          smMins = pts & fmap (ptsMinutes >>> fromIntegral) & maximum
        }

    perMin :: PlayerTotals -> Int -> Rational
    perMin PlayerTotals {ptsMinutes} s =
      fromIntegral s % fromIntegral ptsMinutes

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
