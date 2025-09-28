module Reports.Server
  ( server,
  )
where

import Control.Category ((>>>))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List qualified as List
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Tuple qualified as Tuple
import FPL.LoadData.Fixtures qualified as LD
import FPL.LoadData.Players qualified as LD
import FPL.Reports (pointsForAssist, pointsForCS, pointsForGoal)
import Lucid qualified as L
import Reports.Api qualified as Api
import Servant qualified as Sv
import Servant.HTML.Lucid qualified as L
import Servant.Server.Generic qualified as Sv

server :: Api.Api Sv.AsServer
server =
  Api.Api
    { apiRoot = pure $ baseTemplate $ do
        L.header_ $ L.p_ "Hello Header"
        L.main_ $ L.h1_ "FPL Reports",
      apiStatic = Sv.serveDirectoryFileServer "www",
      apiDefence = defence,
      apiPlayersTotals = playerTotals
    }

baseTemplate :: L.Html a -> L.Html a
baseTemplate body = L.doctypehtml_ $ L.html_ $ do
  L.head_ $ do
    L.meta_ [L.charset_ "utf-8"]
    L.title_ "FPL Reports"
    L.link_ [L.href_ "/static/css/pico.classless.css", L.rel_ "stylesheet"]
  L.body_ $ do
    L.header_ $ L.nav_ $ do
      L.a_ [L.href_ "/"] "Home"
      L.a_ [L.href_ "/defence"] "Team Defence"
      L.a_ [L.href_ "/players/totals"] "Player Totals"
    L.main_ body

defence :: Sv.Server (Sv.Get '[L.HTML] (L.Html ()))
defence = do
  LD.Loaded {..} <- liftIO LD.loadFixturesData
  let teams = ldNames & M.assocs & fmap Tuple.swap & M.fromList
  let resultRows =
        ldResults
          & fmap
            ( \(d, LD.Result {..}) ->
                let (sh, sa) = resScore
                 in M.fromList
                      [(resHome, M.singleton d sa), (resAway, M.singleton d sh)]
            )
          & M.unionsWith (<>)
          & fmap M.elems
          & M.mapKeysMonotonic (teams M.!)
  let maxResults = fmap length resultRows & maximum
  pure $ baseTemplate $ L.table_ $ do
    L.thead_ $ L.tr_ $ do
      L.th_ "Team"
      L.th_ [L.colspan_ (T.show maxResults)] "Results"
      L.th_ "" -- spacer
      L.th_ "Average"
    L.tbody_ $ void $ M.traverseWithKey (renderRow maxResults) resultRows
  where
    padResults :: Int -> [Word] -> [Maybe Word]
    padResults l = fmap Just >>> (<> repeat Nothing) >>> take l

    renderResult :: (Real a, Show a) => Maybe a -> L.Html ()
    renderResult = \case
      Nothing -> L.td_ "-"
      Just n -> do
        let hsl = "hsl(" <> goalsHue (realToFrac n) <> " 100% 50% / 0.25)"
        let style = "background-color: " <> hsl
        L.td_ [L.style_ style] $ L.toHtml $ T.show n

    renderRow :: Int -> T.Text -> [Word] -> L.Html ()
    renderRow maxResults teamName gs = L.tr_ $ do
      L.th_ $ L.toHtml teamName
      padResults maxResults gs & traverse_ renderResult
      L.td_ "" -- spacer
      let avg :: Double = fromIntegral (sum gs) / fromIntegral (length gs)
      renderResult $ Just avg

    goalsHue :: Double -> T.Text
    goalsHue =
      min maxGoals
        >>> negate
        >>> (2 **)
        >>> (* 120)
        >>> T.show

    maxGoals = 3

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
