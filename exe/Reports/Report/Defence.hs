module Reports.Report.Defence (defence) where

import Control.Category ((>>>))
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Set qualified as S
import Data.Text qualified as T
import DoubleMap qualified as DM
import FPL.Database.Types (MatchWeek, Team, teamShortName)
import FPL.LoadData.MatchWeeks (fixtureAway, fixtureAwayScore, fixtureHome, fixtureHomeScore, fixtureIndex, fixtureMatchWeek)
import Lucid qualified as L
import Memo qualified
import Reports.Markup (baseTemplate, greenToRed, showFloatPlaces)
import Servant qualified as Sv
import Servant.HTML.Lucid qualified as L

defence :: Sv.Server (Sv.Get '[L.HTML] (L.Html ()))
defence = do
  db <- liftIO Memo.empty
  fixtureHomeScore' <- fixtureHomeScore db & liftIO
  fixtureAwayScore' <- fixtureAwayScore db & liftIO
  fixtureHome' <- fixtureHome db & liftIO
  fixtureAway' <- fixtureAway db & liftIO
  fixtureIndex' <- fixtureIndex db & liftIO
  fixtureMatchWeek' <- fixtureMatchWeek db & liftIO

  let teamConceded = M.fromListWith (<>) $ do
        (fixture, homeScore) <- M.assocs fixtureHomeScore'
        idx <- M.lookup fixture fixtureIndex' & maybeToList
        awayScore <- M.lookup fixture fixtureAwayScore' & maybeToList
        home <- M.lookup fixture fixtureHome' & maybeToList
        away <- M.lookup fixture fixtureAway' & maybeToList
        mw <- fixtureMatchWeek' & DM.forwards & M.lookup fixture & maybeToList
        [ (home, M.singleton (mw, idx) awayScore),
          (away, M.singleton (mw, idx) homeScore)
          ]

  let mws = foldMap M.keysSet teamConceded

  pure $ baseTemplate $ L.table_ $ do
    L.thead_ $ L.tr_ $ do
      L.th_ "Team"
      forM_ mws (uncurry renderColTitle)
      L.th_ "" -- spacer
      L.th_ "Average"
    L.tbody_ $ void $ M.traverseWithKey (renderRow mws) teamConceded
  where
    renderColTitle :: MatchWeek -> Word -> L.Html ()
    renderColTitle mw =
      L.th_ . \case
        1 -> T.show mw & L.toHtml
        i -> (T.show mw <> " (" <> T.show i <> ")") & L.toHtml

    renderResult :: Int -> Maybe Float -> L.Html ()
    renderResult places = \case
      Nothing -> L.td_ "-"
      Just n -> do
        let style = "background-color: " <> goalsColour n
        L.td_ [L.style_ style] $ L.toHtml $ showFloatPlaces places n

    renderRow :: S.Set (MatchWeek, Word) -> Team -> M.Map (MatchWeek, Word) Word -> L.Html ()
    renderRow allFs teamName rs = L.tr_ $ do
      L.th_ $ L.toHtml (teamShortName teamName)
      forM_
        allFs
        ((rs M.!?) >>> fmap realToFrac >>> renderResult 0)
      L.td_ "" -- spacer
      let avg = fromIntegral (sum rs) / fromIntegral (M.size rs)
      renderResult 2 $ Just avg

    maxGoals = 3

    goalsColour :: Float -> T.Text
    goalsColour =
      min maxGoals
        >>> (/ maxGoals)
        >>> (`subtract` 1)
        >>> greenToRed
