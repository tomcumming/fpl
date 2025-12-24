module Reports.Report.Fixtures (fixtures) where

import Control.Monad (forM_, guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Maybe (listToMaybe, maybeToList)
import Data.Set qualified as S
import Data.Text qualified as T
import DoubleMap qualified as DM
import FPL.Database.Types qualified as DB
import FPL.LoadData.MatchWeeks (fixtureAway, fixtureHome, fixtureHomeScore, fixtureIndex, fixtureMatchWeek)
import FPL.LoadData.TeamNames (teamNames)
import FPL.Model (predictedAwayScore, predictedHomeScore)
import Lucid qualified as L
import Memo qualified
import Reports.Api qualified as Api
import Reports.Markup (baseTemplate, greenToRed, showFloatPlaces)
import Servant qualified as Sv
import Servant.HTML.Lucid qualified as Sv
import Servant.Server.Generic qualified as Sv

data Focus
  = Attacking
  | Defending
  deriving (Eq, Ord)

fixtures :: Api.FixturesApi Sv.AsServer
fixtures =
  Api.FixturesApi
    { apiFixturesAttacking = fixturesReport Attacking,
      apiFixturesDefending = fixturesReport Defending
    }

fixturesReport :: Focus -> Sv.Server (Sv.Get '[Sv.HTML] (L.Html ()))
fixturesReport focus = do
  db <- liftIO Memo.empty
  cols <- liftIO $ do
    fmw <- fixtureMatchWeek db & fmap DM.forwards
    fhs <- fixtureHomeScore db
    fi <- fixtureIndex db
    pure $ S.fromList $ do
      (fixture, mw) <- M.assocs fmw
      guard $ M.notMember fixture fhs
      idx <- M.lookup fixture fi & maybeToList
      pure (mw, idx)
  allScores <- liftIO $ do
    phs <- predictedHomeScore db
    pas <- predictedAwayScore db
    pure $ M.elems phs <> M.elems pas
  let minScore = minimum allScores
  let maxScore = maximum allScores
  allTeamNames <- teamNames db & fmap M.keysSet & liftIO
  renderedRows <-
    traverse
      (renderRow db cols minScore maxScore)
      (S.toList allTeamNames)
  pure $ baseTemplate $ do
    L.h2_ $ case focus of
      Attacking -> "Expected goals for"
      Defending -> "Expected goals against"
    L.table_ [L.class_ "fixtures-table"] $ do
      L.thead_ $ L.tr_ $ do
        L.th_ "Team"
        forM_ cols $ \(mw, _) -> L.th_ $ L.toHtml $ T.show mw
      L.tbody_ $ fold renderedRows
  where
    renderRow :: (MonadIO m) => DB.Database -> S.Set (DB.MatchWeek, Word) -> Float -> Float -> DB.Team -> m (L.Html ())
    renderRow db cols minScore maxScore team = do
      renderedCells <- traverse (renderCell db minScore maxScore team) (S.toList cols)
      pure $ L.tr_ $ do
        L.td_ $ L.toHtml $ DB.teamShortName team
        fold renderedCells

    renderCell :: (MonadIO m) => DB.Database -> Float -> Float -> DB.Team -> (DB.MatchWeek, Word) -> m (L.Html ())
    renderCell db minScore maxScore team mwi@(mw, idx) = do
      maybeData <- liftIO $ do
        fixtureMatchWeek' <- fixtureMatchWeek db
        fixtureIndex' <- fixtureIndex db
        fixtureHome' <- fixtureHome db
        fixtureAway' <- fixtureAway db
        predictedHomeScore' <- predictedHomeScore db
        predictedAwayScore' <- predictedAwayScore db
        pure $ listToMaybe $ do
          fixture <-
            DM.backwards fixtureMatchWeek'
              & M.lookup mw
              & fold
          M.lookup fixture fixtureIndex'
            & (== Just idx)
            & guard
          home <- M.lookup fixture fixtureHome' & maybeToList
          away <- M.lookup fixture fixtureAway' & maybeToList
          homePrediction <- M.lookup fixture predictedHomeScore' & maybeToList
          awayPrediction <- M.lookup fixture predictedAwayScore' & maybeToList
          case () of
            () | home == team -> [(homePrediction, (away, awayPrediction))]
            () | away == team -> [(awayPrediction, (home, homePrediction))]
            () -> []
      pure $ case maybeData of
        Nothing -> L.td_ [L.data_ "key" (T.show mwi)] "-"
        Just (scoreFor, (opp, scoreAgainst)) -> do
          let score = case focus of
                Attacking -> scoreFor
                Defending -> scoreAgainst
          let rating = (score - minScore) / (maxScore - minScore)
          let rating' = case focus of
                Attacking -> rating
                Defending -> 1 - rating
          let style = "background-color: " <> greenToRed rating'
          L.td_
            [ L.class_ "fixture-score",
              L.style_ style,
              L.data_ "key" (T.show mwi)
            ]
            $ do
              L.div_ $ L.toHtml $ DB.teamShortName opp
              L.div_ $ L.toHtml $ showFloatPlaces 1 score
