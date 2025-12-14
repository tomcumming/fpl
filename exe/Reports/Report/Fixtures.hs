module Reports.Report.Fixtures (fixtures) where

import Control.Category ((>>>))
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Text qualified as T
import FPL.LoadData.MatchWeeks (MatchWeek, loadMatchWeeks)
import FPL.LoadData.TeamNames (Team, teamShortName)
import FPL.Model (Score (..), predictScores)
import FPL.Stats (teamStats)
import Lucid qualified as L
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
  (results, fixtures_) <- liftIO loadMatchWeeks
  let ts = teamStats results
  let colsData = transpose $ predictScores ts fixtures_
  let allScores =
        M.elems colsData
          & concatMap M.elems
          & concatMap
            (\(_, Score {..}) -> [scoreFor, scoreAgainst])
  let minScore = minimum allScores
  let maxScore = maximum allScores
  pure $ baseTemplate $ do
    L.h2_ $ case focus of
      Attacking -> "Expected goals for"
      Defending -> "Expected goals against"
    L.table_ [L.class_ "fixtures-table"] $ do
      L.thead_ $ L.tr_ $ do
        L.th_ "Team"
        forM_ (M.keys colsData) $ \(mw, _) -> L.th_ $ L.toHtml $ T.show mw
      L.tbody_ $
        traverse_
          (renderRow minScore maxScore colsData)
          (M.keys ts)
  where
    transpose ::
      M.Map Team (M.Map MatchWeek [(Team, Score Float)]) ->
      M.Map (MatchWeek, Int) (M.Map Team (Team, Score Float))
    transpose =
      M.assocs
        >>> concatMap (\(team, ss) -> (team,) <$> M.assocs ss)
        >>> concatMap
          ( \(team, (mw, ss)) ->
              zip [0 ..] ss
                & fmap (\(idx, s) -> M.singleton (mw, idx) (M.singleton team s))
          )
        >>> M.unionsWith (<>)

    renderRow :: Float -> Float -> M.Map (MatchWeek, Int) (M.Map Team (Team, Score Float)) -> Team -> L.Html ()
    renderRow minScore maxScore colsData team = L.tr_ $ do
      L.td_ $ L.toHtml $ teamShortName team
      void $ M.traverseWithKey (renderCell minScore maxScore team) colsData

    renderCell :: Float -> Float -> Team -> (MatchWeek, Int) -> M.Map Team (Team, Score Float) -> L.Html ()
    renderCell minScore maxScore team mwi =
      (M.!? team) >>> \case
        Nothing -> L.td_ [L.data_ "key" (T.show mwi)] "-"
        Just (opp, Score {..}) -> do
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
              L.div_ $ L.toHtml $ teamShortName opp
              L.div_ $ L.toHtml $ showFloatPlaces 1 score
