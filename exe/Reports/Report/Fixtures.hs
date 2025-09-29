module Reports.Report.Fixtures (fixtures) where

import Control.Category ((>>>))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time (Day)
import FPL.LoadData.Fixtures qualified as LD
import Lucid qualified as L
import Reports.Api qualified as Api
import Reports.Markup (baseTemplate, showFloatPlaces)
import Servant qualified as Sv

fixtures :: Sv.Server Api.FixturesApi
fixtures = do
  LD.Loaded {..} <- liftIO LD.loadFixturesData

  let fixtureRows = collectTeamFixtures ldFixtures
  let maxFixtures = M.elems fixtureRows & fmap length & maximum

  let teamScores = fmap snd ldResults & calcScores
  let scoreRows :: M.Map LD.Team [(LD.Team, Double)] =
        fmap (scoreRow teamScores) fixtureRows
  let allScores = M.elems scoreRows & concatMap (fmap snd)
  let minScore = minimum allScores
  let maxScore = maximum allScores

  pure $ baseTemplate $ L.table_ [L.class_ "fixtures-table"] $ do
    L.thead_ $ L.tr_ $ do
      L.th_ "Team"
      L.th_ [L.colspan_ (T.show maxFixtures)] "Fixtures"
    L.tbody_ $
      void $
        M.traverseWithKey
          (renderRow maxFixtures minScore maxScore)
          scoreRows
  where
    scoreFn :: LD.Result -> M.Map LD.Team Word
    scoreFn LD.Result {..} =
      M.fromList
        [ (resHome, snd resScore),
          (resAway, fst resScore)
        ]

    calcScores :: [LD.Result] -> M.Map LD.Team Double
    calcScores =
      fmap scoreFn
        >>> fmap (fmap (realToFrac >>> pure @[]))
        >>> M.unionsWith (<>)
        >>> fmap (\ss -> sum ss / realToFrac (length ss))

    collectTeamFixtures :: [(Day, LD.Fixture)] -> M.Map LD.Team [LD.Team]
    collectTeamFixtures =
      fmap
        ( \(d, LD.Fixture {..}) ->
            M.fromList
              [ (fixHome, M.singleton d fixAway),
                (fixAway, M.singleton d fixHome)
              ]
        )
        >>> M.unionsWith (<>)
        >>> fmap M.elems

    renderRow :: Int -> Double -> Double -> LD.Team -> [(LD.Team, Double)] -> L.Html ()
    renderRow maxFixtures minScore maxScore team ops = L.tr_ $ do
      L.td_ $ L.toHtml $ LD.unTeam team
      traverse_ (renderCell minScore maxScore) (padRow maxFixtures ops)

    padRow :: Int -> [a] -> [Maybe a]
    padRow maxFixtures = fmap Just >>> (<> repeat Nothing) >>> take maxFixtures

    renderCell :: Double -> Double -> Maybe (LD.Team, Double) -> L.Html ()
    renderCell minScore maxScore = \case
      Nothing -> L.td_ ""
      Just (team, score) -> do
        let hue =
              (score - minScore) / (maxScore - minScore)
                & (* 120)
        let bgCol = "hsl(" <> T.show hue <> " 100% 50% / 0.25)"
        let style = "background-color: " <> bgCol
        L.td_ [L.class_ "fixture-score", L.style_ style] $ do
          L.div_ $ L.toHtml $ LD.unTeam team
          L.div_ $ L.toHtml $ showFloatPlaces 1 score

    scoreRow :: M.Map LD.Team Double -> [LD.Team] -> [(LD.Team, Double)]
    scoreRow teamScores = fmap (\t -> (t, teamScores M.! t))
