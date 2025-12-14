module Reports.Report.Defence (defence) where

import Lucid qualified as L
import Servant qualified as Sv
import Servant.HTML.Lucid qualified as L

defence :: Sv.Server (Sv.Get '[L.HTML] (L.Html ()))
defence = do
  fail "Todo!"

{-
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

    renderResult :: Int -> Maybe Double -> L.Html ()
    renderResult places = \case
      Nothing -> L.td_ "-"
      Just n -> do
        let hsl = "hsl(" <> goalsHue (realToFrac n) <> " 100% 50% / 0.25)"
        let style = "background-color: " <> hsl
        L.td_ [L.style_ style] $ L.toHtml $ showFloatPlaces places n

    renderRow :: Int -> T.Text -> [Word] -> L.Html ()
    renderRow maxResults teamName gs = L.tr_ $ do
      L.th_ $ L.toHtml teamName
      padResults maxResults gs
        & traverse_ (fmap realToFrac >>> renderResult 0)
      L.td_ "" -- spacer
      let avg :: Double = fromIntegral (sum gs) / fromIntegral (length gs)
      renderResult 2 $ Just avg

    goalsHue :: Double -> T.Text
    goalsHue =
      min maxGoals
        >>> negate
        >>> (2 **)
        >>> (* 120)
        >>> T.show

    maxGoals = 3

-}
