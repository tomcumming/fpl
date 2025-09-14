module FPL.LoadData.Historical (loadHistoricalResults) where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text qualified as T

loadHistoricalResults ::
  FilePath ->
  IO [(T.Text, T.Text, (Word, Word))]
loadHistoricalResults =
  Aeson.decodeFileStrict >=> \case
    Nothing -> fail "Could not parse historical file"
    Just vs -> traverse (Aeson.parseEither parse >>> either fail pure) vs
  where
    parse :: [Aeson.Value] -> Aeson.Parser (T.Text, T.Text, (Word, Word))
    parse = \case
      [ Aeson.String home,
        Aeson.Number homeScore,
        Aeson.Number awayScore,
        Aeson.String away
        ] ->
          pure
            ( home,
              away,
              (round homeScore, round awayScore)
            )
      vs -> fail $ "Can't parse entry: " <> show vs
