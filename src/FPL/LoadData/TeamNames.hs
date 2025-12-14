module FPL.LoadData.TeamNames (Team, teamShortName, loadTeamNames, teamFromShortName) where

import Control.Category ((>>>))
import Control.Monad (guard)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T

newtype Team = Team {teamShortName :: T.Text}
  deriving newtype (Eq, Ord, Show)

loadTeamNames :: IO (M.Map T.Text Team)
loadTeamNames =
  do
    T.readFile "data/team-names.tsv"
    >>= (T.lines >>> traverse parseLine >>> fmap M.fromList)
  where
    parseLine =
      T.splitOn "\t" >>> \case
        [code, name] -> pure (name, Team code)
        cells -> fail $ "Could not parse " <> show cells

teamFromShortName :: M.Map T.Text Team -> T.Text -> Maybe Team
teamFromShortName teams sn = do
  guard $ elem (Team sn) teams
  Just $ Team sn
