module FPL.LoadData.TeamNames (teamNames) where

import Control.Category ((>>>))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T
import Debug.Trace (traceM)
import FPL.Database.Types (Key (TeamNames), Team, unsafeTeam)
import Memo qualified

teamNames :: Memo.Memo Key -> IO (M.Map Team T.Text)
teamNames =
  Memo.lookup
    TeamNames
    loadTeamNames

loadTeamNames :: IO (M.Map Team T.Text)
loadTeamNames =
  do
    traceM "loadTeamNames"
    T.readFile "data/team-names.tsv"
    >>= (T.lines >>> traverse parseLine >>> fmap M.fromList)
  where
    parseLine =
      T.splitOn "\t" >>> \case
        [code, name] -> pure (unsafeTeam code, name)
        cells -> fail $ "Could not parse " <> show cells
