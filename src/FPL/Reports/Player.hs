module FPL.Reports.Player (playersOverview) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.List qualified as List
import Data.Map qualified as M
import Data.Text qualified as T
import FPL.LoadData.Fixtures (Team (..))
import FPL.LoadData.Players (PlayerStats (..), PlayersData (..))
import FPL.Reports (tshow)

playersOverview :: PlayersData -> [[T.Text]]
playersOverview = \PlayersData {..} ->
  ["Name", "Team", "Pos", "Minutes", "Pts", "Pts/g", "DefCon/g", "GI", "GI/g", "Cost"]
    : ( M.elems pdPlayers
          & filter (psMinutes >>> (> 0))
          & List.sortOn psPoints
          & fmap goRow
      )
  where
    goRow :: PlayerStats -> [T.Text]
    goRow PlayerStats {..} =
      [ psName,
        unTeam psTeam,
        tshow psPosition,
        tshow psMinutes,
        tshow psPoints,
        tshow @Double $ 90 * realToFrac psPoints / realToFrac psMinutes,
        tshow @Double $ 90 * realToFrac psDefCon / realToFrac psMinutes,
        tshow @Double goalInv,
        tshow @Double $ 90 * goalInv / realToFrac psMinutes,
        tshow @Double $ realToFrac psCost / 10
      ]
      where
        goalInv = realToFrac $ psGoals + psAssists
