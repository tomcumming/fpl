module FPL.Stats
  ( playerCleanSheetPoints,
    playerOtherPoints,
    playerGoalPoints,
    playerApps,
  )
where

import Data.Data (Typeable)
import Data.Function ((&))
import Data.Map.Strict qualified as M
import Data.Maybe (maybeToList)
import Debug.Trace (traceM)
import FPL.Database.Types (Key (..), Player)
import FPL.LoadData.Players (playerAssists, playerCleanSheets, playerGoals, playerMinutes, playerPointsPerGame, playerPosition, playerTotalPoints)
import FPL.Rules (pointsForAssist, pointsForCS, pointsForGoal)
import Memo qualified

loadFirst :: (Typeable val) => Key val -> Memo.Memo Key -> IO val
loadFirst k memo = do
  playerCleanSheets' <- playerCleanSheets memo
  playerPosition' <- playerPosition memo
  playerGoals' <- playerGoals memo
  playerAssists' <- playerAssists memo
  playerTotalPoints' <- playerTotalPoints memo

  let pcsp = M.fromList $ do
        (player, cs) <- M.assocs playerCleanSheets'
        position <- M.lookup player playerPosition' & maybeToList
        pure (player, cs * pointsForCS position & fromIntegral)
  let pgp = M.fromList $ do
        (player, goals) <- M.assocs playerGoals'
        assists <- M.lookup player playerAssists' & maybeToList
        position <- M.lookup player playerPosition' & maybeToList
        pure
          ( player,
            pointsForAssist * assists
              + pointsForGoal position * goals
              & fromIntegral
          )
  let pop = M.fromList $ do
        (player, total) <- M.assocs playerTotalPoints'
        csPs <- M.lookup player pcsp & maybeToList
        goalPs <- M.lookup player pgp & maybeToList
        pure (player, total - csPs - goalPs)

  Memo.insert PlayerCleanSheetPoints pcsp memo
  Memo.insert PlayerOtherPoints pop memo
  Memo.insert PlayerGoalPoints pgp memo

  Memo.lookupUnsafe k memo

playerCleanSheetPoints :: Memo.Memo Key -> IO (M.Map Player Int)
playerCleanSheetPoints = loadFirst PlayerCleanSheetPoints

playerOtherPoints :: Memo.Memo Key -> IO (M.Map Player Int)
playerOtherPoints = loadFirst PlayerOtherPoints

playerGoalPoints :: Memo.Memo Key -> IO (M.Map Player Int)
playerGoalPoints = loadFirst PlayerGoalPoints

playerApps :: Memo.Memo Key -> IO (M.Map Player Word)
playerApps memo = Memo.lookup PlayerApps go memo
  where
    go = do
      traceM "Calculating PlayerApps"
      pm <- playerMinutes memo
      ptp <- playerTotalPoints memo
      pppg <- playerPointsPerGame memo
      pure $ M.fromList $ do
        (player, mins) <- M.assocs pm
        totalPoints <- M.lookup player ptp & maybeToList
        ptsPerGame <- M.lookup player pppg & maybeToList
        pure
          ( player,
            maximum
              [ 0,
                round $ realToFrac totalPoints / ptsPerGame,
                round @Float $ realToFrac mins / 90
              ]
          )
