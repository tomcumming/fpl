module FPL.Rules
  ( pointsForAssist,
    pointsForGoal,
    pointsForCS,
  )
where

import FPL.LoadData.Players (Position (..))

pointsForAssist :: Word
pointsForAssist = 3

pointsForGoal :: Position -> Word
pointsForGoal = \case
  GK -> 10
  Def -> 6
  Mid -> 5
  Att -> 4

pointsForCS :: Position -> Word
pointsForCS = \case
  GK -> 4
  Def -> 4
  Mid -> 1
  Att -> 0
