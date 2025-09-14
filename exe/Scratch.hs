module Main (main) where

import Control.Category ((>>>))
import Control.Monad (forM_)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import FPL.LoadData (Loaded (..), Result (..), loadData, unTeam)

main :: IO ()
main = do
  Loaded {..} <- loadData

  T.putStrLn "Teams:"
  forM_ (M.toList ldNames) $ \(name, code) ->
    [unTeam code, name]
      & T.intercalate "\t"
      & T.putStrLn

  T.putStrLn "Results:"
  forM_ ldResults $ \(day, Result {..}) ->
    [tshow day, unTeam resHome, tshow resScore, unTeam resAway]
      & T.intercalate "\t"
      & T.putStrLn

tshow :: (Show a) => a -> T.Text
tshow = show >>> T.pack
