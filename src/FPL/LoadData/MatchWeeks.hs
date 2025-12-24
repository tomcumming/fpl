module FPL.LoadData.MatchWeeks
  ( fixtureMatchWeek,
    fixtureIndex,
    fixtureHome,
    fixtureAway,
    fixtureHomeScore,
    fixtureAwayScore,
  )
where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Data (Typeable)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Tuple (swap)
import Debug.Trace (traceM)
import DoubleMap qualified as DM
import FPL.Database.Types (Fixture, Key (..), MatchWeek, Team, unsafeFixture, unsafeMatchWeek)
import FPL.LoadData.TeamNames (teamNames)
import Memo qualified

data FixtureRow = FixtureRow
  { fixrHome :: Team,
    fixrAway :: Team,
    fixrScore :: Maybe (Word, Word)
  }
  deriving (Show)

loadFirst :: (Typeable val) => Key val -> Memo.Memo Key -> IO val
loadFirst key memo =
  Memo.lookup
    key
    (insertFixtureData memo >> Memo.lookupUnsafe key memo)
    memo

fixtureMatchWeek :: Memo.Memo Key -> IO (DM.DMap Fixture MatchWeek)
fixtureMatchWeek = loadFirst FixtureMatchWeek

fixtureIndex :: Memo.Memo Key -> IO (M.Map Fixture Word)
fixtureIndex = loadFirst FixtureIndex

fixtureHome :: Memo.Memo Key -> IO (M.Map Fixture Team)
fixtureHome = loadFirst FixtureHome

fixtureAway :: Memo.Memo Key -> IO (M.Map Fixture Team)
fixtureAway = loadFirst FixtureAway

fixtureHomeScore :: Memo.Memo Key -> IO (M.Map Fixture Word)
fixtureHomeScore = loadFirst FixtureHomeScore

fixtureAwayScore :: Memo.Memo Key -> IO (M.Map Fixture Word)
fixtureAwayScore = loadFirst FixtureAwayScore

insertFixtureData :: Memo.Memo Key -> IO ()
insertFixtureData memo = do
  traceM "Loading MatchWeeks"
  tns <-
    teamNames memo
      & fmap (M.assocs >>> fmap swap >>> M.fromList)
  mws <- staggerFixtures <$> loadMatchWeeks tns
  let flat = zip (unsafeFixture <$> [1 ..]) $ do
        (mw, gs) <- M.assocs mws
        (i, fs) <- M.assocs gs
        ((mw, i),) <$> fs

  Memo.insert
    FixtureMatchWeek
    ( DM.fromMap $ M.fromList $ do
        (fixture, ((mw, _), _)) <- flat
        pure (fixture, mw)
    )
    memo
  Memo.insert
    FixtureIndex
    ( M.fromList $ do
        (fixture, ((_mw, i), _)) <- flat
        pure (fixture, i)
    )
    memo
  Memo.insert
    FixtureHome
    ( M.fromList $ do
        (fixture, ((_mw, _i), FixtureRow {fixrHome})) <- flat
        pure (fixture, fixrHome)
    )
    memo
  Memo.insert
    FixtureAway
    ( M.fromList $ do
        (fixture, ((_mw, _i), FixtureRow {fixrAway})) <- flat
        pure (fixture, fixrAway)
    )
    memo
  Memo.insert
    FixtureHomeScore
    ( M.fromList $ do
        (fixture, ((_mw, _i), FixtureRow {fixrScore})) <- flat
        (sh, _) <- maybeToList fixrScore
        pure (fixture, sh)
    )
    memo
  Memo.insert
    FixtureAwayScore
    ( M.fromList $ do
        (fixture, ((_mw, _i), FixtureRow {fixrScore})) <- flat
        (_, sa) <- maybeToList fixrScore
        pure (fixture, sa)
    )
    memo

loadMatchWeeks :: M.Map T.Text Team -> IO (M.Map MatchWeek [FixtureRow])
loadMatchWeeks tns = do
  vals :: [[Aeson.Value]] <-
    Aeson.eitherDecodeFileStrict "data/snapshot/matchweeks.json"
      >>= either fail pure
  Aeson.parseEither (traverse readMatchWeek) vals
    & either fail (fold >>> pure)
  where
    readMatchWeek :: [Aeson.Value] -> Aeson.Parser (M.Map MatchWeek [FixtureRow])
    readMatchWeek = \case
      (Aeson.Object o : rows) -> do
        mw <- unsafeMatchWeek <$> o Aeson..: "matchWeek"
        M.singleton mw <$> traverse parseRow rows
      mw -> fail $ show mw

    parseRow :: Aeson.Value -> Aeson.Parser FixtureRow
    parseRow =
      Aeson.parseJSON @[Aeson.Value] >=> \case
        [homeName, awayName] -> do
          fixrHome <- lookupTeam =<< Aeson.parseJSON homeName
          fixrAway <- lookupTeam =<< Aeson.parseJSON awayName
          pure $ FixtureRow {fixrHome, fixrAway, fixrScore = Nothing}
        [homeName, homeScore, awayScore, awayName] -> do
          fixrHome <- lookupTeam =<< Aeson.parseJSON homeName
          fixrAway <- lookupTeam =<< Aeson.parseJSON awayName
          home <- Aeson.parseJSON homeScore
          away <- Aeson.parseJSON awayScore
          let fixrScore = Just (home, away)
          pure $ FixtureRow {fixrHome, fixrAway, fixrScore}
        cs -> fail $ show cs

    lookupTeam name =
      M.lookup name tns
        & maybe (fail $ show name) pure

staggerFixtures ::
  M.Map MatchWeek [FixtureRow] ->
  M.Map MatchWeek (M.Map Word [FixtureRow])
staggerFixtures = fmap (go >>> fmap snd)
  where
    go :: [FixtureRow] -> M.Map Word (S.Set Team, [FixtureRow])
    go = flip foldr mempty $ \fixr@FixtureRow {..} rest ->
      let fixrTeams = S.fromList [fixrHome, fixrAway]
       in case rest M.!? 1 of
            Just (seen, fixrs)
              | S.disjoint seen fixrTeams ->
                  M.insert 1 (seen <> fixrTeams, fixr : fixrs) rest
              | otherwise ->
                  M.insert 1 (fixrTeams, [fixr]) $ M.mapKeysMonotonic succ rest
            Nothing -> M.singleton 1 (fixrTeams, [fixr])
