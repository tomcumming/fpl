module Memo
  ( Memo,
    empty,
    lookup,
    lookupUnsafe,
    insert,
  )
where

import Data.Data (Typeable)
import Data.Functor ((<&>))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import IndexedMap qualified as IM
import Prelude hiding (lookup)

newtype Memo key = Memo (IORef (IM.IMap key))

empty :: IO (Memo key)
empty = Memo <$> newIORef mempty

lookup ::
  (Show (key val), Ord (key val), Typeable key, Typeable val) =>
  key val ->
  IO val ->
  Memo key ->
  IO val
lookup k mv (Memo ior) =
  readIORef ior <&> IM.lookup k >>= \case
    Nothing -> do
      v <- mv
      modifyIORef' ior (IM.insert k v)
      pure v
    Just v -> pure v

lookupUnsafe ::
  (Show (key val), Ord (key val), Typeable key, Typeable val) =>
  key val ->
  Memo key ->
  IO val
lookupUnsafe key =
  lookup
    key
    (fail $ "Expected to be set: " <> show key)

-- | When you have already computer something, don't waste it
insert ::
  (Show (key val), Ord (key val), Typeable key, Typeable val) =>
  key val ->
  val ->
  Memo key ->
  IO ()
insert k v (Memo ior) = modifyIORef' ior (IM.insert k v)
