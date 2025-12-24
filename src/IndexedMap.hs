{-# LANGUAGE MonoLocalBinds #-}

module IndexedMap
  ( SomeKey (..),
    IMap,
    intoMap,
    fromMap,
    insertWith,
    insert,
    lookup,
  )
where

import Control.Category ((>>>))
import Data.Dynamic (Dynamic, fromDyn, toDyn)
import Data.Function ((&))
import Data.Kind (Type)
import Data.Map.Strict qualified as M
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Data.Typeable (Typeable, typeOf)
import Type.Reflection (typeRep)
import Prelude hiding (lookup)

data SomeKey (k :: Type -> Type)
  = forall t.
  (Show (k t), Ord (k t), Typeable (k t)) =>
  SomeKey {someKey :: k t}

instance Show (SomeKey k) where
  show (SomeKey k) = show k

instance Eq (SomeKey k) where
  SomeKey (k1 :: k t1) == SomeKey (k2 :: k t2) =
    case testEquality (typeRep @(k t1)) (typeRep @(k t2)) of
      Nothing -> False
      Just Refl -> k1 == k2

instance Ord (SomeKey k) where
  SomeKey (k1 :: k t1) `compare` SomeKey (k2 :: k t2) =
    case testEquality (typeRep @(k t1)) (typeRep @(k t2)) of
      Nothing -> typeOf k1 `compare` typeOf k2
      Just Refl -> compare k1 k2

newtype IMap k = IMap
  {intoMap :: M.Map (SomeKey k) Dynamic}
  deriving newtype (Semigroup, Monoid, Show)

fromMap :: M.Map (SomeKey k) Dynamic -> IMap k
fromMap = IMap

fromDynOrError :: (Typeable b) => Dynamic -> b
fromDynOrError x =
  fromDyn x $
    error "Internal error (Dynamic.fromDyn)"

insertWith ::
  forall k v.
  (Typeable v, Show (k v), Ord (k v), Typeable k) =>
  (v -> v -> v) ->
  k v ->
  v ->
  IMap k ->
  IMap k
insertWith f k v =
  intoMap
    >>> M.insertWith go (SomeKey k) (toDyn v)
    >>> fromMap
  where
    go :: Dynamic -> Dynamic -> Dynamic
    go d1 d2 = f (fromDynOrError d1) (fromDynOrError d2) & toDyn

insert ::
  forall k v.
  (Typeable v, Show (k v), Ord (k v), Typeable k) =>
  k v ->
  v ->
  IMap k ->
  IMap k
insert = insertWith const

lookup ::
  (Show (k v), Ord (k v), Typeable k, Typeable v) =>
  k v ->
  IMap k ->
  Maybe v
lookup k = intoMap >>> M.lookup (SomeKey k) >>> fmap fromDynOrError
