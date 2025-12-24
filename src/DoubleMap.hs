module DoubleMap
  ( DMap (..),
    fromMap,
  )
where

import Control.Category ((>>>))
import Data.Bifunctor (second)
import Data.Function ((&))
import Data.Map.Strict qualified as M
import Data.Tuple (swap)

data DMap k v = DMap
  { forwards :: M.Map k v,
    backwards :: M.Map v [k]
  }

fromMap :: (Ord v) => M.Map k v -> DMap k v
fromMap forwards =
  DMap
    { forwards,
      backwards =
        M.assocs forwards
          & fmap (swap >>> second pure)
          & M.fromListWith (<>)
    }
