{-# OPTIONS_GHC -Wno-orphans #-}

module Reports.Api where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Text qualified as T
import FPL.Database.Types (MatchWeek, Position, unsafeMatchWeek)
import GHC.Generics (Generic)
import Lucid qualified as L
import Servant (Get, Raw, (:-), (:>))
import Servant qualified as Sv
import Servant.HTML.Lucid (HTML)
import Text.Read (readEither)

data Api mode = Api
  { apiRoot :: mode :- Get '[HTML] (L.Html ()),
    apiStatic :: mode :- "static" :> Raw,
    apiDefence :: mode :- "defence" :> Get '[HTML] (L.Html ()),
    apiFixtures :: mode :- "fixtures" :> Sv.NamedRoutes FixturesApi,
    apiPlayersTotals ::
      mode
        :- "players"
          :> "totals"
          :> Sv.NamedRoutes PlayerTotalsApi,
    apiPlayerPrediction ::
      mode
        :- "players"
          :> "prediction"
          :> Sv.QueryParam "from" MatchWeek
          :> Sv.QueryParam "upto" MatchWeek
          :> Sv.QueryParam "mw" MatchWeek
          :> Sv.QueryParam "pos" Position
          :> Get '[HTML] (L.Html ())
  }
  deriving stock (Generic)

data FixturesApi mode = FixturesApi
  { apiFixturesAttacking :: mode :- "attacking" :> Get '[HTML] (L.Html ()),
    apiFixturesDefending :: mode :- "defending" :> Get '[HTML] (L.Html ())
  }
  deriving (Generic)

data PlayerTotalsApi mode = PlayerTotalsApi
  { apiPlayerTotalsAbsolute :: mode :- Get '[HTML] (L.Html ()),
    apiPlayerTotalsPer90 :: mode :- "per-90" :> Get '[HTML] (L.Html ())
  }
  deriving (Generic)

instance Sv.FromHttpApiData MatchWeek where
  parseUrlPiece = Sv.parseUrlPiece >=> (unsafeMatchWeek >>> pure)

instance Sv.FromHttpApiData Position where
  parseUrlPiece =
    Sv.parseUrlPiece @String
      >=> (readEither @Position >>> first T.show)
