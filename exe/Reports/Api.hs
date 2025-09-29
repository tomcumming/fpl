module Reports.Api where

import GHC.Generics (Generic)
import Lucid qualified as L
import Servant (Get, Raw, (:-), (:>))
import Servant qualified as Sv
import Servant.HTML.Lucid (HTML)

data Api mode = Api
  { apiRoot :: mode :- Get '[HTML] (L.Html ()),
    apiStatic :: mode :- "static" :> Raw,
    apiDefence :: mode :- "defence" :> Get '[HTML] (L.Html ()),
    apiFixtures :: mode :- "fixtures" :> Sv.NamedRoutes FixturesApi,
    apiPlayersTotals ::
      mode
        :- "players"
          :> "totals"
          :> Get '[HTML] (L.Html ())
  }
  deriving stock (Generic)

data FixturesApi mode = FixturesApi
  { apiFixturesAttacking :: mode :- "attacking" :> Get '[HTML] (L.Html ()),
    apiFixturesDefending :: mode :- "defending" :> Get '[HTML] (L.Html ())
  }
  deriving (Generic)
