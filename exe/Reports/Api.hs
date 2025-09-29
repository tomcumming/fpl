module Reports.Api where

import GHC.Generics (Generic)
import Lucid qualified as L
import Servant (Get, Raw, (:-), (:>))
import Servant.HTML.Lucid (HTML)

type FixturesApi = "fixtures" :> Get '[HTML] (L.Html ())

data Api mode = Api
  { apiRoot :: mode :- Get '[HTML] (L.Html ()),
    apiStatic :: mode :- "static" :> Raw,
    apiDefence :: mode :- "defence" :> Get '[HTML] (L.Html ()),
    apiFixtures :: mode :- FixturesApi,
    apiPlayersTotals ::
      mode
        :- "players"
          :> "totals"
          :> Get '[HTML] (L.Html ())
  }
  deriving stock (Generic)
