module Reports.Api where

import GHC.Generics (Generic)
import Lucid qualified as L
import Servant (Get, Raw, (:-), (:>))
import Servant.HTML.Lucid (HTML)

data Api mode = Api
  { apiRoot :: mode :- Get '[HTML] (L.Html ()),
    apiStatic :: mode :- "static" :> Raw,
    apiDefence :: mode :- "defence" :> Get '[HTML] (L.Html ())
  }
  deriving stock (Generic)
