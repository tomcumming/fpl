module Reports.Server
  ( server,
  )
where

import Lucid qualified as L
import Reports.Api qualified as Api
import Reports.Markup (baseTemplate)
import Reports.Report.Defence qualified as Defence
import Reports.Report.Fixtures qualified as Fixtures
import Reports.Report.PlayerPrediction qualified as PlayerPrediction
import Reports.Report.PlayerTotals qualified as PlayerTotals
import Servant qualified as Sv
import Servant.Server.Generic qualified as Sv

server :: Api.Api Sv.AsServer
server =
  Api.Api
    { apiRoot = pure $ baseTemplate $ do
        L.header_ $ L.p_ "Hello Header"
        L.main_ $ L.h1_ "FPL Reports",
      apiStatic = Sv.serveDirectoryFileServer "www",
      apiDefence = Defence.defence,
      apiFixtures = Fixtures.fixtures,
      apiPlayersTotals = PlayerTotals.playerTotals,
      apiPlayerPrediction = PlayerPrediction.playerPrediction
    }
