module Reports.Markup (baseTemplate) where

import Lucid qualified as L

baseTemplate :: L.Html a -> L.Html a
baseTemplate body = L.doctypehtml_ $ L.html_ $ do
  L.head_ $ do
    L.meta_ [L.charset_ "utf-8"]
    L.title_ "FPL Reports"
    L.link_ [L.href_ "/static/css/pico.classless.css", L.rel_ "stylesheet"]
  L.body_ $ do
    L.header_ $ L.nav_ $ do
      L.a_ [L.href_ "/"] "Home"
      L.a_ [L.href_ "/defence"] "Team Defence"
      L.a_ [L.href_ "/players/totals"] "Player Totals"
    L.main_ body
