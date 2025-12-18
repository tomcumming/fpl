module Reports.Markup (baseTemplate, showFloatPlaces, greenToRed) where

import Data.Function ((&))
import Data.Text qualified as T
import Lucid qualified as L
import Numeric (showFFloat)

baseTemplate :: L.Html a -> L.Html a
baseTemplate body = L.doctypehtml_ $ L.html_ $ do
  L.head_ $ do
    L.meta_ [L.charset_ "utf-8"]
    L.title_ "FPL Reports"
    L.link_ [L.href_ "/static/css/pico.classless.css", L.rel_ "stylesheet"]
  L.body_ $ do
    L.header_ $ L.nav_ $ do
      L.a_ [L.href_ "/"] "Home"
      L.a_ [L.href_ "/defence"] "Team Def"
      L.a_ [L.href_ "/fixtures/attacking"] "Att Fixtures"
      L.a_ [L.href_ "/fixtures/defending"] "Def Fixtures"
      L.a_ [L.href_ "/players/totals"] "Player Totals"
      L.a_ [L.href_ "/players/totals/per-90"] "Player / 90"
      L.a_ [L.href_ "/players/prediction"] "Prediction"
    L.main_ body

showFloatPlaces :: (RealFloat a) => Int -> a -> T.Text
showFloatPlaces places n = showFFloat (Just places) n "" & T.pack

-- | Input should be 0-1
greenToRed :: Float -> T.Text
greenToRed x =
  let hue = x * 120
   in "hsl(" <> T.show hue <> " 100% 50% / 0.25)"
