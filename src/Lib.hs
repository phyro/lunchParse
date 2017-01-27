module Lib
    (
      getMenus
    , Menu (Menu)
    ) where


import Data.Time (defaultTimeLocale, getZonedTime, formatTime)
import Parsers (
    Endpoint (Endpoint)
    , parserUnion
    , parserPiramida
    , DateRepr (DateRepr))
import Text.XML.HXT.Core (runX)
import Text.HandsomeSoup (fromUrl)


data Menu = Menu {
  who :: String,
  options :: [String]
}

getDateRepr :: IO DateRepr
getDateRepr =
  getZonedTime >>= \localT ->
  let
    dayOfWeek = read (formatTime defaultTimeLocale "%w" localT) - 1 :: Int
  in
    return $ DateRepr dayOfWeek localT


parseEndpoint :: Endpoint -> DateRepr -> IO Menu
parseEndpoint (Endpoint name url parser) dateRepr =
  let
    doc = fromUrl url
  in
    (runX $ (parser doc dateRepr)) >>= \todayMenu ->
    return $ Menu name todayMenu


getEndpoints :: [Endpoint]
getEndpoints = [
    Endpoint "Union" "http://www.pivnica-union.si/si/" parserUnion,
    Endpoint "Piramida" "http://pizzerijapiramida.si/malice/" parserPiramida
  ]


getMenus :: IO [Menu]
getMenus =
  getDateRepr >>= \dateRepr ->
  mapM (\endpoint -> parseEndpoint endpoint dateRepr) getEndpoints
