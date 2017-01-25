module Lib
    ( justDoIt
    ) where


import Data.List (intercalate)
import Data.Time (ZonedTime, defaultTimeLocale, getZonedTime, formatTime)
import Data.Tree.NTree.TypeDefs (NTree)
import Text.XML.HXT.Core
import Text.HandsomeSoup

-- structures
data DateRepr = DateRepr {
  dow :: Int,
  localTime :: ZonedTime
}

data Menu = Menu {
  who :: String,
  options :: [String]
}

data Endpoint = Endpoint {
  name :: String,
  url :: String,
  parser :: IOSLA (XIOState ()) XmlTree (NTree XNode)
            -> DateRepr
            -> IOSLA (XIOState ()) XmlTree String
}

instance Show Menu where
  show (Menu who options) = "who:\t" ++ who ++ "\n" ++ "menus:\n\t" ++
                            (intercalate "\n\t" options)


-- parsers
parserUnion :: IOSLA (XIOState ()) XmlTree (NTree XNode)
              -> DateRepr
              -> IOSLA (XIOState ()) XmlTree String
parserUnion doc (DateRepr dow localTime) =
  doc
  >>> css (".foodDayMenuBlock-" ++ show dow ++ " .foodItem")
  -- take only the first menu
  >>. take 1
  >>> css (".foodItemDesc")
  /> getText

parserPiramida :: IOSLA (XIOState ()) XmlTree (NTree XNode)
              -> DateRepr
              -> IOSLA (XIOState ()) XmlTree String
parserPiramida doc (DateRepr dow localTime) =
  doc
  >>> css (".jsrm-menu")
  >. (!! dow)
  >>> css (".item-text")
  //> getText

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


justDoIt :: IO ()
justDoIt =
  getDateRepr >>= \dateRepr ->
  mapM (\endpoint -> parseEndpoint endpoint dateRepr) getEndpoints >>= \menus ->
  mapM_ print menus
