module Parsers (
      parserUnion
    , parserPiramida
    , DateRepr (DateRepr)
    , Endpoint (Endpoint)
    ) where


import Data.Time (ZonedTime, defaultTimeLocale, getZonedTime, formatTime)
import Data.Tree.NTree.TypeDefs (NTree)
import Text.XML.HXT.Core
import Text.HandsomeSoup

data DateRepr = DateRepr {
  dow :: Int,
  localTime :: ZonedTime
}

data Endpoint = Endpoint {
  name :: String,
  url :: String,
  parser :: IOSLA (XIOState ()) XmlTree (NTree XNode)
            -> DateRepr
            -> IOSLA (XIOState ()) XmlTree String
}

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
