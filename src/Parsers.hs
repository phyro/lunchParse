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

type IBelieveInMagic = IOSLA (XIOState ()) XmlTree (NTree XNode)
          -> DateRepr
          -> IOSLA (XIOState ()) XmlTree String

data Endpoint = Endpoint {
  name :: String,
  url :: String,
  parser :: IBelieveInMagic
}

-- parsers
parserUnion :: IBelieveInMagic
parserUnion doc (DateRepr dow localTime) =
  doc
  >>> css (".foodDayMenuBlock-" ++ show dow ++ " .foodItem")
  -- take only the first menu
  >>. take 1
  >>> css (".foodItemDesc")
  /> getText

parserPiramida :: IBelieveInMagic
parserPiramida doc (DateRepr dow localTime) =
  doc
  >>> css (".jsrm-menu")
  >. (!! dow)
  >>> css (".item-text")
  //> getText
