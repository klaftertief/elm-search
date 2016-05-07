module Component.PackageDocs exposing (..)

-- where

import Dict
import Regex
import Set
import String
import Docs.Name as Name
import Docs.Package as Docs
import Docs.Type as Type
import Parse.Type as Type


toNameDict : Docs.Package -> Name.Dictionary
toNameDict pkg =
  Dict.map (\_ modul -> Set.fromList (Dict.keys modul.entries)) pkg


stringToType : String -> Type.Type
stringToType str =
  case Type.parse str of
    Ok tipe ->
      tipe

    Err _ ->
      Type.Var str


var : Regex.Regex
var =
  Regex.regex "^[a-zA-Z0-9_']+$"


operator : Regex.Regex
operator =
  Regex.regex "^\\([^a-zA-Z0-9]+\\)$"


isValue : String -> Maybe String
isValue str =
  if Regex.contains var str then
    Just str
  else if Regex.contains operator str then
    Just (String.dropLeft 1 (String.dropRight 1 str))
  else
    Nothing
