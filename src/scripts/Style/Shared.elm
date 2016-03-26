module Style.Shared (..) where

import Html.CssHelpers exposing (namespace)


type CssClasses
  = SearchInput


type CssIds
  = ElmLogo


cssNamespace =
  namespace "search"
