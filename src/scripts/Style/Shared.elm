module Style.Shared exposing (..)

import Html.CssHelpers exposing (namespace)


type CssClasses
    = SearchInput


type CssIds
    = ElmLogo


cssNamespace =
    namespace "search"
