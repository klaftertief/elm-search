module Stylesheets exposing (..)

import Css.File exposing (..)
import Style.Search as Search


port files : CssFileStructure
port files =
    toFileStructure [ ( "search.css", compile Search.css ) ]
