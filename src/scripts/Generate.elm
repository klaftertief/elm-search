module Generate exposing (elm)

import Docs.Package exposing (Package)


elm : List Package -> String
elm packages =
    String.join "\n"
        [ "module Main exposing (main)"
        , ""
        , "import Web"
        , "import Docs.Type exposing(..)"
        , ""
        , "main ="
        , "  Web.program"
        , "    [" ++ generate packages
        , "    ]"
        ]


generate : List Package -> String
generate packages =
    String.join ", " (List.map generateOne packages)


generateOne : Package -> String
generateOne package =
    Debug.crash "TODO"
