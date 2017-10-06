module Generate exposing (elm)

import Docs.Package exposing (Package)


elm : List Package -> String
elm packages =
    String.join "\n"
        [ "module Main exposing (..)"
        , ""
        , "import Web"
        , "import Docs.Type"
        , ""
        , "main ="
        , "  Web.program <|"
        , "    " ++ generate packages
        , ""
        ]


generate : List Package -> String
generate =
    chunk toString []
        >> List.map toString
        >> String.join " ++ "


chunk : (a -> b) -> List (List b) -> List a -> List (List b)
chunk fn acc original =
    case original of
        a :: b :: c :: d :: e :: f :: rest ->
            chunk fn ([ fn a, fn b, fn c, fn d, fn e, fn f ] :: acc) rest

        _ ->
            List.map fn original :: acc
