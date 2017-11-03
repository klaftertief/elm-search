module Generate exposing (lowerName, main_, package)

import Docs.Package exposing (Complete, Entry, Module)


main_ : List String -> String
main_ moduleNames =
    String.join "\n" <|
        "module Main exposing (..)"
            :: List.map (\name -> "import " ++ name) moduleNames
            ++ [ "import Web"
               , "main = Web.program "
                    ++ chunkedList (\name -> name ++ ".package") moduleNames
               ]


package : String -> Complete -> String
package moduleName complete =
    let
        { defs, inline } =
            fromPackage complete
    in
    String.join "\n" <|
        [ "module " ++ moduleName ++ " exposing(package)"
        , "import Docs.Type exposing(..)"
        , "package = " ++ inline
        ]
            ++ List.map assignment defs


fromPackage : Complete -> { defs : List ( String, String ), inline : String }
fromPackage { user, name, version, modules } =
    let
        moduleDefs =
            List.map fromModule modules
    in
    { defs =
        List.map .def moduleDefs
    , inline =
        record
            [ ( "user", toString user )
            , ( "name", toString name )
            , ( "version", toString version )
            , ( "modules", list .inline moduleDefs )
            ]
    }


fromModule : Module -> { def : ( String, String ), inline : String }
fromModule { name, elmVersion, entries } =
    let
        entryListDefName =
            "v_" ++ lowerName name
    in
    { def =
        ( entryListDefName, chunkedList toString entries )
    , inline =
        record
            [ ( "name", toString name )
            , ( "elmVersion", toString elmVersion )
            , ( "entries", entryListDefName )
            ]
    }


list : (a -> String) -> List a -> String
list f values =
    "[" ++ String.join ", " (List.map f values) ++ "]"


record : List ( String, String ) -> String
record fields =
    "{ "
        ++ String.join ", " (List.map assignment fields)
        ++ " }"


assignment : ( String, String ) -> String
assignment ( left, right ) =
    left ++ " = " ++ right


lowerName : String -> String
lowerName =
    String.map replaceUnsafe >> String.toLower


replaceUnsafe : Char -> Char
replaceUnsafe char =
    if char == '-' || char == '.' then
        '_'
    else
        char


{-| This should be unnecessary with 0.19
<https://github.com/elm-lang/elm-compiler/issues/1521>
-}
chunkedList : (a -> String) -> List a -> String
chunkedList func values =
    if List.isEmpty values then
        "[]"
    else
        chunkedListHelp func values []


chunkSize : Int
chunkSize =
    25


chunkedListHelp : (a -> String) -> List a -> List String -> String
chunkedListHelp func values acc =
    case List.take chunkSize values of
        [] ->
            "(" ++ String.join " ++ " acc ++ ")"

        chunk ->
            chunkedListHelp func
                (List.drop chunkSize values)
                (list func chunk :: acc)
