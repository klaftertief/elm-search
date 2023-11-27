module Generate exposing (lowerName, main_, package)

import Docs.Package as Package


main_ : List String -> String
main_ moduleNames =
    String.join "\n" <|
        "module Main exposing (..)"
            :: List.map (\name -> "import " ++ name) moduleNames
            ++ [ "import Web"
               , "main = Web.program "
                    ++ list (\name -> name ++ ".package") moduleNames
               ]


package : String -> Package.Package -> String
package moduleName { metadata, modules } =
    let
        { defs, inline } =
            fromPackage metadata modules
    in
    String.join "\n" <|
        [ "module " ++ moduleName ++ " exposing(package)"
        , "import Docs.Type exposing(..)"
        , "package = " ++ inline
        ]
            ++ List.map assignment defs


fromPackage :
    Package.Metadata
    -> List Package.Module
    -> { defs : List ( String, String ), inline : String }
fromPackage metadata modules =
    let
        moduleDefs =
            List.map fromModule modules
    in
    { defs =
        List.map .def moduleDefs
    , inline =
        record
            [ ( "metadata", Package.metadataToString metadata )
            , ( "modules", list .inline moduleDefs )
            ]
    }


fromModule : Package.Module -> { def : ( String, String ), inline : String }
fromModule { name, elmVersion, entries } =
    let
        entryListDefName =
            "v_" ++ lowerName name

        entriesWithFirstDocParagraph =
            List.map
                (\entry ->
                    { entry
                        | docs =
                            entry.docs
                                |> String.split "\n\n"
                                |> List.head
                                |> Maybe.withDefault ""
                    }
                )
                entries
    in
    { def =
        ( entryListDefName, list Debug.toString entriesWithFirstDocParagraph )
    , inline =
        record
            [ ( "name", "\"" ++ name ++ "\"" )
            , ( "elmVersion", Debug.toString elmVersion )
            , ( "entries", entryListDefName )
            ]
    }


list : (a -> String) -> List a -> String
list f values =
    "[" ++ String.join "\n    , " (List.map f values) ++ "\n    ]"


record : List ( String, String ) -> String
record fields =
    "{ "
        ++ String.join "\n    , " (List.map assignment fields)
        ++ "\n    }"


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
