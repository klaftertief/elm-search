module Generate exposing (lowerName, main_, package)

import Docs.Package as Package
import Docs.Type
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression
import Elm.Syntax.File as File
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range as Range
import Elm.Writer as Writer


node =
    Node.Node Range.emptyRange


main_ : List String -> String
main_ moduleNames =
    { moduleDefinition =
        node
            (Module.NormalModule
                { moduleName = node [ "Main" ]
                , exposingList = node (Exposing.All Range.emptyRange)
                }
            )
    , imports =
        node
            { moduleName = node [ "Web" ]
            , moduleAlias = Nothing
            , exposingList = Nothing
            }
            :: List.map
                (\moduleName ->
                    node
                        { moduleName = node [ moduleName ]
                        , moduleAlias = Nothing
                        , exposingList = Nothing
                        }
                )
                moduleNames
    , declarations =
        [ node
            (Declaration.FunctionDeclaration
                { documentation = Nothing
                , signature = Nothing
                , declaration =
                    node
                        { name = node "main"
                        , arguments = []
                        , expression =
                            node
                                (Expression.Application
                                    [ node (Expression.FunctionOrValue [ "Web" ] "program")
                                    , node
                                        (Expression.ListExpr
                                            (List.map
                                                (\moduleName ->
                                                    node (Expression.FunctionOrValue [ moduleName ] "package")
                                                )
                                                moduleNames
                                            )
                                        )
                                    ]
                                )
                        }
                }
            )
        ]
    , comments = []
    }
        |> Writer.writeFile
        |> Writer.write



--"module Main exposing (..)"
--    :: List.map (\name -> "import " ++ name) moduleNames
--    ++ [ "import Web"
--       , "main = Web.program "
--            ++ list (\name -> name ++ ".package") moduleNames
--       ]


package : String -> Package.Package -> String
package moduleName { metadata, modules } =
    let
        { defs, inline } =
            fromPackage metadata modules
    in
    String.join "\n\n" <|
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
    in
    { def =
        ( entryListDefName, list entryToString entries )
    , inline =
        record
            [ ( "name", "\"" ++ name ++ "\"" )
            , ( "elmVersion"
              , case elmVersion of
                    Just version ->
                        "Just " ++ string version

                    Nothing ->
                        "Nothing"
              )
            , ( "entries", entryListDefName )
            ]
    }


entryToString entry =
    record
        [ ( "name", string entry.name )
        , ( "docs", multilineString entry.docs )
        , ( "tipe", typeToString entry.tipe )
        ]


typeToString : Docs.Type.Type -> String
typeToString t =
    case t of
        Docs.Type.Function types type_ ->
            "Function "
                ++ list typeToString types
                ++ " ("
                ++ typeToString type_
                ++ ")"

        Docs.Type.Var var ->
            "Var " ++ string var

        Docs.Type.Apply { home, name } types ->
            "Apply "
                ++ record [ ( "home", string home ), ( "name", string name ) ]
                ++ " "
                ++ list typeToString types

        Docs.Type.Tuple types ->
            "Tuple " ++ list typeToString types

        Docs.Type.Record fields maybeExt ->
            "Record "
                ++ list (\( name, type_ ) -> "(" ++ string name ++ ", " ++ typeToString type_ ++ ")") fields
                ++ " "
                ++ (case maybeExt of
                        Just ext ->
                            "(Just " ++ string ext ++ ")"

                        Nothing ->
                            "Nothing"
                   )


string s =
    "\"" ++ s ++ "\""


multilineString s =
    "\"\"\""
        ++ (s
                |> String.replace "\\" "\\\\"
                |> String.replace "\"" "\\\""
           )
        ++ "\"\"\""


list : (a -> String) -> List a -> String
list f values =
    "[" ++ String.join "\n    , " (List.map f values) ++ "]"


record : List ( String, String ) -> String
record fields =
    "{ "
        ++ String.join "\n    , " (List.map assignment fields)
        ++ " }"


extensibleRecord : String -> List ( String, String ) -> String
extensibleRecord ext fields =
    "{ "
        ++ ext
        ++ " | "
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
