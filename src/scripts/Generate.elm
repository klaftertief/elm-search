module Generate exposing (elm)

import Docs.Package exposing (Entry, Module, Package)
import Docs.Type exposing (Type(..))


elm : List Package -> String
elm packages =
    String.join "\n"
        [ "module Main exposing (..)"
        , ""
        , "import Web"
        , "import Docs.Type exposing (Type(..))"
        , ""
        , "main ="
        , "  Web.program"
        , "    [ " ++ String.join "\n    , " (List.map fromPackage packages)
        , "    ]"
        ]


fromPackage : Package -> String
fromPackage { user, name, version, modules } =
    record
        [ ( "user", string user )
        , ( "name", string name )
        , ( "version", string version )
        , ( "modules", list fromModule modules )
        ]


fromModule : Module -> String
fromModule { name, entries, elmVersion } =
    record
        [ ( "name", string name )
        , ( "elmVersion", maybe string elmVersion )
        , ( "entries", list fromEntry entries )
        ]


fromEntry : Entry -> String
fromEntry { name, docs, tipe } =
    record
        [ ( "name", string name )
        , ( "docs", string docs )
        , ( "tipe", fromType tipe )
        ]


fromType : Type -> String
fromType tipe =
    withParens <|
        case tipe of
            Function args last ->
                "Function "
                    ++ list fromType args
                    ++ " "
                    ++ withParens (fromType last)

            Var name ->
                "Var " ++ string name

            Apply { home, name } args ->
                "Apply "
                    ++ record
                        [ ( "name", string name )
                        , ( "home", string home )
                        ]
                    ++ " "
                    ++ list fromType args

            Tuple args ->
                "Tuple "
                    ++ list fromType args

            Record pairs extensible ->
                "Record "
                    ++ list (tuple string fromType) pairs
                    ++ " "
                    ++ withParens (maybe string extensible)


withParens : String -> String
withParens value =
    "(" ++ value ++ ")"


tuple : (a -> String) -> (b -> String) -> ( a, b ) -> String
tuple f g ( a, b ) =
    withParens <| f a ++ ", " ++ g b


string : String -> String
string value =
    "\"" ++ String.foldl stringEscape "" value ++ "\""


stringEscape : Char -> String -> String
stringEscape next acc =
    String.append acc <|
        case next of
            '"' ->
                "\\\""

            '\\' ->
                "\\\\"

            '\n' ->
                "\\n"

            '\t' ->
                "\\t"

            '\x0D' ->
                "\\r"

            _ ->
                String.fromChar next


maybe : (a -> String) -> Maybe a -> String
maybe f =
    Maybe.map (\value -> "Just " ++ f value)
        >> Maybe.withDefault "Nothing"


list : (a -> String) -> List a -> String
list f values =
    "[" ++ String.join ", " (List.map f values) ++ "]"


record : List ( String, String ) -> String
record fields =
    "{ "
        ++ String.join ", " (List.map recordField fields)
        ++ " }"


recordField : ( String, String ) -> String
recordField ( key, value ) =
    key ++ " = " ++ value
