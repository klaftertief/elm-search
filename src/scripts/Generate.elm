module Generate exposing (elm)

import Docs.Package exposing (Entry, Module, Package)
import Docs.Type exposing (Type(..))


elm : List Package -> String
elm packages =
    let
        packageDefs =
            List.map fromPackage packages
    in
    String.join "\n" <|
        [ "module Main exposing (..)"
        , "import Web"
        , "import Docs.Type"
        , "main = Web.program " ++ chunkedList .inline packageDefs
        ]
            ++ List.concatMap (List.map assignment << .defs) packageDefs


fromPackage : Package -> { defs : List ( String, String ), inline : String }
fromPackage { user, name, version, modules } =
    let
        moduleDefs =
            List.map (fromModule user name) modules
    in
    { defs =
        List.map .def moduleDefs
    , inline =
        record
            [ ( "user", string user )
            , ( "name", string name )
            , ( "version", string version )
            , ( "modules", list .inline moduleDefs )
            ]
    }


fromModule :
    String
    -> String
    -> Module
    -> { def : ( String, String ), inline : String }
fromModule packageUser packageName { name, entries, elmVersion } =
    let
        entryListDefName =
            safeName <| packageUser ++ "__" ++ packageName ++ "__" ++ name
    in
    { def =
        ( entryListDefName, chunkedList fromEntry entries )
    , inline =
        record
            [ ( "name", string name )
            , ( "elmVersion", maybe string elmVersion )
            , ( "entries", entryListDefName )
            ]
    }


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
                "Docs.Type.Function "
                    ++ list fromType args
                    ++ " "
                    ++ withParens (fromType last)

            Var name ->
                "Docs.Type.Var " ++ string name

            Apply { home, name } args ->
                "Docs.Type.Apply "
                    ++ record
                        [ ( "name", string name )
                        , ( "home", string home )
                        ]
                    ++ " "
                    ++ list fromType args

            Tuple args ->
                "Docs.Type.Tuple "
                    ++ list fromType args

            Record pairs extensible ->
                "Docs.Type.Record "
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
        ++ String.join ", " (List.map assignment fields)
        ++ " }"


assignment : ( String, String ) -> String
assignment ( left, right ) =
    left ++ " = " ++ right


safeName : String -> String
safeName str =
    "v_" ++ String.map replaceUnsafe str


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
            withParens
                (String.join " ++ " acc)

        chunk ->
            chunkedListHelp func
                (List.drop chunkSize values)
                (list func chunk :: acc)
