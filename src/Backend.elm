port module Backend exposing (main)

import Dict exposing (Dict)
import Elm.Docs
import Elm.Module
import Elm.Package
import Elm.Project
import Elm.Search.Result
import Elm.Type
import Json.Decode
import Json.Encode


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { packages : Dict String Package }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { packages = Dict.empty }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotPackageJson Json.Decode.Value
    | GotSearchRequest String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPackageJson packageJson ->
            case Json.Decode.decodeValue packageDecoder packageJson of
                Ok package ->
                    ( { model | packages = insertPackage package model.packages }
                    , Cmd.none
                    )

                Err err ->
                    ( model, Cmd.none )

        GotSearchRequest queryString ->
            ( model
            , result
                (Json.Encode.object
                    [ ( "query", Json.Encode.string queryString )
                    , ( "result"
                      , model.packages
                            |> Dict.values
                            |> (\packages ->
                                    []
                                        ++ searchUnionsByName queryString packages
                                        ++ searchAliasesByName queryString packages
                                        ++ searchValuesByName queryString packages
                                        ++ searchBinopsByName queryString packages
                                        ++ searchUnionsByComment queryString packages
                                        ++ searchAliasesByComment queryString packages
                                        ++ searchValuesByComment queryString packages
                                        ++ searchBinopsByComment queryString packages
                               )
                            |> Json.Encode.list Elm.Search.Result.encodeBlock
                      )
                    ]
                )
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ addPackage GotPackageJson
        , search GotSearchRequest
        ]



-- PORTS


port addPackage : (Json.Decode.Value -> msg) -> Sub msg


port search : (String -> msg) -> Sub msg


port result : Json.Decode.Value -> Cmd msg



-- PACKAGE


type alias Package =
    { info : Elm.Project.PackageInfo
    , readme : String
    , modules : List Elm.Docs.Module
    }


insertPackage : Package -> Dict String Package -> Dict String Package
insertPackage package =
    Dict.insert (Elm.Package.toString package.info.name) package


packageDecoder : Json.Decode.Decoder Package
packageDecoder =
    Json.Decode.map3 Package
        (Json.Decode.field "package" packageInfoDecoder)
        (Json.Decode.field "readme" Json.Decode.string)
        (Json.Decode.field "docs" docsDecoder)


packageInfoDecoder : Json.Decode.Decoder Elm.Project.PackageInfo
packageInfoDecoder =
    Json.Decode.andThen
        (\project ->
            case project of
                Elm.Project.Package packageInfo ->
                    Json.Decode.succeed packageInfo

                Elm.Project.Application _ ->
                    Json.Decode.fail "Applications are not supported"
        )
        Elm.Project.decoder


docsDecoder : Json.Decode.Decoder (List Elm.Docs.Module)
docsDecoder =
    Json.Decode.list Elm.Docs.decoder



-- SEARCH


searchUnionsByName : String -> List Package -> List Elm.Search.Result.Block
searchUnionsByName =
    searchPackagesEntries (searchPackageEntries searchModuleUnionsByName)


searchAliasesByName : String -> List Package -> List Elm.Search.Result.Block
searchAliasesByName =
    searchPackagesEntries (searchPackageEntries searchModuleAliasesByName)


searchValuesByName : String -> List Package -> List Elm.Search.Result.Block
searchValuesByName =
    searchPackagesEntries (searchPackageEntries searchModuleValuesByName)


searchBinopsByName : String -> List Package -> List Elm.Search.Result.Block
searchBinopsByName =
    searchPackagesEntries (searchPackageEntries searchModuleBinopsByName)


searchUnionsByComment : String -> List Package -> List Elm.Search.Result.Block
searchUnionsByComment =
    searchPackagesEntries (searchPackageEntries searchModuleUnionsByComment)


searchAliasesByComment : String -> List Package -> List Elm.Search.Result.Block
searchAliasesByComment =
    searchPackagesEntries (searchPackageEntries searchModuleAliasesByComment)


searchValuesByComment : String -> List Package -> List Elm.Search.Result.Block
searchValuesByComment =
    searchPackagesEntries (searchPackageEntries searchModuleValuesByComment)


searchBinopsByComment : String -> List Package -> List Elm.Search.Result.Block
searchBinopsByComment =
    searchPackagesEntries (searchPackageEntries searchModuleBinopsByComment)


searchPackagesEntries :
    (String -> Package -> List Elm.Search.Result.Block)
    -> String
    -> List Package
    -> List Elm.Search.Result.Block
searchPackagesEntries searchPackage =
    searchPackage >> List.concatMap


searchPackageEntries :
    (String -> Elm.Search.Result.PackageIdentifier -> Elm.Docs.Module -> List Elm.Search.Result.Block)
    -> String
    -> Package
    -> List Elm.Search.Result.Block
searchPackageEntries searchModule name package =
    List.concatMap (searchModule name <| toPackageIdentifier package)
        package.modules


searchModuleUnionsByName : String -> Elm.Search.Result.PackageIdentifier -> Elm.Docs.Module -> List Elm.Search.Result.Block
searchModuleUnionsByName =
    searchModuleEntriesByName
        { entries = .unions
        , toBlock = Elm.Search.Result.Union
        }


searchModuleAliasesByName : String -> Elm.Search.Result.PackageIdentifier -> Elm.Docs.Module -> List Elm.Search.Result.Block
searchModuleAliasesByName =
    searchModuleEntriesByName
        { entries = .aliases
        , toBlock = Elm.Search.Result.Alias
        }


searchModuleValuesByName : String -> Elm.Search.Result.PackageIdentifier -> Elm.Docs.Module -> List Elm.Search.Result.Block
searchModuleValuesByName =
    searchModuleEntriesByName
        { entries = .values
        , toBlock = Elm.Search.Result.Value
        }


searchModuleBinopsByName : String -> Elm.Search.Result.PackageIdentifier -> Elm.Docs.Module -> List Elm.Search.Result.Block
searchModuleBinopsByName =
    searchModuleEntriesByName
        { entries = .binops
        , toBlock = Elm.Search.Result.Binop
        }


searchModuleEntriesByName :
    { entries : Elm.Docs.Module -> List { a | name : String }
    , toBlock : Elm.Search.Result.PackageIdentifier -> Elm.Search.Result.ModuleIdentifier -> { a | name : String } -> Elm.Search.Result.Block
    }
    -> String
    -> Elm.Search.Result.PackageIdentifier
    -> Elm.Docs.Module
    -> List Elm.Search.Result.Block
searchModuleEntriesByName cfg name packageIdentifier mod =
    let
        toResult entry =
            if String.toLower entry.name == String.toLower name then
                Just (cfg.toBlock packageIdentifier (toModuleIdentifier mod) entry)

            else
                Nothing
    in
    List.filterMap toResult (cfg.entries mod)


searchModuleUnionsByComment : String -> Elm.Search.Result.PackageIdentifier -> Elm.Docs.Module -> List Elm.Search.Result.Block
searchModuleUnionsByComment =
    searchModuleEntriesByComment
        { entries = .unions
        , toBlock = Elm.Search.Result.Union
        }


searchModuleAliasesByComment : String -> Elm.Search.Result.PackageIdentifier -> Elm.Docs.Module -> List Elm.Search.Result.Block
searchModuleAliasesByComment =
    searchModuleEntriesByComment
        { entries = .aliases
        , toBlock = Elm.Search.Result.Alias
        }


searchModuleValuesByComment : String -> Elm.Search.Result.PackageIdentifier -> Elm.Docs.Module -> List Elm.Search.Result.Block
searchModuleValuesByComment =
    searchModuleEntriesByComment
        { entries = .values
        , toBlock = Elm.Search.Result.Value
        }


searchModuleBinopsByComment : String -> Elm.Search.Result.PackageIdentifier -> Elm.Docs.Module -> List Elm.Search.Result.Block
searchModuleBinopsByComment =
    searchModuleEntriesByComment
        { entries = .binops
        , toBlock = Elm.Search.Result.Binop
        }


searchModuleEntriesByComment :
    { entries : Elm.Docs.Module -> List { a | comment : String }
    , toBlock : Elm.Search.Result.PackageIdentifier -> Elm.Search.Result.ModuleIdentifier -> { a | comment : String } -> Elm.Search.Result.Block
    }
    -> String
    -> Elm.Search.Result.PackageIdentifier
    -> Elm.Docs.Module
    -> List Elm.Search.Result.Block
searchModuleEntriesByComment cfg query packageIdentifier mod =
    let
        toResult entry =
            if String.contains (String.toLower query) (String.toLower entry.comment) then
                Just (cfg.toBlock packageIdentifier (toModuleIdentifier mod) entry)

            else
                Nothing
    in
    List.filterMap toResult (cfg.entries mod)


toModuleIdentifier : Elm.Docs.Module -> Elm.Search.Result.ModuleIdentifier
toModuleIdentifier mod =
    { name = mod.name }


toPackageIdentifier : Package -> Elm.Search.Result.PackageIdentifier
toPackageIdentifier package =
    { name = package.info.name
    , version = package.info.version
    }
