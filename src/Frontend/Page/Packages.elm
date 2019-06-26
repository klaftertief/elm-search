module Frontend.Page.Packages exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , toSession
    , update
    , view
    )

import Dict exposing (Dict)
import Elm.Constraint
import Elm.Package
import Elm.Project
import Elm.Search.Index as Index
import Elm.Version
import Frontend.Session as Session exposing (Session)
import Html exposing (Html)
import Html.Attributes
import Http
import Json.Decode
import Logo
import Route
import Set


type Model
    = Model
        { session : Session
        , packages : List Elm.Project.PackageInfo
        , andThens : List Index.Block
        }


type Msg
    = GotPackages (Result Http.Error (List Elm.Project.PackageInfo))
    | GotSearchResult (Result Http.Error (List Index.Block))


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model
        { session = session
        , packages = []
        , andThens = []
        }
    , Cmd.batch
        [ Http.get
            { url = "/api/packages"
            , expect = Http.expectJson GotPackages packagesDecoder
            }
        , Http.get
            { url = "/api/search?l=0&q=(a -> f b) -> f a -> f b"
            , expect = Http.expectJson GotSearchResult searchResultDecoder
            }
        ]
    )


packagesDecoder : Json.Decode.Decoder (List Elm.Project.PackageInfo)
packagesDecoder =
    Json.Decode.field "response"
        (Json.Decode.list (Json.Decode.field "package" Elm.Project.decoder)
            |> Json.Decode.map
                (List.filterMap
                    (\project ->
                        case project of
                            Elm.Project.Application _ ->
                                Nothing

                            Elm.Project.Package info ->
                                Just info
                    )
                )
        )


searchResultDecoder : Json.Decode.Decoder (List Index.Block)
searchResultDecoder =
    Json.Decode.field "response" (Json.Decode.list Json.Decode.value)
        |> Json.Decode.map (List.map (Json.Decode.decodeValue Index.blockDecoder >> Result.toMaybe))
        |> Json.Decode.map (List.filterMap identity)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        GotPackages (Ok packages) ->
            ( Model
                { model
                    | packages = packages
                }
            , Cmd.none
            )

        GotPackages (Err _) ->
            ( Model
                { model
                    | packages = []
                }
            , Cmd.none
            )

        GotSearchResult (Ok blocks) ->
            ( Model
                { model
                    | andThens = blocks
                }
            , Cmd.none
            )

        GotSearchResult (Err _) ->
            ( Model
                { model
                    | andThens = []
                }
            , Cmd.none
            )


view : Model -> { title : String, body : Html Msg }
view model =
    { title = "Elm Search - Statistics"
    , body = viewContent model
    }


viewContent : Model -> Html Msg
viewContent (Model model) =
    let
        packages =
            model.packages

        numberOfPackages =
            List.length packages

        numberOfUsers =
            packages
                |> List.map (.name >> Elm.Package.userName)
                |> Set.fromList
                |> Set.size

        majorVersionFrequencies =
            packages
                |> List.map (.version >> Elm.Version.toTuple >> majorVersion)
                |> frequencies
                |> Dict.toList

        exposedModulesFrequencies =
            packages
                |> List.map (.exposed >> exposedCount)
                |> frequencies
                |> Dict.toList

        dependencyCountFrequencies =
            packages
                |> List.map (.deps >> List.length)
                |> frequencies
                |> Dict.toList

        dependencyFrequencies =
            packages
                |> List.concatMap (.deps >> List.map (Tuple.first >> Elm.Package.toString))
                |> frequencies
                |> Dict.toList

        upperBoundsDependencyFrequencies =
            packages
                |> List.concatMap
                    (.deps
                        >> List.map
                            (\( name, constraint ) ->
                                Elm.Package.toString name
                                    ++ "/"
                                    ++ (Elm.Constraint.upperBound constraint
                                            |> Elm.Version.toString
                                       )
                            )
                    )
                |> frequencies
                |> Dict.toList

        andThenValues =
            List.filterMap andThenValueBlock model.andThens

        andThenNameFrequencies =
            andThenValues
                |> List.map (.info >> .name)
                |> frequencies
                |> Dict.toList
    in
    Html.div [ Html.Attributes.class "page-search" ]
        [ Html.header []
            [ Html.a
                [ Html.Attributes.class "logo"
                , Route.href Route.Home
                ]
                [ Logo.viewWithSize 64 ]
            , Html.div
                [ Html.Attributes.class "container"
                ]
                [ Html.h2 [] [ Html.text "Statistics" ]
                ]
            ]
        , Html.main_ []
            [ Html.div
                [ Html.Attributes.class "container"
                ]
                [ Html.p []
                    [ Html.text "There are in total "
                    , Html.strong [] [ Html.text (String.fromInt numberOfPackages) ]
                    , Html.text " packages by "
                    , Html.strong [] [ Html.text (String.fromInt numberOfUsers) ]
                    , Html.text " users."
                    ]
                , Html.p []
                    [ Html.text "The distribution of major versions is "
                    , Html.ul []
                        (List.map
                            (\( number, count ) ->
                                Html.li []
                                    [ Html.strong [] [ Html.text (String.fromInt number) ]
                                    , Html.text " -> "
                                    , Html.strong [] [ Html.text (String.fromInt count) ]
                                    ]
                            )
                            (List.sortBy Tuple.second majorVersionFrequencies |> List.reverse)
                        )
                    , Html.p []
                        [ Html.text "The distribution of number of exposed modules is "
                        , Html.ul []
                            (List.map
                                (\( number, count ) ->
                                    Html.li []
                                        [ Html.strong [] [ Html.text (String.fromInt number) ]
                                        , Html.text " -> "
                                        , Html.strong [] [ Html.text (String.fromInt count) ]
                                        ]
                                )
                                (List.sortBy Tuple.second exposedModulesFrequencies |> List.reverse)
                            )
                        ]
                    , Html.p []
                        [ Html.text "The distribution of number of dependencies is "
                        , Html.ul []
                            (List.map
                                (\( number, count ) ->
                                    Html.li []
                                        [ Html.strong [] [ Html.text (String.fromInt number) ]
                                        , Html.text " -> "
                                        , Html.strong [] [ Html.text (String.fromInt count) ]
                                        ]
                                )
                                (List.sortBy Tuple.second dependencyCountFrequencies |> List.reverse)
                            )
                        ]
                    , Html.p []
                        [ Html.text "The distribution of dependencies is "
                        , Html.ul []
                            (List.map
                                (\( name, count ) ->
                                    Html.li []
                                        [ Html.strong [] [ Html.text name ]
                                        , Html.text " -> "
                                        , Html.strong [] [ Html.text (String.fromInt count) ]
                                        ]
                                )
                                (List.sortBy Tuple.second dependencyFrequencies |> List.reverse)
                            )
                        ]
                    , Html.p []
                        [ Html.text "The distribution of dependencies with upper bounds is "
                        , Html.ul []
                            (List.map
                                (\( name, count ) ->
                                    Html.li []
                                        [ Html.strong [] [ Html.text name ]
                                        , Html.text " -> "
                                        , Html.strong [] [ Html.text (String.fromInt count) ]
                                        ]
                                )
                                (List.sortBy Tuple.second upperBoundsDependencyFrequencies |> List.reverse)
                            )
                        ]
                    , Html.p []
                        [ Html.text "There are "
                        , Html.strong [] [ Html.text (List.length andThenValues |> String.fromInt) ]
                        , Html.text " functions with an "
                        , Html.strong [] [ Html.text "andThen" ]
                        , Html.text "-like type signature."
                        ]
                    , Html.ul []
                        (List.map
                            (\value ->
                                Html.li []
                                    [ Html.strong [] [ Html.text (Index.exposedIdentifierToString value.identifier) ]
                                    , Html.text ": "
                                    , Html.code [] [ Html.text (Index.elmTypeToText False value.info.tipe) ]
                                    ]
                            )
                            andThenValues
                        )
                    , Html.p []
                        [ Html.text "The distribution of andThen-like names is "
                        , Html.ul []
                            (List.map
                                (\( name, count ) ->
                                    Html.li []
                                        [ Html.strong [] [ Html.text name ]
                                        , Html.text " -> "
                                        , Html.strong [] [ Html.text (String.fromInt count) ]
                                        ]
                                )
                                (List.sortBy Tuple.second andThenNameFrequencies |> List.reverse)
                            )
                        ]
                    ]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


toSession : Model -> Session
toSession (Model { session }) =
    session


type alias Frequencies comparable =
    Dict comparable Int


frequencies : List comparable -> Frequencies comparable
frequencies list =
    let
        populateDict item dict =
            Dict.update item
                (Maybe.map ((+) 1)
                    >> Maybe.withDefault 1
                    >> Just
                )
                dict
    in
    List.foldr populateDict Dict.empty list


majorVersion : ( Int, Int, Int ) -> Int
majorVersion ( major, _, _ ) =
    major


exposedCount : Elm.Project.Exposed -> Int
exposedCount exposed =
    case exposed of
        Elm.Project.ExposedList list ->
            List.length list

        Elm.Project.ExposedDict list ->
            list
                |> List.map (Tuple.second >> List.length)
                |> List.sum


andThenValueBlock : Index.Block -> Maybe Index.ValueBlock
andThenValueBlock block =
    case block of
        Index.Value v ->
            if v.info.name == "filterMap" then
                Nothing

            else
                Just v

        _ ->
            Nothing
