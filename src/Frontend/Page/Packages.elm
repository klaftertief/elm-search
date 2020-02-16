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

        andThenValues =
            List.filterMap andThenValueBlock model.andThens

        andThenValuesCount =
            List.length andThenValues

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
                [ Html.h2 [] [ Html.text "Packages" ]
                , Html.p []
                    [ Html.text "There are in total "
                    , Html.strong [] [ Html.text (String.fromInt numberOfPackages) ]
                    , Html.text " packages by "
                    , Html.strong [] [ Html.text (String.fromInt numberOfUsers) ]
                    , Html.text " users."
                    ]
                , Html.div []
                    [ Html.h3 [] [ Html.text "Major Versions" ]
                    , Html.ul [ Html.Attributes.class "bars" ]
                        (List.map
                            (\( number, count ) ->
                                Html.li
                                    [ bar numberOfPackages count ]
                                    [ Html.text (String.fromInt number)
                                    ]
                            )
                            (List.sortBy Tuple.second majorVersionFrequencies |> List.reverse)
                        )
                    , Html.div []
                        [ Html.h3 [] [ Html.text "Number of exposed modules" ]
                        , Html.ul [ Html.Attributes.class "bars" ]
                            (List.map
                                (\( number, count ) ->
                                    Html.li
                                        [ bar numberOfPackages count ]
                                        [ Html.strong [] [ Html.text (String.fromInt number) ]
                                        ]
                                )
                                (List.sortBy Tuple.second exposedModulesFrequencies |> List.reverse)
                            )
                        ]
                    , Html.div []
                        [ Html.h3 [] [ Html.text "Number of dependencies" ]
                        , Html.ul [ Html.Attributes.class "bars" ]
                            (List.map
                                (\( number, count ) ->
                                    Html.li [ bar numberOfPackages count ]
                                        [ Html.strong [] [ Html.text (String.fromInt number) ]
                                        ]
                                )
                                (List.sortBy Tuple.second dependencyCountFrequencies |> List.reverse)
                            )
                        ]
                    , Html.div []
                        [ Html.h3 [] [ Html.text "Distribution of dependencies" ]
                        , Html.ul [ Html.Attributes.class "bars" ]
                            (List.map
                                (\( name, count ) ->
                                    Html.li [ bar numberOfPackages count ]
                                        [ Html.strong [] [ Html.text name ]
                                        ]
                                )
                                (List.sortBy Tuple.second dependencyFrequencies |> List.reverse)
                            )
                        ]
                    , Html.h2 [] [ Html.text "Type Classes" ]
                    , Html.p []
                        [ Html.text "There are "
                        , Html.strong [] [ Html.text (andThenValuesCount |> String.fromInt) ]
                        , Html.text " functions with an "
                        , Html.strong [] [ Html.text "andThen" ]
                        , Html.text "-like type signature: "
                        , Html.strong [] [ Html.code [] [ Html.text "(a -> f b) -> f a -> f b" ] ]
                        ]
                    , Html.dl []
                        (List.concatMap
                            (\value ->
                                [ Html.dt []
                                    [ Html.text (Index.exposedIdentifierToString value.identifier)
                                    ]
                                , Html.dd []
                                    [ Html.code [] [ Html.text (Index.elmTypeToText False value.info.tipe) ]
                                    ]
                                ]
                            )
                            andThenValues
                        )
                    , Html.div []
                        [ Html.h3 [] [ Html.text "Distribution of andThen-like names" ]
                        , Html.ul [ Html.Attributes.class "bars" ]
                            (List.map
                                (\( name, count ) ->
                                    Html.li [ bar andThenValuesCount count ]
                                        [ Html.strong [] [ Html.text name ]
                                        ]
                                )
                                (List.sortBy Tuple.second andThenNameFrequencies |> List.reverse)
                            )
                        ]
                    ]
                ]
            ]
        ]


bar : Int -> Int -> Html.Attribute msg
bar total v =
    Html.Attributes.style "background-size"
        (String.fromFloat ((toFloat v / toFloat total) * 100) ++ "% 100%")


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
