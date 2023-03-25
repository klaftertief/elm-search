module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import Docs
import Elm.Type
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Lamdera
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , rawSearchQuery = ""
      , searchResults = []
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        EnteredSearchQuery rawSearchQuery ->
            ( { model | rawSearchQuery = rawSearchQuery }
            , Lamdera.sendToBackend (SearchQuerySubmitted rawSearchQuery)
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        SearchResultSent names ->
            ( { model | searchResults = names }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "elm-search"
    , body =
        [ styles
        , Html.div
            [ Html.Attributes.class "text-center pt-8 pb-4 space-y-8 bg-green-100"
            ]
            [ Html.h1 []
                [ Html.text "elm-search" ]
            , Html.form
                [ Html.Attributes.class "flex justify-between mx-4 gap-4" ]
                [ Html.input
                    [ Html.Attributes.type_ "search"
                    , Html.Attributes.class "flex-1 bg-gray-100 px-4 py-2 rounded"
                    , Html.Attributes.class "focus:outline-none focus:ring-4 focus:ring-green-500 focus:ring-opacity-50"
                    , Html.Attributes.placeholder "e.g. (a -> b) -> List a -> List b"
                    , Html.Attributes.value model.rawSearchQuery
                    , Html.Events.onInput EnteredSearchQuery
                    ]
                    []
                , Html.button
                    [ Html.Attributes.class "bg-green-700 text-white px-4 py-2 rounded"
                    , Html.Attributes.class "focus:outline-none focus:ring-4 focus:ring-green-500 focus:ring-opacity-50"
                    ]
                    [ Html.text "Search" ]
                ]
            ]
        , Html.div
            [ Html.Attributes.class "p-4" ]
            [ model.rawSearchQuery
                |> Json.Encode.string
                |> Json.Decode.decodeValue Elm.Type.decoder
                |> Result.map
                    (\tipe ->
                        Docs.viewValue
                            { author = ""
                            , project = ""
                            , moduleName = ""
                            , typeNameDict = Dict.empty
                            , version = Nothing
                            }
                            { name = ""
                            , comment = ""
                            , tipe = tipe
                            }
                    )
                |> Result.withDefault (Html.text "invalid type")
            ]
        , Html.div
            [ Html.Attributes.class "p-4" ]
            (model.searchResults
                |> List.map (\name -> Html.p [] [ Html.text name ])
            )
        ]
    }


styles : Html msg
styles =
    Html.node "link"
        [ Html.Attributes.rel "stylesheet"
        , Html.Attributes.href "styles.css"
        ]
        []
