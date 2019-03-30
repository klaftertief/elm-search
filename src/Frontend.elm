module Frontend exposing (main)

import Browser
import Browser.Navigation as Navigation
import Elm.Package
import Elm.Search.Result
import Elm.Version
import Frontend.Page.Home as Home
import Frontend.Page.Packages as Packages
import Frontend.Page.Search as Search
import Frontend.Route as Route exposing (Route)
import Frontend.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import Markdown
import Url exposing (Url)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Model
    = Redirect Session
    | NotFound Session
    | Home Home.Model
    | Search Search.Model
    | Packages Packages.Model


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromNavKey navKey))



-- UPDATE


type Msg
    = Ignored
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | HomeMsg Home.Msg
    | SearchMsg Search.Msg
    | PackagesMsg Packages.Msg


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Home home ->
            Home.toSession home

        Search search ->
            Search.toSession search

        Packages packages ->
            Packages.toSession packages


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Home ->
            Home.init session
                |> updateWith Home HomeMsg model

        Just (Route.Search maybeQuery) ->
            Search.init session maybeQuery
                |> updateWith Search SearchMsg model

        Just Route.Packages ->
            Packages.init session
                |> updateWith Packages PackagesMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl (Session.navKey (toSession model)) (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( HomeMsg homeMsg, Home home ) ->
            Home.update homeMsg home
                |> updateWith Home HomeMsg model

        ( SearchMsg searchMsg, Search search ) ->
            Search.update searchMsg search
                |> updateWith Search SearchMsg model

        ( PackagesMsg packagesMsg, Packages packages ) ->
            Packages.update packagesMsg packages
                |> updateWith Packages PackagesMsg model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        viewPage toMsg { title, body } =
            { title = title
            , body = [ Html.map toMsg body ]
            }
    in
    case model of
        Redirect _ ->
            viewPage (\_ -> Ignored) { title = "Elm Search", body = Html.text "Redirecting" }

        NotFound _ ->
            viewPage (\_ -> Ignored) { title = "Elm Search", body = Html.text "Not Found" }

        Home home ->
            viewPage HomeMsg (Home.view home)

        Search search ->
            viewPage SearchMsg (Search.view search)

        Packages packages ->
            viewPage PackagesMsg (Packages.view packages)
