module Search.Update exposing (..)

import Package.Package as Package exposing (Package)
import Search.Model as Model exposing (..)


init : Filter -> List Package -> ( Model, Cmd Msg )
init filter packages =
    let
        ( model, cmd ) =
            update (BuildIndex packages) { initialModel | filter = filter }
    in
        case filter.query of
            Just query ->
                update RunFilter model

            Nothing ->
                ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BuildIndex packages ->
            ( { model | index = buildIndex packages }
            , Cmd.none
            )

        SetFilterQueryFrom queryString ->
            let
                filterFacts =
                    model.filter

                filter =
                    { filterFacts
                        | queryString = queryString
                        , query = maybeQueryFromString queryString
                    }
            in
                ( { model | filter = filter }
                , Cmd.none
                )

        SetFilterVersionFrom versionString ->
            let
                filterFacts =
                    model.filter

                filter =
                    { filterFacts
                        | elmVersion = maybeVersionFromString versionString
                    }
            in
                ( { model | filter = filter }
                , Cmd.none
                )

        RunFilter ->
            ( { model | result = runFilter model.filter model.index }
            , Cmd.none
            )
