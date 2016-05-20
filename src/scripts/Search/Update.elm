module Search.Update exposing (..)

import Package.Package as Package exposing (Package)
import Package.Version as Version
import Search.Model as Model exposing (..)


init : List Package -> ( Model, Cmd Msg )
init packages =
    update (BuildIndex packages) initialModel


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
                        | query = maybeQueryFromString queryString
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
