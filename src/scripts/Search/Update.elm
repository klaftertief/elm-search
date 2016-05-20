module Search.Update exposing (..)

import Package.Package as Package exposing (Package)
import Search.Model as Model exposing (..)


init : Filter -> List Package -> Model
init filter packages =
    let
        model =
            update (BuildIndex packages) { initialModel | filter = filter }

        _ =
            Debug.log "#chunks" (List.length model.index.chunks)
    in
        case filter.query of
            Just query ->
                update RunFilter model

            Nothing ->
                model


update : Msg -> Model -> Model
update msg model =
    case msg of
        BuildIndex packages ->
            { model | index = buildIndex packages }

        SetFilter filter ->
            { model | filter = filter }

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
                { model | filter = filter }

        SetFilterVersionFrom versionString ->
            let
                filterFacts =
                    model.filter

                filter =
                    { filterFacts
                        | elmVersion = maybeVersionFromString versionString
                    }
            in
                { model | filter = filter }

        RunFilter ->
            { model | result = runFilter model.filter model.index }
