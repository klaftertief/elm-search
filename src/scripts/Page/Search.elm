module Page.Search (..) where

import Effects as Fx exposing (Effects)
import Html exposing (..)
import StartApp
import Task
import Component.Search as Search


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = [ Signal.map UpdateSearch Search.querySignal ]
    }


main : Signal Html
main =
  app.html


port worker : Signal (Task.Task Fx.Never ())
port worker =
  app.tasks



-- MODEL


type alias Model =
  { search : Search.Model
  }



-- INIT


init : ( Model, Effects Action )
init =
  let
    ( search, searchFx ) =
      Search.init
  in
    ( Model search
    , Fx.map UpdateSearch searchFx
    )



-- UPDATE


type Action
  = UpdateSearch Search.Action


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    UpdateSearch act ->
      let
        ( newSearch, fx ) =
          Search.update act model.search
      in
        ( { model | search = newSearch }
        , Fx.map UpdateSearch fx
        )



-- VIEW


view : Signal.Address Action -> Model -> Html
view addr model =
  Search.view (Signal.forwardTo addr UpdateSearch) model.search
