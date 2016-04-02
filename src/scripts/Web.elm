module Web (..) where

import Effects exposing (Effects)
import Html exposing (Html)
import StartApp
import Task exposing (Task)
import Component.Search as Search


app : StartApp.App Search.Model
app =
  StartApp.start
    { init = Search.init
    , view = Search.view
    , update = Search.update
    , inputs = [ Search.querySignal ]
    }


main : Signal Html
main =
  app.html


port worker : Signal (Task Effects.Never ())
port worker =
  app.tasks
