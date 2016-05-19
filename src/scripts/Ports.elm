port module Ports exposing (..)


port query : (String -> msg) -> Sub msg


port pushQuery : String -> Cmd msg
