module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , rawSearchQuery : String
    , searchResults : List String
    }


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | EnteredSearchQuery String


type ToBackend
    = SearchQuerySubmitted String


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = SearchResultSent (List String)
