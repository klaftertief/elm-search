module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Url


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , rawSearchQuery : String
    , searchResults : List String
    }


type alias BackendModel =
    {}


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | EnteredSearchQuery String


type ToBackend
    = SearchQuerySubmitted String


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = SearchResultSent (List String)
