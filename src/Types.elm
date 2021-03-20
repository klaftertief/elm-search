module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Search.Chunk
import Search.Model
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , queryString : String
    , searchResult : List Search.Chunk.Chunk
    }


type alias BackendModel =
    { chunks : List Search.Chunk.Chunk
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | EnteredSearchInput String
    | SubmittedSearch


type ToBackend
    = QueryToBackend Search.Model.Query


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = SearchResultToFrontend (List Search.Chunk.Chunk)
