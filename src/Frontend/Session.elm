module Frontend.Session exposing (Session, fromNavKey, navKey)

import Browser.Navigation as Navigation


type Session
    = Session Navigation.Key


fromNavKey : Navigation.Key -> Session
fromNavKey =
    Session


navKey : Session -> Navigation.Key
navKey (Session key) =
    key
