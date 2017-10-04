module Blacklist exposing (contains, length)

import Docs.Package exposing (Package)


length : Int
length =
    2


contains : Package -> Bool
contains { user, name } =
    (user == "turboMack" && name == "tea-component")
        || (user == "christophp" && name == "elm-i18next")
