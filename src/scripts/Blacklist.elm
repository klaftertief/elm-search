module Blacklist exposing (contains, length)


length : Int
length =
    2


contains : { a | user : String, name : String } -> Bool
contains { user, name } =
    (user == "turboMack" && name == "tea-component")
        || (user == "christophp" && name == "elm-i18next")
