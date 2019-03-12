module Elm.Search.Query exposing (Query(..))


type Query
    = ValueName String
    | ValueType String
    | UnionName String
    | UnionConstructorName String
    | UnionConstructorType String
    | AliasName String
    | AliasType String
