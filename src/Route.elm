module Route exposing (Route(..), fromUrl, href, pushUrl, replaceUrl, toString)

import Browser.Navigation as Navigation
import Elm.Search.Index as Index
import Html
import Html.Attributes
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query


type Route
    = Home
    | Search (Maybe String)
    | Packages
    | Package Index.PackageIdentifier
    | ExposedNames


parser : Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map Home Url.Parser.top
        , Url.Parser.map Search (Url.Parser.s "search" <?> Url.Parser.Query.string "q")
        , Url.Parser.map Packages (Url.Parser.s "packages")
        , Url.Parser.map Package (Url.Parser.s "packages" </> Index.packageIdentifierUrlParser)
        , Url.Parser.map ExposedNames (Url.Parser.s "exposed-names")
        ]


href : Route -> Html.Attribute msg
href route =
    Html.Attributes.href (toString route)


pushUrl : Navigation.Key -> Route -> Cmd msg
pushUrl key route =
    Navigation.pushUrl key (toString route)


replaceUrl : Navigation.Key -> Route -> Cmd msg
replaceUrl key route =
    Navigation.replaceUrl key (toString route)


fromUrl : Url -> Maybe Route
fromUrl =
    Url.Parser.parse parser


toString : Route -> String
toString route =
    case route of
        Home ->
            Url.Builder.absolute [] []

        Search maybeQuery ->
            Url.Builder.absolute [ "search" ]
                (case maybeQuery of
                    Just query ->
                        [ Url.Builder.string "q" query ]

                    Nothing ->
                        []
                )

        Packages ->
            Url.Builder.absolute [ "packages" ] []

        Package id ->
            Url.Builder.absolute
                [ "packages", Index.packageIdentifierToString id ]
                []

        ExposedNames ->
            Url.Builder.absolute [ "exposed-names" ] []
