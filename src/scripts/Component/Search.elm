module Component.Search (..) where

import Dict
import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Set
import String
import Svg
import Svg.Attributes as SvgA
import Task
import Component.PackageDocs as PDocs
import Docs.Summary as Summary
import Docs.Entry as Entry
import Docs.Name as Name
import Docs.Package as Docs
import Docs.Type as Type
import Docs.Version as Version
import Page.Context as Ctx
import Utils.Path exposing ((</>))
import Signal
import Signal.Time
import Storage
import Html.CssHelpers
import Style.Shared exposing (..)


ns =
  Html.CssHelpers.namespace cssNamespace.name



-- MODEL


type Model
  = Loading
  | Failed Http.Error
  | Catalog (List Summary.Summary)
  | Docs Info


type alias Info =
  { packageDict : Packages
  , chunks : List Chunk
  , failed : List Summary.Summary
  , query : String
  }


type alias PackageIdentifier =
  String


type alias Packages =
  Dict.Dict PackageIdentifier PackageInfo


type alias PackageInfo =
  { package : Docs.Package
  , context : Ctx.VersionContext
  , nameDict : Name.Dictionary
  }


type alias Chunk =
  { package : PackageIdentifier
  , name : Name.Canonical
  , entry : Entry.Model Type.Type
  , entryNormalized : Entry.Model Type.Type
  }



-- INIT


init : ( Model, Effects Action )
init =
  ( Loading
  , getPackageInfo
    --, Fx.none
  )


queryMailbox : Signal.Mailbox Action
queryMailbox =
  Signal.mailbox (Query "")


querySignal : Signal.Signal Action
querySignal =
  Signal.Time.settledAfter 800 queryMailbox.signal



-- UPDATE


type Action
  = Fail Http.Error
  | Load ( List Summary.Summary, List String )
  | FailDocs Summary.Summary
  | RequestDocs Summary.Summary
  | MakeDocs Ctx.VersionContext Docs.Package
  | Query String


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    Query query ->
      flip (,) Fx.none
        <| case model of
            Docs info ->
              Docs { info | query = query }

            _ ->
              model

    Fail httpError ->
      ( Failed httpError
      , Fx.none
      )

    Load ( allSummaries, updatedPkgs ) ->
      let
        updatedSet =
          Set.fromList updatedPkgs

        ( summaries, oldSummaries ) =
          List.partition (\{ name } -> Set.member name updatedSet) allSummaries

        contextEffects =
          List.map getDocsFromStorage summaries
      in
        ( Catalog summaries
        , Fx.batch contextEffects
        )

    FailDocs summary ->
      case model of
        Docs info ->
          ( Docs { info | failed = summary :: info.failed }
          , Fx.none
          )

        _ ->
          ( Docs (Info (Dict.empty) [] [ summary ] "")
          , Fx.none
          )

    RequestDocs summary ->
      ( model
      , getDocs summary
      )

    MakeDocs ctx docs ->
      let
        { user, project, version } =
          ctx

        pkgName =
          user </> project </> version

        pkgInfo =
          PackageInfo docs ctx (PDocs.toNameDict docs)

        chunks =
          docs
            |> Dict.toList
            |> List.concatMap (\( name, moduleDocs ) -> toChunks pkgName moduleDocs)
      in
        case model of
          Docs info ->
            ( Docs
                { info
                  | packageDict = Dict.insert pkgName pkgInfo info.packageDict
                  , chunks = List.append info.chunks chunks
                }
            , Fx.none
            )

          _ ->
            ( Docs (Info (Dict.singleton pkgName pkgInfo) chunks [] "(a -> b -> b) -> b -> List a -> b")
            , Fx.none
            )


latestVersionContext : Summary.Summary -> Result String Ctx.VersionContext
latestVersionContext summary =
  let
    userProjectList =
      List.take 2 (String.split "/" summary.name)

    latestVersionSingleton =
      summary.versions
        |> List.take 1
        |> List.map Version.vsnToString
  in
    case List.append userProjectList latestVersionSingleton of
      [ user, project, version ] ->
        Result.Ok
          (Ctx.VersionContext user project version [] Nothing)

      _ ->
        Result.Err
          "Summary is corrupted"



-- EFFECTS


getPackageInfo : Effects Action
getPackageInfo =
  let
    getAll =
      Http.get Summary.decoder "/packages/all-packages.json"

    getNew =
      Http.get (Json.list Json.string) "/packages/new-packages.json"
  in
    Task.map2 (,) getAll getNew
      |> Task.map Load
      |> flip Task.onError (Task.succeed << Fail)
      |> Fx.task


getDocsFromStorage : Summary.Summary -> Effects Action
getDocsFromStorage summary =
  let
    contextResult =
      latestVersionContext summary

    failTask =
      Task.succeed (FailDocs summary)
  in
    case contextResult of
      Result.Ok ({ user, project, version } as context) ->
        Storage.getItem (user </> project </> version) Docs.decodePackage
          |> Task.map (MakeDocs context)
          |> (flip Task.onError) (always (Task.succeed (RequestDocs summary)))
          |> Fx.task

      Result.Err error ->
        Fx.task failTask


getDocs : Summary.Summary -> Effects Action
getDocs summary =
  let
    contextResult =
      latestVersionContext summary

    failTask =
      Task.succeed (FailDocs summary)
  in
    case contextResult of
      Result.Ok ({ user, project, version } as context) ->
        Ctx.getDocs context
          |> Task.mapError (always "Could not get docs")
          |> (flip Task.andThen)
              -- TODO remove existing items of old versions of the same package
              (\docs ->
                (Storage.setItem (user </> project </> version) (Docs.encodePackage docs))
                  |> (flip Task.andThen) (always (Task.succeed docs))
              )
          |> Task.map (MakeDocs context)
          |> (flip Task.onError) (always failTask)
          |> Fx.task

      Result.Err error ->
        Fx.task failTask



-- VIEW


view : Signal.Address Action -> Model -> Html
view addr model =
  div
    [ class "searchApp" ]
    --<| (viewLogo addr model)
    --::
    <|
      case model of
        Loading ->
          [ p [] [ text "Loading list of packages..." ] ]

        Failed httpError ->
          [ p [] [ text "Package summary did not load." ]
          , p [] [ text (toString httpError) ]
          ]

        Catalog catalog ->
          [ p [] [ text <| "Loading docs for " ++ toString (List.length catalog) ++ "packages..." ]
          ]

        Docs info ->
          [ viewSearchInput addr info
          , if String.isEmpty info.query then
              viewSearchIntro addr info
            else
              viewSearchResults addr info
          ]


viewLogo : Signal.Address Action -> Model -> Html
viewLogo address model =
  let
    orange =
      "#F0AD00"

    blue =
      "#60B5CC"

    green =
      "#7FD13B"

    grey =
      "#5A6378"

    halfHypothenuse =
      128 / (sqrt 2)
  in
    div
      [ class "logo"
      , style
          [ ( "padding", "64px 0 32px 0" )
          , ( "text-align", "center" )
          ]
      ]
      [ Svg.svg
          [ SvgA.width "384", SvgA.height "384", SvgA.viewBox "-64 0 384 384" ]
          [ Svg.g
              [ SvgA.stroke "#fff"
              , SvgA.strokeWidth "1"
              , SvgA.strokeLinejoin "round"
              ]
              [ Svg.polygon
                  [ SvgA.points "0,0 0,128 64,64"
                  , SvgA.fill grey
                  , SvgA.transform "translate(0 64) rotate(-45)"
                  ]
                  []
              , Svg.polygon
                  [ SvgA.points "64,0 128,64 128,0"
                  , SvgA.fill blue
                  , SvgA.transform "translate(-64 64) rotate(-45 64 0)"
                  ]
                  []
              , Svg.polygon
                  [ SvgA.points "0,128 128,128 64,64"
                  , SvgA.fill blue
                  , SvgA.transform ("rotate(90 64 64) translate(64 -" ++ toString halfHypothenuse ++ ")")
                  ]
                  []
              , Svg.polygon
                  [ SvgA.points "32,32 64,64 96,32"
                  , SvgA.fill orange
                  , SvgA.transform "translate(-96 32) rotate(-90 96 32)"
                  ]
                  []
              , Svg.polygon
                  [ SvgA.points "0,0 32,32 96,32 64,0"
                  , SvgA.fill green
                  , SvgA.transform ("scale(1 -1) translate(" ++ toString (halfHypothenuse / 2) ++ " -256) rotate(45)")
                  ]
                  []
              , Svg.polygon
                  [ SvgA.points "96,96 128,128 128,64"
                  , SvgA.fill orange
                  , SvgA.transform ("translate(0 128) rotate(225 96 128)")
                  ]
                  []
                {- , Svg.polygon
                [ SvgA.points "64,64 96,96 128,64 96,32"
                , SvgA.fill green
                , SvgA.transform "translate(0 160) rotate(45 96 64)"
                ]
                []
                -}
                {- , Svg.circle
                [ SvgA.cx "0"
                , SvgA.cy "64"
                , SvgA.r "4"
                , SvgA.strokeWidth "0"
                ]
                []
                -}
              ]
          ]
      ]



{-
<path d="M 0 0 L 0 128 L 64 64 Z"/>
<path d="M 0 128 L 128 128 L 64 64 Z"/>
<path d="M 0 0 L 32 32 L 96 32 L 64 0 Z"/>
<path d="M 32 32 L 64 64 L 96 32 Z"/>
<path d="M 64 64 L 96 96 L 128 64 L 96 32 Z"/>
<path d="M 96 96 L 128 128 L 128 64 Z"/>
<path d="M 64 0 L 128 64 L 128 0 Z"/>
-}


viewSearchInput : Signal.Address Action -> Info -> Html
viewSearchInput addr info =
  div
    [ ns.class [ SearchInput ] ]
    [ input
        [ placeholder "Search function by name or type signature"
        , value info.query
        , on "input" targetValue (Signal.message queryMailbox.address << Query)
        ]
        []
    ]


viewSearchIntro : Signal.Address Action -> Info -> Html
viewSearchIntro addr info =
  div
    []
    [ h1 [] [ text "Welcome to the Elm API Search" ]
    , p [] [ text "Search the modules of the latest Elm packages by either function name or by approximate type signature." ]
    , h2 [] [ text "Example searches" ]
    , exampleSearches addr
    , viewPackesInfo info
    ]


exampleSearches : Signal.Address Action -> Html
exampleSearches addr =
  let
    exampleQueries =
      [ "map"
      , "(a -> b -> b) -> b -> List a -> b"
      , "Result x a -> (a -> Result x b) -> Result x b"
      , "(x -> y -> z) -> y -> x -> z"
      ]

    exampleSearchItem query =
      li
        []
        [ a
            [ style [ ( "cursor", "pointer" ) ]
            , onClick addr (Query query)
            ]
            [ text query ]
        ]
  in
    ul [] (List.map exampleSearchItem exampleQueries)


viewPackesInfo : Info -> Html
viewPackesInfo info =
  div
    []
    [ h2 [] [ text "Some statistics" ]
    , p
        []
        [ text "The search index contains "
        , strong [] [ text (toString (Dict.size info.packageDict)) ]
        , text " packages with a total of "
        , strong [] [ text (toString (List.length info.chunks)) ]
        , text " type definitions."
        ]
    , if not (List.isEmpty info.failed) then
        div
          []
          [ p [] [ text "The following packages did not load or parse," ]
          , ul
              []
              (List.map
                (\summary ->
                  li
                    []
                    [ a
                        [ href ("http://package.elm-lang.org/packages/" ++ summary.name)
                        , style [ ( "color", "#bbb" ) ]
                        ]
                        [ text summary.name ]
                    ]
                )
                info.failed
              )
          ]
      else
        text ""
    ]


viewSearchResults : Signal.Address Action -> Info -> Html
viewSearchResults addr ({ query, chunks } as info) =
  let
    queryType =
      Type.normalize (PDocs.stringToType query)

    filteredChunks =
      -- TODO: we should not need the two different cases, I guess.
      case queryType of
        Type.Var string ->
          chunks
            |> List.map (\chunk -> ( Entry.nameDistance query chunk.entry, chunk ))
            |> List.filter (\( distance, _ ) -> distance <= Type.lowPenalty)

        _ ->
          chunks
            |> List.map (\chunk -> ( Entry.typeDistance queryType chunk.entryNormalized, chunk ))
            |> List.filter (\( distance, _ ) -> distance <= Type.lowPenalty)
  in
    if List.length filteredChunks == 0 then
      div
        []
        [ p [] [ text "Your search did not yield any results. You can try one of the examples below." ]
        , exampleSearches addr
        ]
    else
      div [] (searchResultsChunks info filteredChunks)


searchResultsChunks : Info -> List ( comparable, Chunk ) -> List Html
searchResultsChunks { packageDict } weightedChunks =
  weightedChunks
    |> List.sortBy (\( distance, _ ) -> distance)
    |> List.map
        (\( distance, { package, name, entry } ) ->
          div
            []
            [ Entry.typeViewSearch package name (nameDict packageDict package) entry
            , div [ class "searchDebug" ] [ text (toString distance) ]
            ]
        )



-- MAKE CHUNKS


toChunks : PackageIdentifier -> Docs.Module -> List Chunk
toChunks pkgIdent moduleDocs =
  case String.split "\n@docs " moduleDocs.comment of
    [] ->
      []

    firstChunk :: rest ->
      List.concatMap (subChunks pkgIdent moduleDocs) rest


subChunks : PackageIdentifier -> Docs.Module -> String -> List Chunk
subChunks pkgIdent moduleDocs postDocs =
  catMaybes (subChunksHelp pkgIdent moduleDocs (String.split "," postDocs))


subChunksHelp : PackageIdentifier -> Docs.Module -> List String -> List (Maybe Chunk)
subChunksHelp pkgIdent moduleDocs parts =
  case parts of
    [] ->
      []

    rawPart :: remainingParts ->
      let
        part =
          String.trim rawPart
      in
        case PDocs.isValue part of
          Just valueName ->
            toMaybeChunk pkgIdent moduleDocs valueName
              :: subChunksHelp pkgIdent moduleDocs remainingParts

          Nothing ->
            let
              trimmedPart =
                String.trimLeft rawPart
            in
              case String.words trimmedPart of
                [] ->
                  []

                token :: _ ->
                  case PDocs.isValue token of
                    Just valueName ->
                      [ toMaybeChunk pkgIdent moduleDocs valueName ]

                    Nothing ->
                      []


toMaybeChunk : PackageIdentifier -> Docs.Module -> String -> Maybe Chunk
toMaybeChunk pkgIdent moduleDocs name =
  case Dict.get name moduleDocs.entries of
    Nothing ->
      Nothing

    Just e ->
      let
        entry =
          Entry.map PDocs.stringToType e

        entryNormalized =
          Entry.map Type.normalize entry
      in
        Just
          <| Chunk
              pkgIdent
              (Name.Canonical moduleDocs.name name)
              entry
              entryNormalized


nameDict : Packages -> PackageIdentifier -> Name.Dictionary
nameDict packageDict name =
  case Dict.get name packageDict of
    Just info ->
      .nameDict info

    Nothing ->
      Dict.empty


chunkPackage : Packages -> PackageIdentifier -> Docs.Package
chunkPackage packageDict name =
  case Dict.get name packageDict of
    Just info ->
      .package info

    Nothing ->
      Dict.empty


catMaybes : List (Maybe a) -> List a
catMaybes xs =
  case xs of
    [] ->
      []

    Nothing :: xs' ->
      catMaybes xs'

    (Just x) :: xs' ->
      x :: catMaybes xs'
