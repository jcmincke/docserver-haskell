module Main exposing (..)

import Browser

import Bootstrap.Button as B
import Bootstrap.ButtonGroup as BG
import Bootstrap.Utilities.Spacing as BGS
import Bootstrap.Grid.Row as BGR
import Bootstrap.Grid.Col as BGC
import Bootstrap.Grid as BG
import Bootstrap.Utilities.Border as BGB

import Maybe exposing (Maybe (..), withDefault)
import Debug exposing (log, toString)
import Html exposing (Html, button, div, text, Attribute, input)
import Html.Events exposing (onClick, onCheck)
import Html exposing (text, h1, div, video, source, audio)
import Html.Attributes exposing (width, height, media, attribute,
                                 controls, src, type_, id)
import Html.Events exposing (on)
import Json.Encode exposing (Value)
import Json.Decode as D

import Dict as D
import List as L
import Browser.Navigation as BN
import Html exposing (Html, text, pre)

import Studio as S

import Api.Endpoint
import Api

-- import Page.Login as Login

-- MAIN



type alias Model =
  { entries : D.Dict String (List String)
  , package : Maybe String
  , version : Maybe String
  }

type Msg
  = GotEntities (Result String Entries)
  | SelectPackage String
  | SelectVersion String
  | GoToDoc String String


type alias Entries = D.Dict String (List String)


entriesDecoder : D.Decoder Entries
entriesDecoder =
  let versionDec = D.list D.string
      entryDec = D.map2 (\n vs -> (n, vs))
                            (D.field "name" D.string)
                            (D.field "versions" versionDec)
      entriesDec = D.list entryDec
      dec = entriesDec |> D.andThen (\l -> D.succeed (D.fromList l))
  in dec



init : () -> (Model, Cmd Msg)
init _ =
  ( { entries = D.empty
    , package = Nothing
    , version = Nothing
    }
  , Api.entries GotEntities entriesDecoder
  )

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


update msg model =
  case msg of
  GotEntities (Ok entities) ->  ( { model | entries = entities,
                                            package = Nothing,
                                            version = Nothing}
                                , Cmd.none )
  GotEntities (Err _) ->  ( model, Cmd.none )
  SelectPackage n -> ({model | package = Just n}, Cmd.none)
  SelectVersion v -> ({model | version = Just v}, Cmd.none)
  GoToDoc n v -> ( model, BN.load ("docs/" ++ n ++ "-" ++ v ++ "/html/index.html" ))



subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


view : Model -> Html Msg
view model =
  div []
  ( --button [onClick GoToDoc] [text "Click me"]
  viewPackageNames model
  ++ viewPackageVersions model
  )


viewPackageNames : Model -> List (Html Msg)
viewPackageNames model =
  let go key acc = Html.li [onClick (SelectPackage key)] [text key] :: acc
  in L.foldr go [] (D.keys model.entries)


viewPackageVersions : Model -> List (Html Msg)
viewPackageVersions model =
  case model.package of
  Just n ->
    let go v acc = Html.li [onClick (GoToDoc n v)] [text v] :: acc
    in L.foldr go [] (withDefault [] (D.get n model.entries))
  Nothing -> []









