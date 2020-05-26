port module Main exposing (..)

import Browser

import Bootstrap.Button as B
import Bootstrap.ButtonGroup as BG
import Bootstrap.Utilities.Spacing as BGS
import Bootstrap.Grid.Row as BGR
import Bootstrap.Grid.Col as BGC
import Bootstrap.Grid as BG
import Bootstrap.Utilities.Border as BGB

import Html exposing (..)

import Maybe exposing (Maybe (..), withDefault)
import Debug exposing (log, toString)
import Html.Attributes exposing (..)

--import Html exposing (Html, button, div, text, Attribute, input)
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

import Bootstrap.Button as BB
import Bootstrap.ButtonGroup as BBG
import Bootstrap.Utilities.Spacing as BUS
import Bootstrap.Grid.Row as BGR
import Bootstrap.Grid.Col as BGC
import Bootstrap.Grid as BG
import Bootstrap.Utilities.Border as BUB
import Bootstrap.Form as BF
import Bootstrap.Pagination.Item as BPI
import Bootstrap.Form.InputGroup as BFIG
import Bootstrap.Form.Input as BFI
import Bootstrap.Form.Checkbox as BCB
import Bootstrap.ListGroup as BLG

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
  GoToDoc n v -> ( model, openDoc ("docs/" ++ n ++ "-" ++ v ++ "/index.html" ))
--  GoToDoc n v -> ( model, BN.load ("docs/" ++ n ++ "-" ++ v ++ "/html/index.html" ))




subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


view : Model -> Html Msg
view model =
  let headerHtml = viewHeader model
      packageHtml = viewPackageNames model
      versionHtml = viewPackageVersions model
      contHtml = BG.container []
        [ BG.row []
          [ BG.col [BGC.lg1] []
          , BG.col [BGC.lg4] [packageHtml]
          , BG.col [BGC.lg1] []
          , BG.col [BGC.lg4] [versionHtml]
          , BG.col [BGC.lg1] []
          ]
        ]
  in  div []
      [ headerHtml
      , contHtml
      ]



{-
        <div class="container">
            <div class="row">
                <div class="col-md-6 col-lg-3">
                    <ul class="list-group">
                        <li class="list-group-item"><span>List Group Item 1</span></li>
                        <li class="list-group-item"><span>List Group Item 2</span></li>
                        <li class="list-group-item"><span>List Group Item 3</span></li>
                    </ul>
                </div>
                <div class="col-md-6 col-lg-4">
                    <ul class="list-group">
                        <li class="list-group-item"><span>List Group Item 1</span></li>
                        <li class="list-group-item"><span>List Group Item 2</span></li>
                        <li class="list-group-item"><span>List Group Item 3</span></li>
                    </ul>
                </div>
            </div>
        </div>

  -}

viewPackageNames : Model -> Html Msg
viewPackageNames model =
  let go key acc = BLG.li [BLG.attrs [onClick (SelectPackage key)]] [text key] :: acc
      eHtml =  L.foldr go [] (D.keys model.entries)
  in  BLG.ul eHtml


viewPackageVersions : Model -> Html Msg
viewPackageVersions model =
  case model.package of
  Just n ->
    let go v acc = BLG.li [BLG.attrs [onClick (GoToDoc n v)]] [text v] :: acc
        eHtml =  L.foldr go [] (withDefault [] (D.get n model.entries))
    in BLG.ul eHtml
  Nothing -> BLG.ul []


viewHeader model =
  div [class "d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-5 bg-white border-bottom shadow-sm"]
  [ h3 [class "my-0 mr-md-auto font-weight-normal"]
    [ text "DocServer"]

  ]

{-

        <div class="container">
            <div class="row">
                <div class="col-md-6 col-lg-3">
                    <ul class="list-group">
                        <li class="list-group-item"><span>List Group Item 1</span></li>
                        <li class="list-group-item"><span>List Group Item 2</span></li>
                        <li class="list-group-item"><span>List Group Item 3</span></li>
                    </ul>
                </div>
                <div class="col-md-6 col-lg-4">
                    <ul class="list-group">
                        <li class="list-group-item"><span>List Group Item 1</span></li>
                        <li class="list-group-item"><span>List Group Item 2</span></li>
                        <li class="list-group-item"><span>List Group Item 3</span></li>
                    </ul>
                </div>
            </div>
        </div>

-}

port openDoc : String -> Cmd msg




