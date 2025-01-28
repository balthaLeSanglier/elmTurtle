module Main exposing (..)

import SvgDrawer exposing (interpretInstructions)
import CustomParser exposing (..)
import Browser
import Html exposing (Html, div, input, button, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput, onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
 

-- Modèle
type alias Model =
    { userInput : String,
        svgOutput : Svg Msg }

initialModel : Model
initialModel =
    { userInput = "",
      svgOutput = svg
        [ width "500", height "500", viewBox "0 0 500 500" ]
        [ Svg.line [ x1 "0", y1 "0", x2 "0", y2 "0", stroke "black", strokeWidth "2" ] []
        ]}

-- Messages
type Msg
    = UpdateInput String
    | Submit

-- Mise à jour
update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInput newInput ->
            { model | userInput = newInput }
        Submit ->
            let
                generatedSvg = interpretInstructions model.userInput
            in
            { model | svgOutput = generatedSvg }

-- Vue
view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "card" ]
            [ div [ class "title" ] [Html.text "TcTurtle Interpreter"  ]
            , input
                [ placeholder "Enter instructions (e.g., [Forward 100, Left 90])"
                , value model.userInput
                , onInput UpdateInput
                , class "input"
                ]
                []
            , button
                [ onClick Submit
                , class "button"
                ]
                [ Html.text "Generate SVG" ]
            ]
        , div [ class "svg-container" ]
            [model.svgOutput]
        ]

-- Programme principal
main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }
