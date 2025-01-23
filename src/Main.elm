module Main exposing (..)

import Browser
import Html exposing (Html, div, input, button, text, svg)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput, onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)

-- Modèle
type alias Model =
    { userInput : String }

initialModel : Model
initialModel =
    { userInput = "" }

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
            model  -- Pour l'instant, on ne fait rien lors de la soumission

-- Vue
view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "Entrez du texte ici"
            , value model.userInput
            , onInput UpdateInput
            ]
            []
        , button
            [ onClick Submit ]
            [ text "Soumettre" ]
        , div []
            [ text ("Vous avez entré : " ++ model.userInput) ] 
        , svg
        [ width "200", height "200", viewBox "0 0 200 200" ]
        [ circle
            [ cx "100", cy "100", r "50", fill "red" ]
            []
        ]
        ]

-- Programme principal
main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }
