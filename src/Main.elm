module Main exposing (..)

import SvgDrawer exposing (interpretInstructions)
import CustomParser exposing (..)
import Browser
import Html exposing (Html, div, input, button, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput, onClick)
import Svg exposing (..)
import Parser exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (every, Posix)
import Platform.Cmd exposing (Cmd)


-- Modèle
type alias Model =
    { userInput : String
    , svgOutput : Svg Msg
    , remainingInstructions : List Instruction
    , currentInstructionIndex : Int
    , isDrawing : Bool
    , state : SvgDrawer.State  
    , timerActive : Bool
    , repeatStack : List (Int, List Instruction, List Instruction)  -- (count, subInstructions, continuation)
    }


-- Modèle initial
initialModel : Model
initialModel =
    { userInput = ""
    , svgOutput = SvgDrawer.stateToSvg SvgDrawer.initialState
    , remainingInstructions = []
    , currentInstructionIndex = 0
    , isDrawing = False
    , state = SvgDrawer.initialState
    , timerActive = False
    , repeatStack = []
    }


-- Message
type Msg
    = UpdateInput String
    | Submit
    | TimerTick Posix

interpretInstructionsList : String -> List Instruction
interpretInstructionsList input =
    case Parser.run instructionListParser input of
        Ok instructions -> instructions
        Err _ -> []  

treatInstruction : Instruction -> Model -> (Model, Cmd Msg)
treatInstruction instruction model =
    case instruction of
        -- Traitement des instructions simples
        Forward distance ->
            let newState = SvgDrawer.treatForward distance model.state
            in ({ model | state = newState, svgOutput = SvgDrawer.stateToSvg newState }, Cmd.none)

        Left angle ->
            let newState = SvgDrawer.treatLeft angle model.state
            in ({ model | state = newState, svgOutput = SvgDrawer.stateToSvg newState }, Cmd.none)

        Right angle ->
            let newState = SvgDrawer.treatRight angle model.state
            in ({ model | state = newState, svgOutput = SvgDrawer.stateToSvg newState }, Cmd.none)

        -- Traitement des instructions Repeat
        Repeat count subInstructions ->
            if count > 0 then
                let 
                    continuation = model.remainingInstructions
                    updatedRepeatStack = (count - 1, subInstructions, continuation) :: model.repeatStack
                in 
                ({ model 
                    | repeatStack = updatedRepeatStack
                    , remainingInstructions = subInstructions
                }, Cmd.none)
            else
                (model, Cmd.none)


getAt : Int -> List a -> Maybe a
getAt index list =
    List.head (List.drop index list)


-- Mise à jour
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateInput newInput ->
            ({ model | userInput = newInput }, Cmd.none)

        Submit ->
            let generatedInstructions = interpretInstructionsList model.userInput
            in ({ model | remainingInstructions = generatedInstructions, currentInstructionIndex = 0, state = SvgDrawer.initialState, isDrawing = True, timerActive = True, repeatStack = [] }, Cmd.none)

        TimerTick _ ->
            case model.remainingInstructions of
                [] ->
                    case model.repeatStack of
                        [] -> 
                            ({ model | timerActive = False }, Cmd.none)

                        (remainingCount, subInstructions, continuation) :: rest ->
                            if remainingCount > 0 then
                                ({ model
                                    | repeatStack = (remainingCount - 1, subInstructions, continuation) :: rest
                                    , remainingInstructions = subInstructions
                                }, Cmd.none)
                            else
                                ({ model 
                                    | repeatStack = rest
                                    , remainingInstructions = continuation
                                }, Cmd.none)

                instruction :: rest -> 
                    let (updatedModel, cmd) = treatInstruction instruction { model | remainingInstructions = rest }
                    in (updatedModel, cmd)


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.timerActive then
        Time.every 1     TimerTick 
    else
        Sub.none
        
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
    Browser.element
        { init = \_ -> (initialModel, Cmd.none)
        , update = update
        , view = view
        , subscriptions = subscriptions
        }