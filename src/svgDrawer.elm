module SvgDrawer exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import CustomParser exposing (..)
import Parser exposing (..)

type alias State =
    { position : (Float, Float)  -- Position actuelle de la tortue (x, y)
    , angle : Float              -- Direction actuelle en degrés
    , lines : List ((Float, Float), (Float, Float)) -- Lignes tracées
interpretInstructions : String -> Svg msg
interpretInstructions input =
    let
        listInstruction = Parser.run instructionListParser input
    in
        case listInstruction of
        Ok instructions ->
            let
                finalState = treatInstructionList instructions initialState
            in
            stateToSvg finalState

        Err _ ->
            svg [ width "500", height "500", viewBox "-250 -250 500 500" ]
                [ text_
                    [ x "-200"
                    , y "0"
                    , fill "red"
                    , fontSize "20"
                    ]
                    [ Svg.text "Parsing Error: Invalid input" ]
                ]

stateToSvg : State -> Svg msg
stateToSvg state =
    svg [ width "500", height "500", viewBox "-250 -250 500 500" ]
    (List.map lineToSvg state.lines)

lineToSvg : ((Float, Float), (Float, Float)) -> Svg msg
lineToSvg ((x1, y1), (x2, y2)) =
    line
        [ Svg.Attributes.x1 (String.fromFloat x1)
        , Svg.Attributes.y1 (String.fromFloat y1)
        , Svg.Attributes.x2 (String.fromFloat x2)
        , Svg.Attributes.y2 (String.fromFloat y2)
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "2"
        ]
        []
    }

interpretInstructions : String -> Svg msg
interpretInstructions input =
treatLeft newAngle state =
    {state | angle = state.angle - newAngle}
    in
        case listInstruction of
treatRight newAngle state =
    {state | angle = state.angle + newAngle}
                finalState = treatInstructionList instructions initialState
            in
            stateToSvg finalState

        Err _ ->
            svg [ width "500", height "500", viewBox "-250 -250 500 500" ]
                [ text_
                    [ x "-200"
                    , y "0"
                    , fill "red"
                    , fontSize "20"
                    ]
                    [ Svg.text "Parsing Error: Invalid input" ]
                ]

stateToSvg : State -> Svg msg
stateToSvg state =
    svg [ width "500", height "500", viewBox "-250 -250 500 500" ]
    (List.map lineToSvg state.lines)

lineToSvg : ((Float, Float), (Float, Float)) -> Svg msg
lineToSvg ((x1, y1), (x2, y2)) =
    line
        [ Svg.Attributes.x1 (String.fromFloat x1)
        , Svg.Attributes.y1 (String.fromFloat y1)
        , Svg.Attributes.x2 (String.fromFloat x2)
        , Svg.Attributes.y2 (String.fromFloat y2)
        , Svg.Attributes.stroke "black"
        , Svg.Attributes.strokeWidth "2"
        ]
        []

initialState : State
initialState =
    { position = (0, 0)
    , angle = 0
    , lines = []
    }

treatInstructionList : List Instruction -> State -> State
treatInstructionList instructions state =
    List.foldl
        convertInstructionToLine
        state
        instructions
    


convertInstructionToLine: Instruction -> State -> State
convertInstructionToLine instruction state =
    case instruction of
        Forward distance -> 
            treatForward distance state
        Left angle ->
            treatLeft angle state
        Right angle ->
            treatRight angle state
        Repeat count subInstructions  ->
            treatRepeat count subInstructions state
        
            

treatForward: Float -> State -> State
treatForward distance state =
    let
        (x, y) = state.position
        rad = degreesToRadians state.angle
        x2 = x + distance * cos rad
        y2 = y + distance * sin rad
    in
    { state
        | position = (x2, y2)
        , lines = ((x, y), (x2, y2)) :: state.lines
    }

treatLeft: Float -> State -> State
treatLeft newAngle state =
    {state | angle = state.angle - newAngle}

treatRight: Float -> State -> State
treatRight newAngle state =
    {state | angle = state.angle + newAngle}

treatRepeat : Int -> List Instruction -> State -> State
treatRepeat count instructions state =
    List.foldl (\_ accState -> treatInstructionList instructions accState) state (List.repeat count ())


degreesToRadians : Float -> Float
degreesToRadians degrees =
    degrees * pi / 180