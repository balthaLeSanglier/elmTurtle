module SvgDrawer exposing (..)
import CustomParser exposing (..)
import Parser exposing (..)

type alias State =
    { position : (Float, Float)  -- Position actuelle de la tortue (x, y)
    , angle : Float              -- Direction actuelle en degrés
    , lines : List ((Float, Float), (Float, Float)) -- Lignes tracées
    }


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
treatLeft angle state =
    {state | angle = angle}

treatRight: Float -> State -> State
treatRight angle state =
    {state | angle = -angle}

treatRepeat : Int -> List Instruction -> State -> State
treatRepeat count instructions state =
    List.foldl (\_ accState -> treatInstructionList instructions accState) state (List.repeat count ())


degreesToRadians : Float -> Float
degreesToRadians degrees =
    degrees * pi / 180
