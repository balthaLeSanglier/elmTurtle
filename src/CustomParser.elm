module CustomParser exposing (..)

import Parser exposing (..)

-- Définition des instructions TcTurtle
type Instruction
    = Repeat Int (List Instruction)
    | Forward Float
    | Left Float
    | Right Float 

-- Parse un float
floatParser : Parser Float
floatParser =
    Parser.float

intParser : Parser Int
intParser =
    Parser.int

-- Parse une instruction "Forward"
forwardParser : Parser Instruction
forwardParser =
    succeed Forward
        |. symbol "Forward"
        |. spaces
        |= floatParser

-- Parse une instruction "Left"
leftParser : Parser Instruction
leftParser =
    succeed Left
        |. symbol "Left"
        |. spaces
        |= floatParser

-- Parse une instruction "Right"
rightParser : Parser Instruction
rightParser =
    succeed Right
        |. symbol "Right"
        |. spaces
        |= floatParser

-- Parse une instruction "Repeat"
repeatParser : Parser Instruction
repeatParser =
    succeed Repeat
        |. symbol "Repeat"
        |. spaces
        |= intParser
        |. spaces
        |= lazy (\_ -> instructionListParser)

-- Parse une instruction
instructionParser : Parser Instruction
instructionParser =
    oneOf [ forwardParser, leftParser, rightParser, repeatParser ]

instructionListParser : Parser (List Instruction)
instructionListParser =
    Parser.sequence {
        start = "["
        , separator = ","
        , end = "]"
        , spaces = spaces
        , item = instructionParser
        , trailing = Optional
    }

