module Font.ParsePathCommand exposing (path)

import Font.PathCommand exposing (PathCommand(..))
import Parser exposing ((|.), (|=), Parser)


path : Parser (List (List PathCommand))
path =
    Parser.succeed identity
        |= Parser.repeat Parser.zeroOrMore commands
        |. Parser.end


commands : Parser (List PathCommand)
commands =
    Parser.succeed identity
        |= Parser.repeat Parser.zeroOrMore command
        |. close


command : Parser PathCommand
command =
    Parser.oneOf
        [ moveTo
        , lineTo
        , quadraticCurveTo
        , bezierCurveTo
        ]


moveTo : Parser PathCommand
moveTo =
    Parser.succeed MoveTo
        |. Parser.symbol "M"
        |= number
        |. spaces
        |= number
        |. spaces


lineTo : Parser PathCommand
lineTo =
    Parser.succeed LineTo
        |. Parser.symbol "L"
        |= number
        |. spaces
        |= number
        |. spaces


quadraticCurveTo : Parser PathCommand
quadraticCurveTo =
    Parser.succeed QuadraticCurveTo
        |. Parser.symbol "Q"
        |= number
        |. spaces
        |= number
        |. spaces
        |= number
        |. spaces
        |= number
        |. spaces


bezierCurveTo : Parser PathCommand
bezierCurveTo =
    Parser.succeed BezierCurveTo
        |. Parser.symbol "C"
        |= number
        |. spaces
        |= number
        |. spaces
        |= number
        |. spaces
        |= number
        |. spaces
        |= number
        |. spaces
        |= number
        |. spaces


close : Parser ()
close =
    Parser.succeed ()
        |. Parser.symbol "Z"
        |. spaces


number : Parser Float
number =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.float
        , Parser.float
        ]


spaces : Parser ()
spaces =
    Parser.ignore Parser.zeroOrMore (\char -> char == ' ')
