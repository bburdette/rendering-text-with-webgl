module Slides exposing (slides)

import Custom exposing (Content, Slide)
import Formatting exposing (..)
import Html exposing (h1, text)
import Html.Attributes exposing (style)
import SliceShow.Content as Content
import SliceShow.Slide as Slide


intro : List Content
intro =
    [ Content.item
        (h1
            [ style
                [ ( "font", "100px/1.2 FiraSans-Light, sans-serif" )
                , ( "letter-spacing", "-3px" )
                , ( "margin", "30px 0 150px" )
                ]
            ]
            [ text "Rendering Text with WebGL" ]
        )
    , position ( 120, 445 ) [ image ( 160, 160 ) "assets/mogee.png" ]
    , position ( 320, 435 )
        [ richtext """Andrey Kuzmin

Twitter: [@unsoundscapes](https://twitter.com/unsoundscapes)

GitHub: [@w0rm](https://github.com/w0rm)"""
        ]
    , position ( 840, 465 ) [ image ( 240, 140 ) "assets/soundcloud.png" ]
    ]


cssProperties : List Content
cssProperties =
    [ title "~50 Text CSS Properties"
    , richtext """`color direction font font-display font-family font-feature-settings font-kerning font-language-override font-size font-size-adjust font-smoothing font-stretch font-style font-synthesis font-variant font-variant-alternates font-variant-caps font-variant-east-asian font-variant-ligatures font-variant-numeric font-variant-position font-variation-settings font-weight hanging-punctuation hyphens letter-spacing line-break line-height line-height-step overflow-wrap tab-size text-align text-align-last text-combine-upright text-decoration text-decoration-color text-decoration-line text-decoration-style text-indent text-justify text-orientation text-rendering text-shadow text-size-adjust text-transform text-underline-position unicode-bidi white-space word-break word-spacing word-wrap writing-mode`"""
    ]


solvedProblem : List Content
solvedProblem =
    [ spacing 160
    , align Center [ imageLink ( 802, 430 ) "assets/solved-problems.png" "https://twitter.com/AbletonDev/status/902486487664615428" ]
    ]


exploringTheProblem : List Content
exploringTheProblem =
    [ title "Rendering Text in Elm WebGL"
    , bullets
        [ bullet "Elm is a great tool to explore the problem space"
        , bullet "WebGL in Elm is a fun way to dive into graphics programming"
        , bullet "Opens possibilities for creative coding"
        ]
    ]


toc : List Content
toc =
    [ title "Rendering Text in Elm WebGL"
    , bullets
        [ bullet "Font as a program"
        , bullet "Font as data"
        , bullet "Glyph metrics"
        , bullet "Type features"
        , bullet "Line breaking"
        ]
    ]


glyphMetrics : List Content
glyphMetrics =
    [ title "Sort"
    , position ( 100, 145 ) [ richtext "(typesetting)" ]
    , position ( 500, 0 ) [ Custom.sort { width = 720, height = 720 } ]
    ]


fontAsAProgram : List Content
fontAsAProgram =
    [ position ( 100, 80 )
        [ Custom.pixelfont
            { text = "Font as\na Program"
            , pixelSize = 25
            , width = 1180
            , height = 640
            }
        ]
    ]


openTypeFeatures : List Content
openTypeFeatures =
    [ position ( 0, 0 )
        [ Custom.zoom
            { width = 1280
            , height = 720
            , text = "OpenType Features"
            , fontSize = 210
            }
        ]
    ]


lineBreaking : List Content
lineBreaking =
    [ position ( 0, 0 )
        [ Custom.typewriter
            { width = 1280
            , height = 720
            , fontSize = 150
            , text = "Line breaking is the process of breaking a section of text into lines such that it will fit in the available display area."
            }
        ]
    ]


slides : List Slide
slides =
    [ [ padded intro ]
    , [ shout "Text is user interface" ]
    , [ padded cssProperties ]
    , solvedProblem
    , [ padded exploringTheProblem ]
    , [ padded toc ]
    , [ padded fontAsAProgram ]
    , [ shout "Font as data" ]
    , [ padded glyphMetrics ]
    , openTypeFeatures
    , lineBreaking
    , [ shout "Thank you!" ]
    ]
        -- make 16:9 slides
        |> List.map (Slide.slide >> Slide.setDimensions ( 1280, 720 ))
