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
    , position ( 840, 465 ) [ image ( 230, 145 ) "assets/soundcloud.png" ]
    ]


cssProperties : List Content
cssProperties =
    [ Content.item
        (Html.code
            [ style
                [ ( "font", "37px/1.3 FiraCode, monospace" )
                , ( "margin", "0 5px" )
                , ( "display", "block" )
                ]
            ]
            [ text "color direction font font-display font-family font-feature-settings font-kerning font-language-override font-size font-size-adjust font-smoothing font-stretch font-style font-synthesis font-variant font-variant-alternates font-variant-caps font-variant-east-asian font-variant-ligatures font-variant-numeric font-variant-position font-variation-settings font-weight hanging-punctuation hyphens letter-spacing line-break line-height line-height-step overflow-wrap tab-size text-align text-align-last text-combine-upright text-decoration text-decoration-color text-decoration-line text-decoration-style text-indent text-justify text-orientation text-rendering text-shadow text-size-adjust text-transform text-underline-position unicode-bidi white-space word-break word-spacing word-wrap writing-mode" ]
        )
    ]


solvedProblem : List Content
solvedProblem =
    [ Content.item
        (Html.div
            [ style
                [ ( "font", "37px/1.3 FiraCode, monospace" )
                , ( "margin", "0 5px" )
                , ( "display", "block" )
                , ( "color", "#9e9c9d" )
                ]
            ]
            [ text "color direction font font-display font-family font-feature-settings font-kerning font-language-override font-size font-size-adjust font-smoothing font-stretch font-style font-synthesis font-variant font-variant-alternates font-variant-caps font-variant-east-asian font-variant-ligatures font-variant-numeric font-variant-position font-variation-settings font-weight hanging-punctuation hyphens letter-spacing line-break line-height line-height-step overflow-wrap tab-size text-align text-align-last text-combine-upright text-decoration text-decoration-color text-decoration-line text-decoration-style text-indent text-justify text-orientation text-rendering text-shadow text-size-adjust text-transform text-underline-position unicode-bidi white-space word-break word-spacing word-wrap writing-mode" ]
        )
    , position
        ( 250, 145 )
        [ imageLink ( 802, 430 ) "assets/solved-problems.png" "https://twitter.com/AbletonDev/status/902486487664615428" ]
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


mogeeFont : List Content
mogeeFont =
    [ title "MogeeFont"
    , scale 1.5
        [ code "elm" """text :
  (Letter -> List a -> List a)
  -> String
  -> List a

-- Base64 data: URI
fontSrc : String
"""
        ]
    , position ( 900, 200 )
        [ Custom.pixelfont
            { text = "ABCDEFGHIJ\nKLMNOPQRS\nTUVWXYZ\nabcdefghijk\nlmnopqrst\nuvwxyz"
            , pixelSize = 6
            , width = 300
            , height = 400
            }
        ]
    ]


mogeeFontUsage : List Content
mogeeFontUsage =
    [ title "MogeeFont"
    , split
        [ imageLink ( 350, 350 ) "assets/elm-mogee.png" "https://unsoundscapes.itch.io/mogee"
        , richtext """[elm-mogee](https://unsoundscapes.itch.io/mogee)"""
        ]
        [ imageLink ( 350, 350 ) "assets/elm-cubik.png" "https://unsoundscapes.itch.io/cubik"
        , richtext """[elm-cubik](https://unsoundscapes.itch.io/cubik)"""
        ]
    ]


bulletPoints : List String
bulletPoints =
    [ "Parse SVG path with elm-tools/parser"
    , "Convert Bézier curves to line segments using ianmackenzie/elm-geometry"
    , "Find outlines and holes based on winding"
    , "Triangulate outlines with holes using monotone polygon triangulation algorithm from ianmackenzie/elm-geometry"
    , "Generate a WebGL mesh"
    ]


steps : Int -> Content
steps n =
    bulletPoints
        |> List.take n
        |> List.map bullet
        |> Content.container
            (Html.ol
                [ style
                    [ ( "font", "20px/1.2 FiraCode, monospace" )
                    , ( "position", "absolute" )
                    , ( "left", "50%" )
                    , ( "top", "100px" )
                    , ( "margin", "0" )
                    , ( "right", "100px" )
                    ]
                ]
            )


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


thankYou : List Content
thankYou =
    [ position ( 0, 0 )
        [ Custom.zoom
            { width = 1280
            , height = 720
            , text = "That’s all. Thank you!"
            , fontSize = 195
            }
        ]
    ]


slides : List Slide
slides =
    [ [ padded intro ]
    , [ shout "Web Design is 95% Typography" ] -- https://ia.net/topics/the-web-is-all-about-typography-period
    , cssProperties
    , solvedProblem
    , [ padded exploringTheProblem ]
    , fontAsAProgram
    , [ padded mogeeFont ]
    , [ shout "MogeeFont usage slide" ]
    , [ padded mogeeFontUsage ]
    , [ shout "Font as data" ]
    , [ Custom.metrics { width = 1280, height = 720, fontSize = 500 } ]
    , [ background "assets/letterpress.jpg"
            [ position ( 990, 600 )
                [ imageLink ( 216, 68 )
                    "assets/miat.png"
                    "http://www.miat.gent.be/"
                ]
            ]
      ]
    , [ Custom.sort { width = 1280, height = 720 } ]
    , [ Custom.outlines { width = 1280, height = 720, step = 1 } ]
    , [ Custom.outlines { width = 1280, height = 720, step = 2 }, steps 1 ]
    , [ Custom.outlines { width = 1280, height = 720, step = 3 }, steps 2 ]
    , [ Custom.outlines { width = 1280, height = 720, step = 4 }, steps 3 ]
    , [ Custom.outlines { width = 1280, height = 720, step = 5 }, steps 4 ]
    , [ Custom.outlines { width = 1280, height = 720, step = 5 }, steps 5 ]
    , lineBreaking
    , thankYou
    ]
        -- make 16:9 slides
        |> List.map (Slide.slide >> Slide.setDimensions ( 1280, 720 ))
