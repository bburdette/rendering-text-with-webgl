module Custom.Outline exposing (Options, view)

import Font.Font as Font
import Font.Glyph as Glyph exposing (Glyph)
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
import Iverni exposing (font)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttributes


type alias Options =
    { fontSize : Float
    , text : String
    , width : Float
    , height : Float
    , left : Float
    , top : Float
    }


view : Options -> Html msg
view { width, height, left, top, fontSize, text } =
    text
        |> String.toList
        |> List.foldl
            (\char { advance, paths } ->
                let
                    k =
                        fontSize / font.unitsPerEm

                    glyph =
                        Font.getGlyph font char
                            |> Maybe.withDefault Glyph.empty
                in
                { advance = advance + glyph.advanceWidth * k
                , paths = renderGlyph advance (top + font.ascender * k) k glyph :: paths
                }
            )
            { advance = left, paths = [] }
        |> .paths
        |> Svg.svg
            [ HtmlAttributes.style [ ( "display", "block" ) ]
            , SvgAttributes.width (toString width)
            , SvgAttributes.height (toString height)
            ]


renderGlyph : Float -> Float -> Float -> Glyph -> Svg msg
renderGlyph x y k { path } =
    Svg.g
        [ SvgAttributes.transform
            ("translate("
                ++ toString x
                ++ ","
                ++ toString y
                ++ ") scale("
                ++ toString k
                ++ ","
                ++ toString -k
                ++ ")"
            )
        ]
        [ Svg.path
            [ SvgAttributes.d path
            , SvgAttributes.fill "transparent"
            , SvgAttributes.stroke "black"
            , SvgAttributes.strokeWidth (toString (2 / k))
            ]
            []
        ]
