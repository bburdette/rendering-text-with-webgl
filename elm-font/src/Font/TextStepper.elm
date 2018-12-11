{- Identical to Font.Text but with stepping
   functionality and exposes the context for visualization
-}


module Font.TextStepper exposing
    ( Context
    , GlyphInfo
    , Style
    , style
    , text
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Font.ClassifiedGlyph as ClassifiedGlyph exposing (Class(..), ClassifiedGlyph)
import Font.Feature exposing (Feature(..))
import Font.Font as Font exposing (Font)
import Font.Glyph as Glyph exposing (Glyph)
import Font.Gpos as Gpos
import Font.Ligatures as Ligatures


type alias InternalStyle glyph =
    { font : Font
    , fontSize : Float
    , lineHeight : Float
    , width : Float
    , step : Int
    , features : List Feature
    , cache : Dict Int glyph
    }


type Style glyph
    = Style (InternalStyle glyph)


type alias GlyphInfo glyph =
    { glyph : glyph
    , x : Float
    , y : Float
    , size : Float
    }


type alias Context glyph =
    { font : Font
    , size : Float
    , lineHeight : Float
    , width : Float
    , features : List Feature
    , step : Int
    , penX : Float
    , penY : Float
    , cache : Dict Int glyph
    , glyphs : List (GlyphInfo glyph)
    , xAtLastWordBreak : Float
    , nextIndices : List ClassifiedGlyph
    , currentWordIndices : List ClassifiedGlyph
    }


style :
    { font : Font
    , fontSize : Float
    , lineHeight : Float
    , width : Float
    , features : List Feature
    , step : Int
    }
    -> Style a
style { font, fontSize, step, lineHeight, width, features } =
    Style
        { font = font
        , step = step
        , fontSize = fontSize
        , lineHeight = lineHeight
        , width = width
        , features = features
        , cache = Dict.empty
        }


text :
    (Glyph -> glyph)
    -> Style glyph
    -> String
    -> Context glyph
text glyphFn (Style style_) string =
    string
        -- unicode string to classified glyph indices
        |> ClassifiedGlyph.fromString style_.font.cmap
        -- substitute ligatures
        |> (if List.member Liga style_.features then
                Ligatures.substitute style_.font.ligatures

            else
                identity
           )
        -- initialize the  context
        |> init style_
        -- process the layout
        |> process glyphFn


init : InternalStyle glyph -> List ClassifiedGlyph -> Context glyph
init { font, fontSize, step, features, lineHeight, width, cache } glyphIndices =
    { font = font
    , cache = cache
    , step = step
    , size = fontSize / font.unitsPerEm
    , features = features
    , lineHeight = lineHeight * font.unitsPerEm
    , width = width / fontSize * font.unitsPerEm
    , penX = 0
    , penY = -font.ascender
    , xAtLastWordBreak = 0
    , nextIndices = glyphIndices
    , currentWordIndices = []
    , glyphs = []
    }


process : (Glyph -> glyph) -> Context glyph -> Context glyph
process glyphFn ctx =
    case ( ctx.step, ctx.nextIndices ) of
        ( _, [] ) ->
            { ctx | currentWordIndices = [] }

        ( 0, _ ) ->
            ctx

        ( _, classifiedGlyph :: nextIndices ) ->
            let
                fontGlyph =
                    Array.get classifiedGlyph.index ctx.font.glyphs
                        |> Maybe.withDefault Glyph.empty

                ( glyph, cache ) =
                    case Dict.get classifiedGlyph.index ctx.cache of
                        Just glyph_ ->
                            ( glyph_, ctx.cache )

                        Nothing ->
                            let
                                glyph_ =
                                    glyphFn fontGlyph
                            in
                            ( glyph_
                            , Dict.insert classifiedGlyph.index glyph_ ctx.cache
                            )

                xAdvance =
                    List.head nextIndices
                        |> Maybe.andThen (Gpos.get ctx.features ctx.font.gpos classifiedGlyph)
                        |> Maybe.withDefault 0

                newX =
                    ctx.penX + fontGlyph.advanceWidth + xAdvance
            in
            case classifiedGlyph.class of
                Space ->
                    process glyphFn
                        { ctx
                            | cache = cache
                            , penX = newX
                            , nextIndices = nextIndices
                            , currentWordIndices = []
                            , xAtLastWordBreak = newX
                            , step = ctx.step - 1
                        }

                Newline ->
                    process glyphFn
                        { ctx
                            | cache = cache
                            , penX = 0
                            , penY = ctx.penY - ctx.lineHeight
                            , nextIndices = nextIndices
                            , currentWordIndices = []
                            , xAtLastWordBreak = 0
                            , step = ctx.step - 1
                        }

                Other ->
                    if newX > ctx.width && ctx.xAtLastWordBreak /= 0 then
                        process glyphFn
                            { ctx
                                | cache = cache
                                , penX = 0
                                , penY = ctx.penY - ctx.lineHeight
                                , glyphs = List.drop (List.length ctx.currentWordIndices) ctx.glyphs
                                , nextIndices = List.foldl (::) ctx.nextIndices ctx.currentWordIndices
                                , currentWordIndices = []
                                , xAtLastWordBreak = 0
                                , step = ctx.step - 1
                            }

                    else
                        process glyphFn
                            { ctx
                                | cache = cache
                                , penX = newX
                                , glyphs =
                                    { glyph = glyph
                                    , x = ctx.penX
                                    , y = ctx.penY
                                    , size = ctx.size
                                    }
                                        :: ctx.glyphs
                                , nextIndices = nextIndices
                                , currentWordIndices = classifiedGlyph :: ctx.currentWordIndices
                                , step = ctx.step - 1
                            }
