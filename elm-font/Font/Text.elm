module Font.Text exposing (Feature(..), GlyphInfo, Style, text)

import Array.Hamt as Array exposing (Array)
import Dict exposing (Dict)
import Font.ClassifiedGlyph as ClassifiedGlyph exposing (Class(..), ClassifiedGlyph)
import Font.Font as Font exposing (Font)
import Font.Glyph as Glyph exposing (Glyph)
import Font.Kerning as Kerning
import Font.Ligatures as Ligatures


type alias Style glyph =
    { font : Font
    , fontSize : Float
    , lineHeight : Float
    , width : Float
    , features : List Feature
    , cache : Dict Int glyph
    }


type Feature
    = Liga
    | Kern


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
    , kerning : Bool
    , penX : Float
    , penY : Float
    , cache : Dict Int glyph
    , glyphs : List (GlyphInfo glyph)
    , xAtLastWordBreak : Float
    , nextIndices : List ClassifiedGlyph
    , currentWordIndices : List ClassifiedGlyph
    }


type alias TextFunction glyph =
    Style glyph
    -> String
    -> ( List (GlyphInfo glyph), Dict Int glyph )


text : (Glyph -> glyph) -> TextFunction glyph
text glyphFn style string =
    string
        -- unicode string to classified glyph indices
        |> ClassifiedGlyph.fromString style.font.cmap
        -- substitute ligatures
        |> (if List.member Liga style.features then
                Ligatures.substitute style.font.ligatures

            else
                identity
           )
        -- initialize the  context
        |> init style
        -- process the layout
        |> process glyphFn
        -- extract the result
        |> end


init : Style glyph -> List ClassifiedGlyph -> Context glyph
init { font, fontSize, features, lineHeight, width, cache } glyphIndices =
    { font = font
    , cache = cache
    , size = fontSize / font.unitsPerEm
    , kerning = List.member Kern features
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
    case ctx.nextIndices of
        [] ->
            ctx

        classifiedGlyph :: nextIndices ->
            let
                fontGlyph =
                    Array.get classifiedGlyph.index ctx.font.glyphs
                        |> Maybe.withDefault Glyph.empty

                ( glyph, cache ) =
                    case Dict.get classifiedGlyph.index ctx.cache of
                        Just glyph ->
                            ( glyph, ctx.cache )

                        Nothing ->
                            let
                                glyph =
                                    glyphFn fontGlyph
                            in
                            ( glyph
                            , Dict.insert classifiedGlyph.index glyph ctx.cache
                            )

                xAdvance =
                    if ctx.kerning then
                        List.head nextIndices
                            |> Maybe.map .index
                            |> Maybe.andThen (Kerning.get ctx.font.kerning classifiedGlyph.index)
                            |> Maybe.withDefault 0

                    else
                        0

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
                            }


end : Context glyph -> ( List (GlyphInfo glyph), Dict Int glyph )
end context =
    ( List.reverse context.glyphs
    , context.cache
    )
