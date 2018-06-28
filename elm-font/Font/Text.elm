module Font.Text exposing (Feature(..), GlyphInfo, Style, text2d, text3d)

import Array.Hamt as Array exposing (Array)
import Char
import Dict exposing (Dict)
import Font.Font as Font exposing (Font, Kerning(..), Ligature)
import Font.Glyph as Glyph exposing (Glyph)
import Font.Mesh as Mesh exposing (Attributes2d, Attributes3d, mesh2d, mesh3d)
import Font.ParsePathCommand as ParsePathCommand
import Font.PathCommand as PathCommand
import Math.Matrix4 as Mat4 exposing (Mat4)
import Parser
import Point2d exposing (Point2d)
import WebGL exposing (Entity, Mesh)


type Feature
    = Liga
    | Kern


type Class
    = Space
    | Newline
    | Other


type alias ClassifiedGlyph =
    { class : Class
    , index : Int
    }


classify : Char -> Class
classify char =
    case char of
        ' ' ->
            Space

        '\t' ->
            Space

        '\n' ->
            Newline |> Debug.log ""

        _ ->
            Other


type alias Context attributes =
    { font : Font
    , size : Float
    , lineHeight : Float
    , width : Float
    , kerning : Bool
    , x : Float
    , y : Float
    , cache : Dict Int (Mesh attributes)
    , glyphs : List (GlyphInfo attributes)
    , xAtLastWordBreak : Float
    , nextIndices : List ClassifiedGlyph
    , prevIndices : List ClassifiedGlyph
    }


type alias Style attributes =
    { font : Font
    , fontSize : Float
    , lineHeight : Float
    , width : Float
    , features : List Feature
    , cache : Dict Int (Mesh attributes)
    }


type alias GlyphInfo attributes =
    { mesh : Mesh attributes
    , transform : Mat4
    }


contextFromStyle : Style attributes -> List ClassifiedGlyph -> Context attributes
contextFromStyle style glyphIndices =
    { font = style.font
    , size = style.fontSize / style.font.unitsPerEm
    , kerning = List.member Kern style.features
    , lineHeight = style.lineHeight * style.font.unitsPerEm
    , width = style.width / style.fontSize * style.font.unitsPerEm
    , x = 0
    , y = -style.font.ascender
    , cache = style.cache
    , glyphs = []
    , xAtLastWordBreak = 0
    , nextIndices = glyphIndices
    , prevIndices = []
    }


stringToGlyphIndices : Font -> String -> List ClassifiedGlyph
stringToGlyphIndices font text =
    List.map
        (\char ->
            Dict.get (Char.toCode char) font.cmap
                |> Maybe.withDefault 0
                -- TODO: use "glyph not found" glyph but not for new line?
                |> ClassifiedGlyph (classify char)
        )
        (String.toList text)


replaceLigaturesHelp : Font -> List ClassifiedGlyph -> List ClassifiedGlyph -> List ClassifiedGlyph
replaceLigaturesHelp font text result =
    case text of
        [] ->
            List.reverse result

        glyph :: rest ->
            case findLigature font.ligatures text of
                Just ligature ->
                    replaceLigaturesHelp
                        font
                        (List.drop (List.length ligature.sub) text)
                        ({ index = ligature.by, class = Other } :: result)

                Nothing ->
                    replaceLigaturesHelp
                        font
                        rest
                        (glyph :: result)


findLigature : List Ligature -> List ClassifiedGlyph -> Maybe Ligature
findLigature ligatures text =
    case ligatures of
        ligature :: rest ->
            let
                ligatureSize =
                    List.length ligature.sub

                takeFromText =
                    List.take ligatureSize text
            in
            if List.map .index takeFromText == ligature.sub then
                Just ligature

            else
                findLigature rest text

        [] ->
            Nothing


getKerningValue : List Kerning -> Int -> Int -> Maybe Float
getKerningValue kerning leftIndex rightIndex =
    case kerning of
        (KerningPairPosFormat1 { coverage, pairSets }) :: rest ->
            case
                Font.getCoverageIndex coverage leftIndex
                    |> Maybe.andThen (\covIndex -> Array.get covIndex pairSets)
                    |> Maybe.andThen (\pairsPositions -> findInPairsPositions pairsPositions rightIndex)
            of
                Just width ->
                    Just width

                Nothing ->
                    getKerningValue rest leftIndex rightIndex

        (KerningPairPosFormat2 { coverage, classDef1, classDef2, classRecords }) :: rest ->
            if Font.getCoverageIndex coverage leftIndex == Nothing then
                getKerningValue rest leftIndex rightIndex

            else
                let
                    leftClass =
                        Font.getGlyphClass classDef1 leftIndex

                    rightClass =
                        Font.getGlyphClass classDef2 rightIndex
                in
                classRecords
                    |> Array.get leftClass
                    |> Maybe.andThen (Array.get rightClass)

        [] ->
            Nothing


findInPairsPositions : List { secondGlyph : Int, xAdvance : Float } -> Int -> Maybe Float
findInPairsPositions pairsPositions rightIndex =
    case pairsPositions of
        { secondGlyph, xAdvance } :: morePairs ->
            if secondGlyph == rightIndex then
                Just xAdvance

            else
                findInPairsPositions morePairs rightIndex

        [] ->
            Nothing


type alias TextFunction attributes =
    Style attributes
    -> String
    -> ( List (GlyphInfo attributes), Dict Int (Mesh attributes) )


text2d : TextFunction Attributes2d
text2d =
    text mesh2d


text3d : TextFunction Attributes3d
text3d =
    text mesh3d


text : (List (List Point2d) -> Mesh attributes) -> TextFunction attributes
text meshFn style string =
    let
        indicesList =
            stringToGlyphIndices style.font string

        indicesArray =
            if List.member Liga style.features then
                replaceLigaturesHelp style.font indicesList []

            else
                indicesList

        result =
            textHelp meshFn (contextFromStyle style indicesArray)
    in
    ( List.reverse result.glyphs, result.cache )


textHelp : (List (List Point2d) -> Mesh attributes) -> Context attributes -> Context attributes
textHelp meshFn ctx =
    case ctx.nextIndices of
        [] ->
            ctx

        classifiedGlyph :: nextIndices ->
            let
                xAdvance =
                    if ctx.kerning then
                        List.head nextIndices
                            |> Maybe.map .index
                            |> Maybe.andThen (getKerningValue ctx.font.kerning classifiedGlyph.index)
                            |> Maybe.withDefault 0

                    else
                        0

                glyph =
                    Array.get classifiedGlyph.index ctx.font.glyphs
                        |> Maybe.withDefault Glyph.empty

                cached =
                    Dict.get classifiedGlyph.index ctx.cache

                mesh =
                    case cached of
                        Just mesh ->
                            mesh

                        Nothing ->
                            glyph.path
                                |> Parser.run ParsePathCommand.path
                                |> Result.withDefault []
                                |> List.map (PathCommand.pathToPolygon 10)
                                |> meshFn

                cache =
                    case cached of
                        Nothing ->
                            Dict.insert classifiedGlyph.index mesh ctx.cache

                        _ ->
                            ctx.cache

                newX =
                    ctx.x + glyph.advanceWidth + xAdvance
            in
            case classifiedGlyph.class of
                Space ->
                    textHelp meshFn
                        { ctx
                            | cache = cache
                            , x = newX
                            , nextIndices = nextIndices
                            , prevIndices = []
                            , xAtLastWordBreak = newX
                        }

                Newline ->
                    textHelp meshFn
                        { ctx
                            | cache = cache
                            , x = 0
                            , y = ctx.y - ctx.lineHeight
                            , nextIndices = nextIndices
                            , prevIndices = []
                            , xAtLastWordBreak = 0
                        }

                Other ->
                    if newX > ctx.width && ctx.xAtLastWordBreak /= 0 then
                        textHelp meshFn
                            { ctx
                                | cache = cache
                                , x = 0
                                , y = ctx.y - ctx.lineHeight
                                , glyphs = List.drop (List.length ctx.prevIndices) ctx.glyphs
                                , nextIndices = List.foldl (::) ctx.nextIndices ctx.prevIndices
                                , prevIndices = []
                                , xAtLastWordBreak = 0
                            }

                    else
                        textHelp meshFn
                            { ctx
                                | cache = cache
                                , x = newX
                                , glyphs =
                                    { mesh = mesh
                                    , transform =
                                        Mat4.makeTranslate3 ctx.x ctx.y 0
                                            |> Mat4.mul (Mat4.makeScale3 ctx.size ctx.size ctx.size)
                                    }
                                        :: ctx.glyphs
                                , nextIndices = nextIndices
                                , prevIndices = classifiedGlyph :: ctx.prevIndices
                            }
