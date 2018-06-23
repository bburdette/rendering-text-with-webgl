module Font.Font exposing (Font, Kerning(..), Ligature, decodeFont, empty, getCoverageIndex, getGlyph, getGlyphClass)

import Array.Hamt as Array exposing (Array)
import Char
import Dict exposing (Dict)
import Font.Glyph exposing (Glyph)
import Json.Decode as Decode


type alias Font =
    { glyphs : Array Glyph -- an array of glyphs
    , cmap : Dict Int Int -- a mapping of character codes to a glyph index
    , ligatures : List Ligature
    , kerning : List Kerning
    , ascender : Float
    , descender : Float
    , unitsPerEm : Float
    }


type Kerning
    = KerningPairPosFormat1
        { coverage : Coverage
        , pairSets : Array (List KerningPairPosFormat1Pair)
        }
    | KerningPairPosFormat2
        { coverage : Coverage
        , classDef1 : ClassDef
        , classDef2 : ClassDef
        , classRecords : Array (Array Float)
        }


type alias KerningPairPosFormat1Pair =
    { secondGlyph : Int
    , xAdvance : Float
    }


type ClassDef
    = ClassDefFormat1 { startGlyph : Int, classes : Array Int }
    | ClassDefFormat2 (List ClassDefRange)


type alias ClassDefRange =
    { start : Int
    , end : Int
    , classId : Int
    }


type alias Ligature =
    { sub : List Int
    , by : Int
    }


type Coverage
    = CoverageList (List Int) -- 1 a list of individual glyph indices in the glyph set.
    | CoverageRanges (List CoverageRangeRecord) -- 2 ranges of consecutive indices


type alias CoverageRangeRecord =
    { start : Int --first glyph ID in the range
    , end : Int -- last glyph ID in the range
    , index : Int -- coverage Index of first glyph ID in range
    }


getGlyphClass : ClassDef -> Int -> Int
getGlyphClass classDef glyphIndex =
    case classDef of
        ClassDefFormat1 { startGlyph, classes } ->
            Array.get (glyphIndex - startGlyph) classes
                |> Maybe.withDefault 0

        ClassDefFormat2 classDefRanges ->
            searchClassDefRangeHelp classDefRanges glyphIndex
                |> Maybe.withDefault 0


searchClassDefRangeHelp : List ClassDefRange -> Int -> Maybe Int
searchClassDefRangeHelp list glyphIndex =
    case list of
        [] ->
            Nothing

        range :: rest ->
            if range.start >= glyphIndex && range.end <= glyphIndex then
                Just range.classId

            else
                searchClassDefRangeHelp rest glyphIndex


getCoverageIndex : Coverage -> Int -> Maybe Int
getCoverageIndex coverage glyphIndex =
    case coverage of
        CoverageList list ->
            searchIndexHelp list glyphIndex 0

        CoverageRanges ranges ->
            searchRangeHelp ranges glyphIndex 0


searchIndexHelp : List Int -> Int -> Int -> Maybe Int
searchIndexHelp list glyphIndex currentIndex =
    case list of
        [] ->
            Nothing

        el :: rest ->
            if el == glyphIndex then
                Just currentIndex

            else
                searchIndexHelp rest glyphIndex (currentIndex + 1)


searchRangeHelp : List CoverageRangeRecord -> Int -> Int -> Maybe Int
searchRangeHelp list glyphIndex currentIndex =
    case list of
        [] ->
            Nothing

        range :: rest ->
            if range.start >= glyphIndex && range.end <= glyphIndex then
                Just (range.index + glyphIndex - range.start)

            else
                searchRangeHelp rest glyphIndex (currentIndex + 1)


getGlyph : Font -> Char -> Maybe Glyph
getGlyph { glyphs, cmap } char =
    Dict.get (Char.toCode char) cmap
        |> Maybe.andThen (\index -> Array.get index glyphs)


empty : Font
empty =
    { glyphs = Array.empty
    , cmap = Dict.empty
    , ligatures = []
    , kerning = []
    , ascender = 0
    , descender = 0
    , unitsPerEm = 0
    }


decodeFont : Decode.Decoder Font
decodeFont =
    Decode.map7 Font
        (Decode.map Array.fromList (Decode.field "glyphs" (Decode.list decodeGlyph)))
        (Decode.map (List.filterMap (\( a, index ) -> Maybe.map (\code -> ( code, index )) (String.toInt a |> Result.toMaybe)) >> Dict.fromList) (Decode.field "cmap" (Decode.keyValuePairs Decode.int)))
        (Decode.field "ligatures" (Decode.list decodeLigature))
        (Decode.field "kerning" (Decode.list decodeKerning))
        (Decode.field "ascender" Decode.float)
        (Decode.field "descender" Decode.float)
        (Decode.field "unitsPerEm" Decode.float)


decodeKerning : Decode.Decoder Kerning
decodeKerning =
    Decode.oneOf
        [ decodeKerningPairPosFormat2
        , decodeKerningPairPosFormat1
        ]


decodeKerningPairPosFormat1 : Decode.Decoder Kerning
decodeKerningPairPosFormat1 =
    Decode.map KerningPairPosFormat1
        (Decode.map2
            (\coverage pairSets ->
                { coverage = coverage
                , pairSets = Array.fromList pairSets
                }
            )
            (Decode.field "coverage" decodeCoverage)
            (Decode.field "pairSets" (Decode.list (Decode.list decodeKerningPairPosFormat1Pair)))
        )


decodeKerningPairPosFormat2 : Decode.Decoder Kerning
decodeKerningPairPosFormat2 =
    Decode.map4
        (\coverage classDef1 classDef2 classRecords ->
            KerningPairPosFormat2
                { coverage = coverage
                , classDef1 = classDef1
                , classDef2 = classDef2
                , classRecords = Array.fromList classRecords
                }
        )
        (Decode.field "coverage" decodeCoverage)
        (Decode.field "classDef1" decodeClassDef)
        (Decode.field "classDef2" decodeClassDef)
        (Decode.field "classRecords" (Decode.list (Decode.map Array.fromList (Decode.list decodeXAdvance))))


decodeXAdvance : Decode.Decoder Float
decodeXAdvance =
    Decode.at [ "value1", "xAdvance" ] Decode.float


decodeClassDef : Decode.Decoder ClassDef
decodeClassDef =
    Decode.oneOf
        [ Decode.map2
            (\startGlyph classes ->
                ClassDefFormat1
                    { startGlyph = startGlyph
                    , classes = Array.fromList classes
                    }
            )
            (Decode.field "startGlyph" Decode.int)
            (Decode.field "classes" (Decode.list Decode.int))
        , Decode.map ClassDefFormat2
            (Decode.field "ranges" (Decode.list decodeClassDefRange))
        ]


decodeClassDefRange : Decode.Decoder ClassDefRange
decodeClassDefRange =
    Decode.map3 ClassDefRange
        (Decode.field "start" Decode.int)
        (Decode.field "end" Decode.int)
        (Decode.field "classId" Decode.int)


decodeCoverage : Decode.Decoder Coverage
decodeCoverage =
    Decode.oneOf
        [ Decode.map CoverageList
            (Decode.field "glyphs" (Decode.list Decode.int))
        , Decode.map CoverageRanges
            (Decode.field "ranges" (Decode.list decodeCoverageRangeRecord))
        ]


decodeCoverageRangeRecord : Decode.Decoder CoverageRangeRecord
decodeCoverageRangeRecord =
    Decode.map3 CoverageRangeRecord
        (Decode.field "start" Decode.int)
        (Decode.field "end" Decode.int)
        (Decode.field "index" Decode.int)


decodeKerningPairPosFormat1Pair : Decode.Decoder KerningPairPosFormat1Pair
decodeKerningPairPosFormat1Pair =
    Decode.map2 KerningPairPosFormat1Pair
        (Decode.field "secondGlyph" Decode.int)
        decodeXAdvance


decodeLigature : Decode.Decoder Ligature
decodeLigature =
    Decode.map2 Ligature
        (Decode.field "sub" (Decode.list Decode.int))
        (Decode.field "by" Decode.int)


decodeGlyph : Decode.Decoder Glyph
decodeGlyph =
    Decode.map3 Glyph
        (Decode.field "path" Decode.string)
        (Decode.field "advanceWidth" Decode.float)
        (Decode.field "leftSideBearing" Decode.float)
