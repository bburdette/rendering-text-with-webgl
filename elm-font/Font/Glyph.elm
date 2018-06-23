module Font.Glyph exposing (Glyph, empty)

-- More about metrics: https://www.freetype.org/freetype2/docs/glyphs/glyphs-3.html


type alias Glyph =
    { path : String -- Path in SVG path format (only supports M,C,L & Z)
    , advanceWidth : Float -- The distance between two successive pen positions
    , leftSideBearing : Float -- The horizontal distance from the current pen position to the glyph's left bbox edge
    }


empty : Glyph
empty =
    { path = ""
    , advanceWidth = 0
    , leftSideBearing = 0
    }
