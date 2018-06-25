module Custom.Zoom exposing (Model, Msg, initial, subscriptions, update, view)

import AnimationFrame
import Dict
import Font.Mesh as Mesh exposing (Attributes2d)
import Font.Text as Text exposing (GlyphInfo)
import Html exposing (Html)
import Html.Attributes as HtmlAttributes
import Iverni exposing (font)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader)


type Msg
    = Animate Time


type alias Model =
    { elapsed : Time
    , text : List (GlyphInfo Attributes2d)
    , width : Float
    , height : Float
    }


initial : { text : String, fontSize : Float, width : Float, height : Float } -> Model
initial options =
    { elapsed = 0
    , text = Tuple.first (Text.text2d (style options.fontSize) options.text)
    , width = options.width
    , height = options.height
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    AnimationFrame.diffs Animate


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        Animate elapsed ->
            ( { model | elapsed = model.elapsed + elapsed }
            , Cmd.none
            )


style : Float -> Text.Style a
style fontSize =
    { font = Iverni.font
    , fontSize = fontSize
    , lineHeight = 1.3
    , width = 10000 -- don't need line wrapping
    , features = [ Text.Liga, Text.Kern ]
    , cache = Dict.empty
    }


view : Model -> Html msg
view { elapsed, width, height, text } =
    let
        devicePixelRatio =
            2

        projection =
            Mat4.makeOrtho2D 0 width -height 0

        centerX =
            width / 2

        centerY =
            -height / 2

        wordCenterX =
            -- todo: measure word width
            centerX - 550

        wordCenterY =
            centerY + 80

        glyphToEntity { transform, mesh } =
            WebGL.entity
                vertex2d
                fragment2d
                mesh
                { projection = projection
                , color = vec3 1 0 0
                , center = vec3 (width * (0.5 + 0.5 * sin (elapsed / 2000))) (centerY - 20) 0
                , transform =
                    transform
                        |> Mat4.mul (Mat4.makeTranslate3 wordCenterX wordCenterY 0)
                }
    in
    WebGL.toHtml
        [ HtmlAttributes.width (round (width * devicePixelRatio))
        , HtmlAttributes.height (round (height * devicePixelRatio))
        , HtmlAttributes.style
            [ ( "display", "block" )
            , ( "width", toString width ++ "px" )
            , ( "height", toString height ++ "px" )
            ]
        ]
        (List.map glyphToEntity text)


type alias Uniforms2d =
    { projection : Mat4
    , transform : Mat4
    , color : Vec3
    , center : Vec3
    }


vertex2d : Shader Attributes2d Uniforms2d {}
vertex2d =
    [glsl|
        precision highp float;
        attribute vec3 position;
        uniform vec3 center;
        uniform mat4 projection;
        uniform mat4 transform;
        void main () {
            float maxdist = 100.0;
            vec4 p = transform * vec4(position, 1.0);
            vec4 c = vec4(center, 1.0);
            float dist = distance(p, c);

            if (dist > maxdist) {
                dist = maxdist;
            }

            float k = (1.0 - dist * dist / maxdist / maxdist);
            gl_Position = projection * (p + (p - c) * k);
        }
    |]


fragment2d : Shader {} Uniforms2d {}
fragment2d =
    [glsl|
        precision mediump float;
        uniform vec3 color;
        void main () {
            gl_FragColor = vec4(color, 1.0);
        }
    |]
