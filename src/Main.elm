module Main exposing (Model, Msg(..), main, update, view)

import Browser as B
import Html exposing (Html)
import Ivernifont as I
import Math.Vector3 as Vec3 exposing (Vec3, vec3)


type Msg
    = Noop


type alias Model =
    {}


view : Model -> Html Msg
view m =
    I.view
        { features = []
        , fontSize = 115
        , text = "ABCDEFGHIJ KLMNOPQR STUVWXYZab cdefghijklmno pqrstuvwxyz 0123456789"
        , lineHeight = 1
        , width = 430
        , height = 705
        , color = vec3 0.5 0.5 0.5
        }



{- text = "blah"
   , fontSize = 12
   , lineHeight = 12
   , width = 30
   , color = vec3 0.5 0.5 0.5
   , height = 12
   , features = []
-}
--    Html.text "blah"


update : Msg -> Model -> Model
update msg model =
    model


main =
    B.sandbox
        { init = {}
        , view = view
        , update = update
        }
