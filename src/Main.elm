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
        { text = "blah"
        , fontSize = 12
        , lineHeight = 12
        , width = 30
        , color = vec3 50 100 150
        , height = 12
        , features = []
        }



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
