module Main exposing (Model, Msg(..), main, update, view)

import Browser as B
import Html exposing (Html)
import Ivernifont as I


type Msg
    = Noop


type alias Model =
    {}


view : Model -> Html Msg
view m =
    Html.text "blah"


update : Msg -> Model -> Model
update msg model =
    model


main =
    B.sandbox
        { init = {}
        , view = view
        , update = update
        }
