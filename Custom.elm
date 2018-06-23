module Custom
    exposing
        ( Content
        , Model
        , Msg
        , Slide
        , pixelfont
        , sort
        , subscriptions
        , typewriter
        , update
        , view
        , zoom
        )

import Custom.Pixelfont as Pixelfont
import Custom.Sort as Sort
import Custom.Typewriter as Typewriter
import Custom.Zoom as Zoom
import Html exposing (Html)
import SliceShow.Content as Content
import SliceShow.Slide as Slide


type alias Content =
    Content.Content Model Msg


type alias Slide =
    Slide.Slide Model Msg


type Model
    = SortModel Sort.Model
    | ZoomModel Zoom.Model
    | TypewriterModel Typewriter.Model
    | PixelfontModel Pixelfont.Model


type Msg
    = SortMsg Sort.Msg
    | ZoomMsg Zoom.Msg
    | TypewriterMsg Typewriter.Msg
    | PixelfontMsg Pixelfont.Msg


sort : { width : Float, height : Float } -> Content
sort size =
    Content.custom (SortModel (Sort.initial size))


zoom : { text : String, fontSize : Float, width : Float, height : Float } -> Content
zoom options =
    Content.custom (ZoomModel (Zoom.initial options))


typewriter : { text : String, fontSize : Float, width : Float, height : Float } -> Content
typewriter options =
    Content.custom (TypewriterModel (Typewriter.initial options))


pixelfont : { pixelSize : Int, text : String, width : Float, height : Float } -> Content
pixelfont options =
    Content.custom (PixelfontModel (Pixelfont.initial options))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        SortModel submodel ->
            Sub.map SortMsg (Sort.subscriptions submodel)

        ZoomModel submodel ->
            Sub.map ZoomMsg (Zoom.subscriptions submodel)

        TypewriterModel submodel ->
            Sub.map TypewriterMsg (Typewriter.subscriptions submodel)

        PixelfontModel submodel ->
            Sub.map PixelfontMsg (Pixelfont.subscriptions submodel)


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case ( action, model ) of
        ( SortMsg a, SortModel m ) ->
            let
                ( newModel, newCmd ) =
                    Sort.update a m
            in
            ( SortModel newModel, Cmd.map SortMsg newCmd )

        ( ZoomMsg a, ZoomModel m ) ->
            let
                ( newModel, newCmd ) =
                    Zoom.update a m
            in
            ( ZoomModel newModel, Cmd.map ZoomMsg newCmd )

        ( TypewriterMsg a, TypewriterModel m ) ->
            let
                ( newModel, newCmd ) =
                    Typewriter.update a m
            in
            ( TypewriterModel newModel, Cmd.map TypewriterMsg newCmd )

        ( PixelfontMsg a, PixelfontModel m ) ->
            let
                ( newModel, newCmd ) =
                    Pixelfont.update a m
            in
            ( PixelfontModel newModel, Cmd.map PixelfontMsg newCmd )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        SortModel submodel ->
            Html.map SortMsg (Sort.view submodel)

        ZoomModel submodel ->
            Html.map ZoomMsg (Zoom.view submodel)

        TypewriterModel submodel ->
            Html.map TypewriterMsg (Typewriter.view submodel)

        PixelfontModel submodel ->
            Html.map PixelfontMsg (Pixelfont.view submodel)
