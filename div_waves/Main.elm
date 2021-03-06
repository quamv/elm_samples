module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)
import AnimationFrame exposing (diffs)
import Json.Decode as Json
import Debug exposing (log)


type alias XYPoint = {
    x: Float
    ,y: Float
    }

type alias Wave = {
    radius: Float
    ,center: XYPoint
    ,wavelength: Float
    ,opacity: Float
    ,crests: Int
    }

type alias Model = {
    waves: List Wave
    ,run: Bool
    }

type Msg =
    Frame Time
    | ToggleRunState
    | ClickXY XYPoint


settings = {
    wavesettings = {
        maxopacity = 0.9
        ,maxborderwidth = 1
        ,minradius = 2
        ,growthRate = 0.4 --0.3
        ,attenuationFactor = 0.005 -- 0.001
        }
    }

main =
    Html.program {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
        }


init : (Model, Cmd Msg)
init =
    let
        minradius =
            settings.wavesettings.minradius
    in
    ({
        waves = [
                Wave minradius (XYPoint 100 100) 50 0.8 2
                ,Wave minradius (XYPoint 500 500) 30 0.8 3
                ]
            ,run = True
        }
        , Cmd.none
    )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Frame timediff ->
            nextAnimationFrame model

        ToggleRunState ->
            ({model | run = not model.run},Cmd.none)

        ClickXY xy ->
            ({model |
                waves = model.waves ++ [defaultWave xy.x xy.y]
                ,run = True
                }
             , Cmd.none)


view : Model -> Html Msg
view model =
    let
        mainContainerStyle = [
            ("width","800px")
            ,("margin","0 auto")
            ,("margin-top","20%")
            ]

        psuedoCanvasStyle = [
            ("height","600px")
            ,("position","relative")
            ,("background","seagreen")
            ,("overflow","hidden")
            ]

        waves =
            List.map waveView model.waves

        pauseBtnText =
            case model.run of
                True -> "Pause"
                False -> "Resume"
    in
    div [style mainContainerStyle] [
        div [style psuedoCanvasStyle, onMyClick ClickXY]
            waves
        ,button [onClick ToggleRunState, disabled (List.length model.waves == 0)]
            [text pauseBtnText]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.run of
        True ->
            Sub.batch [
                AnimationFrame.diffs Frame
                ]

        False ->
            Sub.none


-- when clicking in child div, incorporate offset of parent
childdivdecoder : Json.Decoder XYPoint
childdivdecoder =
    Json.map4
        xyfrom4ints
        (Json.field "offsetX" Json.int)
        (Json.at ["target","offsetLeft"] Json.int)
        (Json.field "offsetY" Json.int)
        (Json.at ["target","offsetTop"] Json.int)

parentdivdecoder : Json.Decoder XYPoint
parentdivdecoder =
    Json.map2
        xyfromints
        (Json.field "offsetX" Json.int)
        (Json.field "offsetY" Json.int)

xyfrom4ints : Int -> Int -> Int -> Int -> XYPoint
xyfrom4ints x1 x2 y1 y2=
    XYPoint (toFloat <| x1 + x2) (toFloat <| y1 + y2)

xyfromints : Int -> Int -> XYPoint
xyfromints x y =
    XYPoint (toFloat x) (toFloat y)

onMyClick : (XYPoint -> msg) -> Attribute msg
onMyClick tagger =
  on "click" (Json.map tagger parentdivdecoder)


waveView : Wave -> Html Msg
waveView wave =
    let
        maxopacity =
            settings.wavesettings.maxopacity

        maxborder =
            settings.wavesettings.maxborderwidth

        wavestyle = [
            --("border", (toString <| wave.opacity / maxopacity * maxborder) ++ "px solid azure")
            ("border", "1px solid azure")
            ,("opacity", (toString wave.opacity))
            ,("left", (toString <| wave.center.x - wave.radius) ++ "px")
            ,("top", (toString <| wave.center.y - wave.radius) ++ "px")
            ,("width", (toString <| wave.radius * 2) ++ "px")
            ,("height", (toString <| wave.radius * 2) ++ "px")
            ,("background","transparent")
            ,("border-radius","50%")
            ,("position","absolute")
            ]
    in
    div [   style wavestyle
            ,onWithOptions
                "click"
                { stopPropagation = True, preventDefault = True }
                (Json.map ClickXY childdivdecoder)
        ] []

defaultWave : Float -> Float -> Wave
defaultWave x y =
    Wave
        settings.wavesettings.minradius
        (XYPoint x y) -- wave.center
        50 -- wave.wavelength
        settings.wavesettings.maxopacity
        0 --(wave.crests - 1)


newCrest : Wave -> Wave
newCrest wave =
    Wave
        settings.wavesettings.minradius
        wave.center
        wave.wavelength
        settings.wavesettings.maxopacity
        (wave.crests - 1)


nextAnimationFrame : Model -> (Model, Cmd Msg)
nextAnimationFrame model =
    let
        updatedwavesfiltered =
            List.map updateWave model.waves
            |> List.filter (\n -> n.opacity > 0.05)

        newwaves =
            List.filter (\n -> n.crests > 0 && n.radius > n.wavelength) model.waves
            |> List.map newCrest

        allwaves =
            updatedwavesfiltered ++ newwaves

        newrunstate =
            case List.length allwaves == 0 of
                True ->  False
                False -> model.run
    in
        ({model |
            waves = allwaves
            ,run = newrunstate
            }, Cmd.none)


updateWave : Wave -> Wave
updateWave oldwave =
    let
        newwave =
            {oldwave |
                radius =
                    oldwave.radius + settings.wavesettings.growthRate

                ,opacity =
                    oldwave.opacity - settings.wavesettings.attenuationFactor

                ,crests =
                    case oldwave.radius > oldwave.wavelength of
                        True -> 0
                        False -> oldwave.crests
            }
    in
        newwave

