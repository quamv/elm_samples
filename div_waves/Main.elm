module Main exposing (..)

import Array exposing (..)
import Debug exposing (log)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Time exposing (..)
import Browser
import Browser.Events as BrowserEvents


type alias XYPoint =
    { x : Float
    , y : Float
    }


type alias Wave =
    { radius : Float
    , center : XYPoint
    , wavelength : Float
    , opacity : Float
    , crests : Int
    }


type alias Model =
    { waves : List Wave
    , run : Bool
    , crestsPerWave : Int
    }


type Msg
    = Frame Posix | 
    ToggleRunState
    | ClickXY XYPoint
    | NewCrestsPerWave String


settings : { wavesettings : { maxopacity : Float, maxborderwidth : number, minradius : number, growthRate : Float, attenuationFactor : Float } }
settings =
    { wavesettings =
        { maxopacity = 0.9
        , maxborderwidth = 1
        , minradius = 2
        , growthRate = 0.4 --0.3
        , attenuationFactor = 0.005 -- 0.001
        }
    }


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init :() -> (Model, Cmd Msg)
init _ =
    let
        minradius =
            settings.wavesettings.minradius
    in
    ( Model 
            [ Wave minradius (XYPoint 100 100) 50 0.8 2
            --, Wave minradius (XYPoint 500 500) 30 0.8 3
            ]
        True
        1
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame _ ->
             nextAnimationFrame model

        ToggleRunState ->
            ( { model | run = not model.run }, Cmd.none )

        ClickXY xy ->
            ( { model
                | waves = model.waves ++ [ defaultWave model xy.x xy.y ]
                , run = True
              }
            , Cmd.none
            )

        NewCrestsPerWave str ->
            let
                newCrestsPerWaveMaybe = String.toInt str

                newCrestsPerWave = 
                    case newCrestsPerWaveMaybe of
                        Just anInt -> anInt
                        Nothing -> model.crestsPerWave
            in
                ( { model
                    | waves = model.waves
                    , run = True
                    , crestsPerWave = newCrestsPerWave
                }
                , Cmd.none
                )



view : Model -> Html Msg
view model =
    let
        mainContainerStyle =
            [ style "width" "800px"
            , style "margin" "0 auto"
            , style "margin-top" "20%"
            ]

        psuedoCanvasStyle =
            [ style "height" "600px"
            , style "position" "relative"
            , style "background" "seagreen"
            , style "overflow" "hidden"
            ]

        combinedStyle =
            psuedoCanvasStyle ++ [ onMyClick ClickXY ]

        waves =
            List.map waveView (List.filter (\n -> n.crests > 0) model.waves)

        pauseBtnText =
            if model.run then "Pause" else "Resume"
    in
    div mainContainerStyle
        [
        div [] [
            input [ Html.Attributes.attribute "type" "number", value <| String.fromInt model.crestsPerWave, onInput NewCrestsPerWave ] []
        ] 
        ,div (psuedoCanvasStyle ++ [ onMyClick ClickXY ]) -- combinedStyle --, onMyClick ClickXY ]
            waves
        , button [ onClick ToggleRunState, disabled (List.length model.waves == 0) ]
            [ text pauseBtnText ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.run then Sub.batch [ BrowserEvents.onAnimationFrame posixToMsg ]
    else Sub.none

posixToMsg : Posix -> Msg
posixToMsg = Frame

-- when clicking in child div, incorporate offset of parent

childdivdecoder : Json.Decoder XYPoint
childdivdecoder =
    Json.map4
        xyfrom4ints
        (Json.field "offsetX" Json.int)
        (Json.at [ "target", "offsetLeft" ] Json.int)
        (Json.field "offsetY" Json.int)
        (Json.at [ "target", "offsetTop" ] Json.int)


parentdivdecoder : Json.Decoder XYPoint
parentdivdecoder =
    Json.map2
        xyfromints
        (Json.field "offsetX" Json.int)
        (Json.field "offsetY" Json.int)


xyfrom4ints : Int -> Int -> Int -> Int -> XYPoint
xyfrom4ints x1 x2 y1 y2 =
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
        -- maxopacity = settings.wavesettings.maxopacity
        -- maxborder = settings.wavesettings.maxborderwidth

        wavestyle : List (Attribute Msg)
        wavestyle =
            [ --("border", (String.fromFloat <| wave.opacity / maxopacity * maxborder) ++ "px solid azure"
              style "border" "1px solid azure" 
            , style "opacity" <| String.fromFloat wave.opacity
            , style "left" <| (String.fromFloat <| wave.center.x - wave.radius) ++ "px" 
            , style "top" <| (String.fromFloat <| wave.center.y - wave.radius) ++ "px" 
            , style "width" <| (String.fromFloat <| wave.radius * 2) ++ "px" 
            , style "height" <| (String.fromFloat <| wave.radius * 2) ++ "px" 
            , style "background" "transparent" 
            , style "border-radius" "50%" 
            , style "position" "absolute"
            ]

        combinedStyle : List (Attribute Msg)
        combinedStyle = wavestyle ++ [ on "click" (Json.map ClickXY parentdivdecoder) ]
    in
    div
        combinedStyle
        --wavestyle
        -- [ style wavestyle
        -- , onWithOptions
        --     "click"
        --     { stopPropagation = True, preventDefault = True }
        --     (Json.map ClickXY childdivdecoder)
        -- ]
        []


defaultWave : Model -> Float -> Float -> Wave
defaultWave model x y =
    Wave
        settings.wavesettings.minradius
        (XYPoint x y)
        -- wave.center
        50
        -- wave.wavelength
        settings.wavesettings.maxopacity
        model.crestsPerWave



--(wave.crests - 1)


newCrest : Wave -> Wave
newCrest wave =
    Wave
        settings.wavesettings.minradius
        wave.center
        wave.wavelength
        settings.wavesettings.maxopacity
        (wave.crests - 1)


nextAnimationFrame : Model -> ( Model, Cmd Msg )
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
            if List.length allwaves == 0 then False else model.run
    in
    ( { model
        | waves = allwaves
        , run = newrunstate
      }
    , Cmd.none
    )


updateWave : Wave -> Wave
updateWave oldwave =
    let
        newwave =
            { oldwave
                | radius =
                    oldwave.radius + settings.wavesettings.growthRate
                , opacity =
                    oldwave.opacity - settings.wavesettings.attenuationFactor
                , crests =
                    if oldwave.radius > oldwave.wavelength then 0 else oldwave.crests
            }
    in
    newwave
