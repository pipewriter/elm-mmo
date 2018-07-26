module Main exposing (main)

import Html exposing (Html, text, div)
import Html.Attributes exposing (class)
import AnimationFrame
import Time exposing (Time)
import Collage exposing (Form, collage, filled, move, rect, toForm, groupTransform, toForm, group)
import Transform
import Element exposing (image)
import Color
import Char
import Keyboard exposing (KeyCode)
import WebSocket exposing (listen, send)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Element exposing (centered)
import Text exposing (fromString)
import Mouse exposing (moves, Position)
import List.Extra exposing (find)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }


url : String
url =
    "ws://localhost:8080"



-- Model


type alias Player =
    { x : Float
    , y : Float
    , name : String
    , id : Int
    }


type alias Model =
    { players : List Player
    , playerId : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { players = []
      , playerId = 0
      }
    , sendName "fred"
    )


sendName : String -> Cmd Msg
sendName string =
    WebSocket.send url ("{\"name\":\"" ++ string ++ "\"}")


sendDirection : ( Float, Float ) -> Cmd Msg
sendDirection ( x, y ) =
    WebSocket.send url
        ((Encode.object
            [ ( "uvx", Encode.float x )
            , ( "uvy", Encode.float y )
            ]
         )
            |> Encode.encode 0
        )



-- Update


type Msg
    = RecieveMessage Model
    | MouseMoved Position
    | NoOp


getPlayer : Model -> Maybe Player
getPlayer model =
    find (\player -> player.id == model.playerId) model.players


computeDirection : Position -> Player -> ( Float, Float )
computeDirection mousePosition player =
    let
        disx =
            (toFloat mousePosition.x) - player.x

        disy =
            (toFloat mousePosition.y) - player.y

        mag =
            sqrt (disx ^ 2 + disy ^ 2)

        uvx =
            disx / mag

        uvy =
            disy / mag
    in
        ( uvx, uvy )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecieveMessage newModel ->
            ( newModel, Cmd.none )

        MouseMoved position ->
            let
                cmd =
                    getPlayer model
                        |> Maybe.map (computeDirection position)
                        |> Maybe.map sendDirection
                        |> Maybe.withDefault Cmd.none
            in
                ( model, cmd )

        NoOp ->
            ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ Element.toHtml <|
            collage
                1000
                1000
                (background :: (List.map playerForm model.players))
        , div [ class "instructions" ]
            [ text "Use A and D to move, Space to jump"
            ]
        ]


background : Form
background =
    rect (toFloat 1000) (toFloat 1000)
        |> filled Color.blue


playerForm : Player -> Form
playerForm player =
    group
        [ (rect (toFloat 20) (toFloat 20)
            |> filled Color.red
          )
        , (fromString player.name
            |> centered
            |> toForm
            |> move ( 0.0, 20.0 )
          )
        ]
        |> move ( -(500 - player.x), 500 - player.y )



-- Subscriptions


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ listen url parseMessage
        , moves MouseMoved
        ]


playerDecoder : Decoder Player
playerDecoder =
    Decode.map4 Player
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "name" Decode.string)
        (Decode.field "id" Decode.int)


modelDecoder : Decoder Model
modelDecoder =
    Decode.map2 Model
        (Decode.field "players" (Decode.list playerDecoder))
        (Decode.field "playerId" Decode.int)


parseMessage : String -> Msg
parseMessage string =
    Decode.decodeString modelDecoder string
        |> Result.map RecieveMessage
        |> Result.withDefault NoOp
