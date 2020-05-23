module Main exposing (..)

import Browser
import Html exposing (Html, div, fieldset, h1, input, label, text)
import Html.Attributes exposing (checked, style, type_)
import Html.Events exposing (onClick)
import HtmlRenderer exposing (renderAllCardsAsImages, renderHtml)
import List.Extra
import Messages exposing (..)
import Model exposing (..)
import Random
import Random.List
import SvgRenderer exposing (..)



{-
   - a game has
     - 10 game slots
       - 4 slots initialized with 6 cards (24)
       - 6 slots initialized with 5 cards (30)
     - 5 sets à 10 cards to draw in the draw-card slot (50)
     - 1 empty slot where completed sets are being placed
-}


init : ( Model, Cmd Msg )
init =
    ( UninitializedGame, Random.generate InitializedGame (Random.map initializeGame (Random.List.shuffle calcGameCards)) )



--foldl : (a → b → b) → b → List a → b
--foldl func acc list defined in List


chunkList : List a -> List Int -> List (List a)
chunkList items chunkSizes =
    let
        helper : List a -> List Int -> List (List a) -> List (List a)
        helper rest restChunks result =
            case restChunks of
                [] ->
                    List.reverse result

                chunkSize :: otherChunks ->
                    helper (List.drop chunkSize rest) otherChunks (List.take chunkSize rest :: result)
    in
    helper items chunkSizes []


calcGameCards : List Card
calcGameCards =
    List.append calcDeck calcDeck


faceUpLastCard : List Card -> List Card
faceUpLastCard cards =
    case List.Extra.unconsLast cards of
        Just ( last, rest ) ->
            rest ++ [ { last | isFacedUp = True } ]

        Nothing ->
            []


initializeGame : List Card -> Game
initializeGame shuffledCards =
    case chunkList shuffledCards [ 54, 50 ] of
        [ gameSlotCards, newCardSlots ] ->
            let
                gameslots =
                    chunkList gameSlotCards [ 5, 5, 5, 5, 4, 4, 4, 4, 4, 4 ]
                        |> List.map faceUpLastCard
            in
            { gameSlots = gameslots
            , drawCardSlots = chunkList newCardSlots (List.repeat 5 10)
            , moves = []
            , completedCardSlots = []
            , isDebug = True
            }

        _ ->
            { gameSlots = []
            , drawCardSlots = []
            , moves = []
            , completedCardSlots = []
            , isDebug = True
            }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitializedGame game ->
            ( RunningGame game, Cmd.none )

        ToggleDebug ->
            case model of
                UninitializedGame ->
                    ( model, Cmd.none )

                RunningGame game ->
                    ( RunningGame { game | isDebug = not game.isDebug }, Cmd.none )



---- VIEW ----


checkbox : Msg -> String -> Bool -> Html Msg
checkbox msg name isChecked =
    label
        [ style "padding" "20px" ]
        [ input [ type_ "checkbox", checked isChecked, onClick msg ] []
        , text name
        ]


view : Model -> Html Msg
view model =
    let
        isDebug =
            case model of
                UninitializedGame ->
                    False

                RunningGame game ->
                    game.isDebug
    in
    div
        [ Html.Attributes.class "gameView"
        ]
        [ div [ Html.Attributes.class "header" ]
            [ h1 [] [ text "Elm Spider Solitaire" ]
            ]
        , renderHtml
            model
        , div [ Html.Attributes.class "sidebar" ]
            [ h1 [] [ text "Sidebar" ]
            , fieldset [] [ checkbox ToggleDebug "debugView" isDebug ]
            ]
        , div [ Html.Attributes.class "footer" ]
            [ h1 [] [ text "Footer" ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
