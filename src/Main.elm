module Main exposing (..)

import Browser
import Html exposing (Html, button, div, fieldset, h1, input, label, p, text)
import Html.Attributes exposing (checked, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import HtmlRenderer exposing (renderAllCardsAsImages, renderHtml)
import List.Extra
import Messages exposing (..)
import Model exposing (..)
import Random exposing (..)
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
    ( createGameBySeed 42
    , Cmd.none
    )


createGameBySeed : Int -> Model
createGameBySeed seedValue =
    let
        seed =
            Random.initialSeed seedValue

        cardsGenerator : Generator (List Card)
        cardsGenerator =
            Random.List.shuffle calcGameCards

        ( cards, seed_ ) =
            Random.step cardsGenerator seed

        game =
            initializeGame cards seedValue
    in
    RunningGame game



--generate : (a → msg) → Generator a → Cmd msg
--generate tagger generator defined in Random
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


initializeGame : List Card -> Int -> Game
initializeGame shuffledCards seedValue =
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
            , seedValue = seedValue
            , seedValueTextboxEntry = seedValue
            }

        _ ->
            { gameSlots = []
            , drawCardSlots = []
            , moves = []
            , completedCardSlots = []
            , isDebug = True
            , seedValue = seedValue
            , seedValueTextboxEntry = seedValue
            }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitializedGame game ->
            ( RunningGame game, Cmd.none )

        ToggleDebug ->
            case model of
                RunningGame game ->
                    ( RunningGame { game | isDebug = not game.isDebug }, Cmd.none )

        NewGameWithSeed seedValue ->
            ( createGameBySeed seedValue, Cmd.none )

        NewGameWithRandomSeed ->
            ( model, Random.generate NewGameWithSeed (Random.int Random.minInt Random.maxInt) )

        ChangeSeedValueEntry newValue ->
            case String.toInt newValue of
                Nothing ->
                    ( model, Cmd.none )

                Just parsed ->
                    case model of
                        RunningGame game ->
                            ( RunningGame { game | seedValueTextboxEntry = parsed }, Cmd.none )



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
        ( isDebug, seedValue, seedValueTextboxEntry ) =
            case model of
                RunningGame game ->
                    ( game.isDebug, game.seedValue, game.seedValueTextboxEntry )
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
            , fieldset []
                [ checkbox ToggleDebug "debugView" isDebug
                , input [ placeholder "seedvalue", value (String.fromInt seedValueTextboxEntry), onInput ChangeSeedValueEntry ] []
                , button [ onClick (NewGameWithSeed seedValueTextboxEntry) ] [ text "Restart with seed" ]
                , button [ onClick NewGameWithRandomSeed ] [ text "Restart with random seed" ]
                ]
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
