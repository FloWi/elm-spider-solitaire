module Main exposing (..)

import Browser
import Html exposing (Html)
import HtmlRenderer exposing (..)
import List.Extra
import Messages exposing (..)
import Model exposing (..)
import Random
import Random.List



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
    ( UninitializedGame, Random.generate InitializedGame (Random.map initializeGame (Random.List.shuffle calcDecks)) )


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs



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


calcDecks : List Card
calcDecks =
    let
        suits =
            [ Clubs, Diamonds, Hearts, Spades ]

        ranks =
            [ Ace, King, Queen, Jack, R10, R9, R8, R7, R6, R5, R4, R3, R2 ]

        deck : List Card
        deck =
            List.map (\( s, r ) -> { suit = s, rank = r, isFacedUp = False }) (cartesian suits ranks)

        allCards : List Card
        allCards =
            List.append deck deck
    in
    allCards


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
            }

        _ ->
            { gameSlots = []
            , drawCardSlots = []
            , moves = []
            , completedCardSlots = []
            }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitializedGame game ->
            ( RunningGame game, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    renderHtml model



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
