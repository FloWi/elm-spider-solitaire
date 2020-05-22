module Main exposing (..)

import Browser
import Html exposing (Html, div, h1, h2, h3, img, p, text)
import Html.Attributes exposing (src)
import List.Extra
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
---- MODEL ----


type Suit
    = Clubs
    | Diamonds
    | Hearts
    | Spades


type Rank
    = Ace
    | King
    | Queen
    | Jack
    | R10
    | R9
    | R8
    | R7
    | R6
    | R5
    | R4
    | R3
    | R2


type alias Card =
    { suit : Suit
    , rank : Rank
    , isFacedUp : Bool
    }


type alias Game =
    { gameSlots : List (List Card)
    , drawCardSlots : List (List Card)
    , moves : List Move
    , completedCardSlots : List (List Card)
    }


type CardStack
    = List Card


type alias CardIndex =
    { slotIndex : Int, cardIndex : Int }


type Move
    = DrawNewCards
    | MoveCardStack { startCardIndex : CardIndex, cardStack : CardStack, destinationSlotIndex : Int }


type Model
    = UninitializedGame
    | RunningGame Game


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


type Msg
    = InitializedGame Game


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitializedGame game ->
            ( RunningGame game, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        UninitializedGame ->
            div []
                [ h1 [] [ text "uninitialized" ] ]

        RunningGame game ->
            div []
                [ h1 [] [ text "Game is on" ]
                , renderGameSlots "Game Slots" game.gameSlots
                , renderGameSlots "Draw new card stacks" game.drawCardSlots
                , renderGameSlots "completed card slots" game.completedCardSlots
                ]


suitString : Suit -> String
suitString suit =
    case suit of
        Clubs ->
            "♣"

        Diamonds ->
            "♦"

        Hearts ->
            "♥"

        Spades ->
            "♠"


rankString : Rank -> String
rankString rank =
    case rank of
        Ace ->
            "A"

        King ->
            "K"

        Queen ->
            "Q"

        Jack ->
            "J"

        R10 ->
            "10"

        R9 ->
            "9"

        R8 ->
            "8"

        R7 ->
            "7"

        R6 ->
            "6"

        R5 ->
            "5"

        R4 ->
            "4"

        R3 ->
            "3"

        R2 ->
            "2"


renderCard : Card -> Html msg
renderCard card =
    let
        rank =
            rankString card.rank

        suit =
            suitString card.suit

        hidden : List (Html.Attribute msg)
        hidden =
            if card.isFacedUp then
                []

            else
                [ Html.Attributes.class "hiddenCard" ]
    in
    p hidden [ text (suit ++ rank) ]


renderGameSlots : String -> List (List Card) -> Html msg
renderGameSlots label gameSlots =
    let
        renderGameSlot : List Card -> Html msg
        renderGameSlot cards =
            cards
                |> List.map (\card -> p [] [ renderCard card ])
                |> div []

        gameSlotDivs : List (Html msg)
        gameSlotDivs =
            gameSlots
                |> List.indexedMap Tuple.pair
                |> List.map
                    (\( i, cards ) ->
                        div
                            []
                            [ text ("stack " ++ String.fromInt i)
                            , renderGameSlot cards
                            ]
                    )
    in
    div []
        [ h2 [] [ text label ]
        , div
            [ Html.Attributes.class "gameStacks" ]
            gameSlotDivs
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
