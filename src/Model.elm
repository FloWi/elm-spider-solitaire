module Model exposing (..)

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
