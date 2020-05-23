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
    , isDebug : Bool
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


suitUtf8Symbol : Suit -> String
suitUtf8Symbol suit =
    case suit of
        Clubs ->
            "♣"

        Diamonds ->
            "♦"

        Hearts ->
            "♥"

        Spades ->
            "♠"


suitString : Suit -> String
suitString suit =
    case suit of
        Clubs ->
            "C"

        Diamonds ->
            "D"

        Hearts ->
            "H"

        Spades ->
            "S"


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


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


calcDeck : List Card
calcDeck =
    let
        suits =
            [ Clubs, Diamonds, Hearts, Spades ]

        ranks =
            [ Ace, King, Queen, Jack, R10, R9, R8, R7, R6, R5, R4, R3, R2 ]

        deck : List Card
        deck =
            List.map (\( s, r ) -> { suit = s, rank = r, isFacedUp = False }) (cartesian suits ranks)
    in
    deck


cardFileName : Card -> String
cardFileName card =
    case card.isFacedUp of
        True ->
            rankString card.rank ++ suitString card.suit ++ ".svg"

        False ->
            "RED_BACK.svg"


cardImgUrl : Card -> String
cardImgUrl card =
    "%PUBLIC_URL%/cards/cards/" ++ cardFileName card
