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
    , seedValue : Int
    , seedValueTextboxEntry : Int
    , clickedCard : Maybe ( Card, StackLocation )
    }


type CardStack
    = List Card


type Move
    = DrawNewCards



--| MoveCardStack { startCardIndex : CardIndex, cardStack : CardStack, destinationSlotIndex : Int }


type Model
    = RunningGame Game


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


type alias Location =
    { x : Int
    , y : Int
    }


type StackType
    = PlayStack
    | DrawStack


type StackIndex
    = StackIndex Int


type CardIndex
    = CardIndex Int


stackIndexInt : StackIndex -> Int
stackIndexInt si =
    case si of
        StackIndex i ->
            i


cardIndexInt : CardIndex -> Int
cardIndexInt si =
    case si of
        CardIndex i ->
            i


stackTypeString : StackType -> String
stackTypeString stackType =
    case stackType of
        PlayStack ->
            "PlayStack"

        DrawStack ->
            "DrawStack"


type alias StackLocation =
    { stackType : StackType
    , stackIndex : StackIndex
    , cardIndex : CardIndex
    }


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


cardString : Card -> String
cardString card =
    rankString card.rank ++ suitString card.suit


cardFileName : Card -> String
cardFileName card =
    case card.isFacedUp of
        True ->
            cardString card ++ ".svg"

        False ->
            "RED_BACK.svg"


cardImgUrl : Card -> String
cardImgUrl card =
    "%PUBLIC_URL%/cards/cards/" ++ cardFileName card
