module Main exposing (..)

import Browser
import Html exposing (Html, button, div, fieldset, h1, input, label, text)
import Html.Attributes exposing (checked, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
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
            , clickedCard = Nothing
            , selectedCard = Nothing
            }

        _ ->
            { gameSlots = []
            , drawCardSlots = []
            , moves = []
            , completedCardSlots = []
            , isDebug = True
            , seedValue = seedValue
            , seedValueTextboxEntry = seedValue
            , clickedCard = Nothing
            , selectedCard = Nothing
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

        ClickedCard stackLocation card ->
            case model of
                RunningGame game ->
                    ( RunningGame
                        (handleCardClick game stackLocation card)
                    , Cmd.none
                    )

        ClickedOnGameBoard ->
            case model of
                RunningGame game ->
                    ( RunningGame
                        { game
                            | clickedCard = Nothing
                            , selectedCard = Nothing
                        }
                    , Cmd.none
                    )


handleCardClick : Game -> StackLocation -> Card -> Game
handleCardClick game stackLocation newCard =
    let
        evaluatedMove =
            case game.selectedCard of
                Just ( previousSelectedCard, previousSelectedStackLocation ) ->
                    if isValidMove game previousSelectedStackLocation stackLocation then
                        Just { fromStackLocation = previousSelectedStackLocation, fromCard = previousSelectedCard, toStackLocation = stackLocation, toCard = newCard }

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    case evaluatedMove of
        Just move ->
            let
                newGame =
                    moveCard game move
                        |> faceUpTopmostCards
            in
            { newGame
                | clickedCard = Nothing
                , selectedCard = Nothing
            }

        Nothing ->
            { game
                | clickedCard = Just ( newCard, stackLocation )
                , selectedCard =
                    if canSelectCard game stackLocation newCard then
                        Just ( newCard, stackLocation )

                    else
                        Nothing
            }


faceUpTopmostCards : Game -> Game
faceUpTopmostCards game =
    let
        faceupTopmostOfStack : List Card -> List Card
        faceupTopmostOfStack cards =
            case List.Extra.unconsLast cards of
                Just ( last, others ) ->
                    others ++ [ { last | isFacedUp = True } ]

                Nothing ->
                    []
    in
    { game | gameSlots = List.map faceupTopmostOfStack game.gameSlots }


moveCard : Game -> Move -> Game
moveCard game { fromStackLocation, fromCard, toStackLocation, toCard } =
    {-
       logic:
       - take the cards from the fromStack
       - update the fromStack and remove the dragged cards
       - put the cards on top of the toStack
    -}
    let
        (StackIndex fromStackIndex) =
            fromStackLocation.stackIndex

        (CardIndex fromCardIndex) =
            fromStackLocation.cardIndex

        fromStack =
            List.Extra.getAt fromStackIndex game.gameSlots
                |> Maybe.withDefault []

        (StackIndex toStackIndex) =
            toStackLocation.stackIndex

        toStack =
            List.Extra.getAt toStackIndex game.gameSlots
                |> Maybe.withDefault []

        ( newFromStack, draggedCards ) =
            List.Extra.splitAt fromCardIndex fromStack

        newToStack =
            toStack ++ draggedCards

        newGameSlots =
            game.gameSlots
                |> List.Extra.updateAt fromStackIndex (\_ -> newFromStack)
                |> List.Extra.updateAt toStackIndex (\_ -> newToStack)
    in
    { game | gameSlots = newGameSlots }


type alias Move =
    { fromStackLocation : StackLocation
    , fromCard : Card
    , toStackLocation : StackLocation
    , toCard : Card
    }


isValidMove : Game -> StackLocation -> StackLocation -> Bool
isValidMove game from to =
    let
        isValidRank : Card -> Card -> Bool
        isValidRank fromCard toCard =
            cardRank fromCard + 1 == cardRank toCard

        isValidFromLocation : Bool
        isValidFromLocation =
            True

        isValidToLocation : Bool
        isValidToLocation =
            True

        validRank =
            case ( getCardAt game from, getCardAt game to ) of
                ( Just fromCard, Just toCard ) ->
                    isValidRank fromCard toCard

                _ ->
                    False
    in
    isValidFromLocation && isValidToLocation && validRank


getCardAt : Game -> StackLocation -> Maybe Card
getCardAt game stackLocation =
    let
        (StackIndex stackIndex) =
            stackLocation.stackIndex

        (CardIndex cardIndex) =
            stackLocation.cardIndex
    in
    game.gameSlots
        |> List.Extra.getAt stackIndex
        |> Maybe.andThen (List.Extra.getAt cardIndex)


canSelectCard : Game -> StackLocation -> Card -> Bool
canSelectCard game stackLocation card =
    let
        (StackIndex stackIndex) =
            stackLocation.stackIndex

        (CardIndex cardIndex) =
            stackLocation.cardIndex

        fromStack =
            List.Extra.getAt stackIndex game.gameSlots
                |> Maybe.withDefault []

        ( _, potentialSelection ) =
            List.Extra.splitAt cardIndex fromStack

        facedUpCount =
            potentialSelection
                |> List.filter .isFacedUp
                |> List.length

        allFacedUp =
            facedUpCount == List.length potentialSelection

        isValidRankStep : Card -> Card -> Bool
        isValidRankStep card1 card2 =
            card1.suit == card2.suit && cardRank card1 == cardRank card2 + 1

        isValidChain : List Card -> Bool
        isValidChain cards =
            let
                helper : List Card -> Card -> Bool
                helper rest current =
                    case rest of
                        [] ->
                            True

                        head :: newRest ->
                            if isValidRankStep current head then
                                helper newRest head

                            else
                                False
            in
            case cards of
                [] ->
                    False

                h :: tail ->
                    helper tail h
    in
    allFacedUp && isValidChain potentialSelection



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
        , renderGameBoard
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
