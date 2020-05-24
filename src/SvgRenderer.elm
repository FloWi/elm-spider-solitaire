module SvgRenderer exposing (..)

import Html exposing (Html, div)
import Html.Attributes
import Messages exposing (..)
import Model exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)


renderGameBoard model =
    case model of
        RunningGame game ->
            let
                ( containerClass, debugScreenDiv ) =
                    if game.isDebug then
                        ( "contentDebug"
                        , [ renderDebugScreen game ]
                        )

                    else
                        ( "content", [] )
            in
            div
                [ Html.Attributes.class containerClass
                ]
                ([ div [ Html.Attributes.class "gameScreen" ]
                    [ svg
                        [ class "svgGameTable"
                        , preserveAspectRatio "xMinYMin meet"
                        , viewBox "0 0 1000 1000"
                        ]
                        [ renderPlaySlots game.gameSlots
                        ]
                    ]
                 ]
                    ++ debugScreenDiv
                )


renderDebugScreen : Game -> Html Msg
renderDebugScreen game =
    let
        clickedCardText =
            case game.clickedCard of
                Just ( card, stackLocation ) ->
                    let
                        si =
                            stackIndexInt stackLocation.stackIndex

                        ci =
                            cardIndexInt stackLocation.cardIndex

                        stackTypeStr =
                            stackTypeString stackLocation.stackType

                        infoString =
                            [ "clicked card: " ++ cardString card
                            , "StackType: " ++ stackTypeStr
                            , "StackIndex: " ++ String.fromInt si
                            , "CardIndex: " ++ String.fromInt ci
                            ]
                                |> String.join "\n"
                    in
                    [ Html.pre
                        [ Html.Attributes.align "left"
                        ]
                        [ Html.text
                            infoString
                        ]
                    ]

                Nothing ->
                    []
    in
    div [ Html.Attributes.class "debugScreen" ]
        ([ Html.h2 []
            [ Html.text "DebugScreen" ]
         ]
            ++ clickedCardText
        )


renderPlaySlots : List (List Card) -> Svg Msg
renderPlaySlots cardSlots =
    cardSlots
        |> List.indexedMap renderPlaySlot
        |> Svg.g []


renderPlaySlot : Int -> List Card -> Svg Msg
renderPlaySlot slotIndex cards =
    let
        location =
            { x = renderOptions.playStackLeftOffset + slotIndex * (round renderOptions.cardWidth + renderOptions.playStackXOffset)
            , y = renderOptions.playStackTopOffset
            }
    in
    renderCardStack cards location (StackIndex slotIndex) PlayStack


cardHeight =
    200


cardAspectRatio : Float
cardAspectRatio =
    224 / 313


renderOptions :
    { cardInStackVerticalOffset : Int
    , playStackXOffset : Int
    , playStackLeftOffset : Int
    , playStackTopOffset : Int
    , cardHeight : Float
    , cardWidth : Float
    }
renderOptions =
    { cardInStackVerticalOffset = 20
    , playStackXOffset = 25
    , playStackLeftOffset = 25
    , playStackTopOffset = 50
    , cardHeight = cardHeight
    , cardWidth = cardAspectRatio * cardHeight
    }


renderCardStack : List Card -> Location -> StackIndex -> StackType -> Svg Msg
renderCardStack cards stackBasePosition stackindex stacktype =
    cards
        |> List.indexedMap
            (\i card ->
                let
                    position =
                        { stackBasePosition | y = stackBasePosition.y + i * renderOptions.cardInStackVerticalOffset }

                    stackLocation =
                        { stackType = stacktype
                        , stackIndex = stackindex
                        , cardIndex = CardIndex i
                        }
                in
                renderCard card stackLocation position
            )
        |> Svg.g []


renderCard : Card -> StackLocation -> Location -> Svg Msg
renderCard card stackLocation location =
    Svg.image
        [ x (String.fromInt location.x)
        , y (String.fromInt location.y)

        --, width "50"
        , height (String.fromFloat renderOptions.cardHeight)
        , xlinkHref (cardImgUrl card)
        , onClick (ClickedCard stackLocation card)
        ]
        []



--renderCard : Card -> Html msg
--renderCard card =
--    let
--        rank =
--            rankString card.rank
--
--        suit =
--            suitString card.suit
--
--        hidden : List (Html.Attribute msg)
--        hidden =
--            if card.isFacedUp then
--                []
--
--            else
--                [ Html.Attributes.class "hiddenCard" ]
--    in
--    p hidden [ text (suit ++ rank) ]
--renderGameSlots : String -> List (List Card) -> Html msg
--renderGameSlots label gameSlots =
--    let
--        renderGameSlot : List Card -> Html msg
--        renderGameSlot cards =
--            cards
--                |> List.map (\card -> p [] [ renderCard card ])
--                |> div []
--
--        gameSlotDivs : List (Html msg)
--        gameSlotDivs =
--            gameSlots
--                |> List.indexedMap Tuple.pair
--                |> List.map
--                    (\( i, cards ) ->
--                        div
--                            []
--                            [ text ("stack " ++ String.fromInt i)
--                            , renderGameSlot cards
--                            ]
--                    )
--    in
--    div []
--        [ h2 [] [ text label ]
--        , div
--            [ Html.Attributes.class "gameStacks" ]
--            gameSlotDivs
--        ]
