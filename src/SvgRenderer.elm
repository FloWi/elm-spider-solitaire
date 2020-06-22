module SvgRenderer exposing (..)

import Html exposing (Html, div)
import Html.Attributes
import Messages exposing (..)
import Model exposing (..)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import SvgRenderOptions exposing (renderOptions)


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
                        [ class "svgGame"
                        , preserveAspectRatio "xMinYMin meet"
                        , viewBox "0 0 1600 1024"
                        ]
                        [ Svg.g []
                            [ renderGameTable game
                            , renderPlaySlots game.gameSlots (Maybe.map Tuple.second game.selectedCard)
                            ]
                        ]
                    ]
                 ]
                    ++ debugScreenDiv
                )


renderGameTable : Game -> Svg Msg
renderGameTable game =
    Svg.rect
        [ x "0"
        , y "0"
        , width "1600"
        , height "1024"
        , rx "15"
        , ry "15"
        , class "svgGameTable"
        , onClick ClickedOnGameBoard
        ]
        []


renderDebugScreen : Game -> Html Msg
renderDebugScreen game =
    let
        cardInfoString card stackLocation =
            let
                si =
                    stackIndexInt stackLocation.stackIndex

                ci =
                    cardIndexInt stackLocation.cardIndex

                stackTypeStr =
                    stackTypeString stackLocation.stackType
            in
            [ "card: " ++ cardString card
            , "StackType: " ++ stackTypeStr
            , "StackIndex: " ++ String.fromInt si
            , "CardIndex: " ++ String.fromInt ci
            ]
                |> String.join "\n"

        clickedCardText =
            case game.clickedCard of
                Just ( card, stackLocation ) ->
                    [ Html.h5 [] [ Html.text "Clicked Card" ]
                    , Html.pre [ Html.Attributes.align "left" ] [ Html.text (cardInfoString card stackLocation) ]
                    ]

                Nothing ->
                    []

        selectedCardText =
            case game.selectedCard of
                Just ( card, stackLocation ) ->
                    [ Html.h5 [] [ Html.text "Selected Card" ]
                    , Html.pre [ Html.Attributes.align "left" ] [ Html.text (cardInfoString card stackLocation) ]
                    ]

                Nothing ->
                    []
    in
    div [ Html.Attributes.class "debugScreen" ]
        ([ Html.h2 []
            [ Html.text "DebugScreen" ]
         ]
            ++ clickedCardText
            ++ selectedCardText
        )


renderPlaySlots : List (List Card) -> Maybe StackLocation -> Svg Msg
renderPlaySlots cardSlots selectedCardLocation =
    cardSlots
        |> List.indexedMap (renderPlaySlot selectedCardLocation)
        |> Svg.g []


renderPlaySlot : Maybe StackLocation -> Int -> List Card -> Svg Msg
renderPlaySlot selectedCardLocation slotIndex cards =
    let
        location =
            { x = round (renderOptions.playStackXOffset + toFloat slotIndex * (renderOptions.cardWidth + renderOptions.playStackXOffset))
            , y = renderOptions.playStackTopOffset
            }

        emptyStackIndicator =
            Svg.rect
                [ x (String.fromInt location.x)
                , y (String.fromInt location.y)
                , width (String.fromInt (round renderOptions.cardWidth))
                , height (String.fromInt (round renderOptions.cardHeight))
                , rx "5"
                , ry "5"
                , class "emptyPlaySlot"
                , onClick (ClickedOnEmptyPlaySlot PlayStack (StackIndex slotIndex))
                ]
                []

        cardStack =
            renderCardStack selectedCardLocation cards location (StackIndex slotIndex) PlayStack
    in
    Svg.g [] [ emptyStackIndicator, cardStack ]



-- 10 cards evenly spread out over 10 slots


renderCardStack : Maybe StackLocation -> List Card -> Location -> StackIndex -> StackType -> Svg Msg
renderCardStack selectedCardLocation cards stackBasePosition stackindex stacktype =
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
                renderCard card selectedCardLocation stackLocation position
            )
        |> Svg.g []


renderCard : Card -> Maybe StackLocation -> StackLocation -> Location -> Svg Msg
renderCard card selectedCardLocation stackLocation location =
    let
        isSelectedCard =
            case selectedCardLocation of
                Just a ->
                    a == stackLocation

                Nothing ->
                    False

        cardImage =
            Svg.image
                [ x (String.fromInt location.x)
                , y (String.fromInt location.y)
                , width (String.fromFloat renderOptions.cardWidth)
                , height (String.fromFloat renderOptions.cardHeight)
                , xlinkHref (cardImgUrl card)
                , onClick (ClickedCard stackLocation card)
                , class
                    (if isSelectedCard then
                        "isSelected"

                     else
                        "notSelected"
                    )
                ]
                []
    in
    cardImage
