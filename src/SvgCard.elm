module SvgCard exposing (..)

import Dict
import Html
import Messages exposing (Msg)
import Model exposing (Card, Rank(..), Suit(..))
import RankSvgSymbol exposing (rankId, rankSvgSymbol)
import Svg exposing (Attribute, Svg, g, rect, svg, symbol, use)
import Svg.Attributes exposing (..)


suitSvgSymbol : Suit -> Svg msg
suitSvgSymbol suit =
    case suit of
        Spades ->
            symbol
                [ id "symbol-spades"
                , viewBox "-600 -600 1200 1200"
                , preserveAspectRatio "xMinYMid"
                ]
                [ Svg.path
                    [ d "M0 -500C100 -250 355 -100 355 185A150 150 0 0 1 55 185A10 10 0 0 0 35 185C35 385 85 400 130 500L-130 500C-85 400 -35 385 -35 185A10 10 0 0 0 -55 185A150 150 0 0 1 -355 185C-355 -100 -100 -250 0 -500Z"
                    , fill "black"
                    ]
                    []
                ]

        _ ->
            svg [] []


border =
    rect
        [ width "239"
        , height "335"
        , x "-119.5"
        , y "-167.5"
        , rx "12"
        , ry "12"
        , fill "white"
        , stroke "black"
        ]
        []


cardSvg : List (Html.Attribute Msg) -> Card -> Svg Msg
cardSvg attributes card =
    let
        rankSymbol =
            rankSvgSymbol card.rank

        suitSymbol =
            suitSvgSymbol card.suit

        topLeftRank =
            use
                [ xlinkHref ("#" ++ rankId card.rank)
                , height "32"
                , x "-114.4"
                , y "-156"
                ]
                []

        topLeftSuit =
            use
                [ xlinkHref "#symbol-spades"
                , height "26.769"
                , x "-111.784"
                , y "-119"
                ]
                []

        suits =
            use
                [ xlinkHref "#symbol-spades"
                , height "70"
                , x "-35"
                , y "-135.501"
                ]
                []

        group =
            g []
                [ topLeftRank
                , topLeftSuit
                , suits
                ]
    in
    svg
        ([ class "card"
         , height "3.5in"
         , width "2.5in"
         , preserveAspectRatio "none"
         , viewBox "-120 -168 240 336"
         ]
            ++ attributes
        )
        [ border
        , rankSymbol
        , suitSymbol
        , group
        , g [ transform "rotate(180)" ] [ group ]
        ]
