module SvgSymbols exposing (..)

import Model exposing (Card, Rank(..), Suit(..), suitColor)
import Svg exposing (Attribute, Svg, symbol)
import Svg.Attributes exposing (..)


rankId rank suit =
    "rank" ++ String.fromInt (Model.rankValue rank) ++ "_" ++ Model.suitColor suit


rankSvgSymbol : Rank -> Suit -> Svg msg
rankSvgSymbol rank suit =
    let
        c : String
        c =
            suitColor suit

        rankSymbol pathData =
            symbol
                [ id (rankId rank suit)
                , viewBox "-500 -500 1000 1000"
                , preserveAspectRatio "xMinYMid"
                ]
                [ Svg.path
                    [ stroke c
                    , d pathData
                    , strokeWidth "80"
                    , strokeLinecap "square"
                    , strokeMiterlimit "1.5"
                    , fill "none"
                    ]
                    []
                ]

        result =
            case rank of
                King ->
                    rankSymbol "M-285 -460L-85 -460M-185 -460L-185 460M-285 460L-85 460M85 -460L285 -460M185 -440L-170 155M85 460L285 460M185 440L-10 -70"

                Queen ->
                    rankSymbol "M-260 100C40 100 -40 460 260 460M-175 0L-175 -285A175 175 0 0 1 175 -285L175 285A175 175 0 0 1 -175 285Z"

                Jack ->
                    rankSymbol "M50 -460L250 -460M150 -460L150 250A100 100 0 0 1 -250 250L-250 220"

                R10 ->
                    rankSymbol "M-260 430L-260 -430M-50 0L-50 -310A150 150 0 0 1 250 -310L250 310A150 150 0 0 1 -50 310Z"

                R9 ->
                    rankSymbol "M250 -100A250 250 0 0 1 -250 -100L-250 -210A250 250 0 0 1 250 -210L250 210A250 250 0 0 1 0 460C-150 460 -180 400 -200 375"

                R8 ->
                    rankSymbol "M-1 -50A205 205 0 1 1 1 -50L-1 -50A255 255 0 1 0 1 -50Z"

                R7 ->
                    rankSymbol "M-265 -320L-265 -460L265 -460C135 -200 -90 100 -90 460"

                R6 ->
                    rankSymbol "M-250 100A250 250 0 0 1 250 100L250 210A250 250 0 0 1 -250 210L-250 -210A250 250 0 0 1 0 -460C150 -460 180 -400 200 -375"

                R5 ->
                    rankSymbol "M170 -460L-175 -460L-210 -115C-210 -115 -200 -200 0 -200C100 -200 255 -80 255 120C255 320 180 460 -20 460C-220 460 -255 285 -255 285"

                R4 ->
                    rankSymbol "M50 460L250 460M150 460L150 -460L-300 175L-300 200L270 200"

                R3 ->
                    rankSymbol "M-250 -320L-250 -460L200 -460L-110 -80C-100 -90 -50 -120 0 -120C200 -120 250 0 250 150C250 350 170 460 -30 460C-230 460 -260 300 -260 300"

                R2 ->
                    rankSymbol "M-225 -225C-245 -265 -200 -460 0 -460C 200 -460 225 -325 225 -225C225 -25 -225 160 -225 460L225 460L225 300"

                Ace ->
                    rankSymbol "M-270 460L-110 460M-200 450L0 -460L200 450M110 460L270 460M-120 130L120 130"
    in
    result


suitId suit =
    "suit" ++ Model.suitString suit


suitSvgSymbol : Suit -> Svg msg
suitSvgSymbol suit =
    let
        suitSymbol pathData =
            symbol
                [ id (suitId suit)
                , viewBox "-600 -600 1200 1200"
                , preserveAspectRatio "xMinYMid"
                ]
                [ Svg.path
                    [ d pathData
                    , fill (suitColor suit)
                    ]
                    []
                ]
    in
    case suit of
        Clubs ->
            suitSymbol "M30 150C35 385 85 400 130 500L-130 500C-85 400 -35 385 -30 150A10 10 0 0 0 -50 150A210 210 0 1 1 -124 -51A10 10 0 0 0 -110 -65A230 230 0 1 1 110 -65A10 10 0 0 0 124 -51A210 210 0 1 1 50 150A10 10 0 0 0 30 150Z"

        Diamonds ->
            suitSymbol "M-400 0C-350 0 0 -450 0 -500C0 -450 350 0 400 0C350 0 0 450 0 500C0 450 -350 0 -400 0Z"

        Hearts ->
            suitSymbol "M0 -300C0 -400 100 -500 200 -500C300 -500 400 -400 400 -250C400 0 0 400 0 500C0 400 -400 0 -400 -250C-400 -400 -300 -500 -200 -500C-100 -500 0 -400 -0 -300Z"

        Spades ->
            suitSymbol "M0 -500C100 -250 355 -100 355 185A150 150 0 0 1 55 185A10 10 0 0 0 35 185C35 385 85 400 130 500L-130 500C-85 400 -35 385 -35 185A10 10 0 0 0 -55 185A150 150 0 0 1 -355 185C-355 -100 -100 -250 0 -500Z"
