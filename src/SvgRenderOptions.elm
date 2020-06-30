module SvgRenderOptions exposing (renderOptions)


gameWidth =
    1600


gameHeight =
    1024


playStackXOffset =
    25


cardHeight =
    cardWidth / cardAspectRatio


cardWidth =
    100


cardAspectRatio : Float
cardAspectRatio =
    224 / 313


renderOptions :
    { cardInStackVerticalOffset : Int
    , playStackXOffset : Float
    , playStackTopOffset : Float
    , cardHeight : Float
    , cardWidth : Float
    , drawNewCardXOffset : Float
    , drawNewCardYOffset : Float
    , gameWidth : Int
    , gameHeight : Int
    }
renderOptions =
    { cardInStackVerticalOffset = 30
    , playStackXOffset = playStackXOffset
    , playStackTopOffset = playStackXOffset
    , cardHeight = cardHeight
    , cardWidth = cardWidth
    , drawNewCardXOffset = gameWidth - cardWidth - playStackXOffset
    , drawNewCardYOffset = gameHeight - cardHeight - playStackXOffset
    , gameWidth = gameWidth
    , gameHeight = gameHeight
    }
