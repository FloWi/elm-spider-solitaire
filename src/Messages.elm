module Messages exposing (..)

import Model exposing (..)


type Msg
    = InitializedGame Game
    | ToggleDebug
    | NewGameWithSeed Int
    | NewGameWithRandomSeed
    | ChangeSeedValueEntry String
    | ClickedCard StackLocation
    | ClickedOnGameBoard
    | ClickedOnEmptyPlaySlot StackType StackIndex
    | DrawNewCard
