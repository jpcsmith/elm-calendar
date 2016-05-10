module Counter where

import Time exposing (..)
import Date exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- MODEL

type alias Model = Int


-- UPDATE

type Action = Increment | Decrement

update : Action -> Model -> Model
update action model =
  case action of
    Increment ->
      model + 1

    Decrement ->
      model - 1


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  table []
    [ thead []
      [ tr [] 
        [ th [] [ text "S" ]
        , th [] [ text "M" ]
        , th [] [ text "T" ]
        , th [] [ text "W" ]
        , th [] [ text "T" ]
        , th [] [ text "F" ]
        , th [] [ text "S" ]
        ]
      ]
    , layoutWeek (Date.fromTime 200000000.0)
    ]

  {--
  div []
    [ button [ onClick address Decrement ] [ text "-" ]
    , div [ countStyle ] [ text (toString model) ]
    , button [ onClick address Increment ] [ text "+" ]
    ]
  --}

secondsInWeek = 
  7 * 24 * 60 * 60

layoutWeek : Date -> Html
layoutWeek date = 
  if Date.day date == 1
    then layoutFirstWeek date
    else if Date.month (incrementByWeek date) /= Date.month date
      then layoutLastWeek date
      else 
        tr [] 
        (List.map createCalendarDay [(Date.day date)..(Date.day date + 6)])
        
      
layoutFirstWeek : Date -> Html
layoutFirstWeek date =
  tr [] []

layoutLastWeek : Date -> Html
layoutLastWeek date =
  tr [] []

createCalendarDay : Int -> Html
createCalendarDay day =
  td [] [ text (toString day) ]

incrementByWeek : Date -> Date
incrementByWeek date = 
  Date.fromTime (Time.inSeconds (Date.toTime date) + secondsInWeek)


countStyle : Attribute
countStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]
