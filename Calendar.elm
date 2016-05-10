module Calendar where 

import List 
import Date exposing (Date)

import Date.Extra.I18n.I_en_us exposing (monthName)
import Date.Extra.Floor as DateFloor
import Date.Extra.Core as DateExtra
import Date.Extra.Utils as DateUtils
import Date.Extra.Duration as DateDuration

import Html exposing (..)
import Html.Attributes exposing (style, classList)
import Html.Events exposing (onClick)


-- MODEL

type alias Model =
  { uid : Int
  , name : String
  , viewDate: Date
  }

emptyModel : Model
emptyModel = 
  { uid = 0
  , name = ""
  , viewDate = DateUtils.unsafeFromString "August 1, 2016"
  }


prevMonth : Date -> Date
prevMonth date =
  DateDuration.add DateDuration.Month -1 date


nextMonth : Date -> Date
nextMonth date =
  DateDuration.add DateDuration.Month 1 date



-- UPDATE

type Action = PreviousMonth | NextMonth

update : Action -> Model -> Model
update action model =
  case action of
    PreviousMonth ->
      { model | viewDate = 
        prevMonth model.viewDate
          |> DateFloor.floor DateFloor.Month
      }

    NextMonth ->
      { model | viewDate = 
        nextMonth model.viewDate
          |> DateFloor.floor DateFloor.Month
      }

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  let
    header = 
      List.map text [ "S", "M", "T", "W", "T", "F", "S" ]
        |> List.map (List.repeat 1)
        |> List.map (th [])
        |> thead []
  in 
    div [] 
      [ viewControls address model
      , table [] (header :: viewMonth model.viewDate)
      ]


viewControls : Signal.Address Action -> Model -> Html
viewControls address model =
  let
    month = Date.month model.viewDate |> monthName
    year = Date.year model.viewDate |> toString
  in
    div []
      [ button [ onClick address PreviousMonth ] [ text "-" ]
      , div [] [ month ++ ", " ++ year |> text  ]
      , button [ onClick address NextMonth ] [ text "+" ]
      ]


{-| Splits a list of `td` into 7 day blocks. The length of the input list
should be a multiple of 7. -}
splitWeeks: List Html -> List (List Html)
splitWeeks days =
  if List.length days == 0 then []
  else
    let 
      thisWeek = List.take 7 days |> List.repeat 1
      remainingDays = List.drop 7 days
    in
      List.append thisWeek (splitWeeks remainingDays)


{-| Craft the Html for a list of days. The class `other-month` is attached if
the `isExternal` is True. -}
viewDays : List Int -> Bool -> List Html
viewDays days isExternal =
  List.map toString days
    |> List.map text
    |> List.map (List.repeat 1)
    |> List.map (td [ classList [ ("other-month", isExternal) ] ])


{-| Craft the Html for a month. -}
viewMonth : Date -> List Html
viewMonth date = 
  let
    previousDays = 
      let days = lastWeekOfMonth (prevMonth date)
      in if List.length days < 7 then days else []
    currentDays = [1..(DateExtra.daysInMonthDate date)]
    nextDays = 
      let days = firstWeekOfMonth (nextMonth date)
      in if List.length days < 7 then days else []
  in
    viewDays nextDays True
      |> List.append (viewDays currentDays False)
      |> List.append (viewDays previousDays True)
      |> splitWeeks
      |> List.map (tr [])


{-| Get the list of days in the last week of the month. The week begins on
Sunday. -}
lastWeekOfMonth : Date -> List Int
lastWeekOfMonth date = 
  let
    days = 
      let weekDay = Date.dayOfWeek (DateExtra.lastOfMonthDate date)
      in DateExtra.daysBackToStartOfWeek weekDay Date.Sun
    daysInMonth = DateExtra.daysInMonthDate date
  in
    [(daysInMonth - days)..(daysInMonth)]


{-| Get the list of days in the first week of the month. The week begins on
Sunday. -}
firstWeekOfMonth : Date -> List Int
firstWeekOfMonth date =
  let 
    days = 
      let weekDay = Date.dayOfWeek (DateExtra.toFirstOfMonth date)
      in DateExtra.daysBackToStartOfWeek Date.Sun weekDay
    dd = Debug.log "D" (Date.day date)
  in
    [1..days]
