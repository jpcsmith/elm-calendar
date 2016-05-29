module Calendar exposing (..)

import List 
import Date exposing (Date)

import Date.Extra.I18n.I_en_us exposing (monthName)
import Date.Extra.Floor as DateFloor
import Date.Extra.Core as DateExtra
import Date.Extra.Utils as DateUtils
import Date.Extra.Field as DateField
import Date.Extra.Duration as DateDuration
import Date.Extra.Compare as DateCompare

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Json.Decode as Json
import String
import Html.Attributes exposing (style, classList, class, src, disabled)
import Html.Events exposing (onClick)



main =
  App.beginnerProgram
    { model = emptyModel
    , view = view
    , update = update
    }

-- MODEL

type alias Model =
  { uid : Int
  , name : String
  , today: Date
  , viewDate: Date
  , image: String
  , completedDays: List Date
  }

emptyModel : Model
emptyModel = 
  { uid = 0
  , name = "Exercise"
  , today = DateUtils.unsafeFromString "August 26, 2016"
  , viewDate = DateUtils.unsafeFromString "August 1, 2016"
  , image = "images/baby-pushup-16-9.jpg" 
  , completedDays = []
  }


-- UPDATE

type Msg = PreviousMonth | NextMonth | Toggle Date

update : Msg -> Model -> Model
update msg model =
  case msg of
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

    Toggle day ->
      { model | completedDays = 
          let 
            equalToDay = DateCompare.is DateCompare.Same day
          in
            if List.any equalToDay model.completedDays then
              List.filter (\other -> not (equalToDay other)) model.completedDays
            else
              day :: model.completedDays 
      }

view : Model -> Html Msg
view model =
  let
    header = 
      List.map text [ "S", "M", "T", "W", "T", "F", "S" ]
        |> List.map (List.repeat 1)
        |> List.map (th [])
        |> thead []
  in 
    section [ class "card" ]
      [ div [ class "media" ] 
        [ img [ src model.image ] [] 
        , h2 [ class "title" ] [ text model.name ]
        ]
      , viewControls model
      , div [ class "body" ]
        [ table [] (header :: viewMonth model model.today model.viewDate) ]
      ]


viewControls : Model -> Html Msg
viewControls model =
  let
    month = Date.month model.viewDate |> monthName
    year = Date.year model.viewDate |> toString
  in
    nav [ class "header" ]
      [ button [ onClick PreviousMonth ] [ text "‹" ]
      , div [] [ month ++ ", " ++ year |> text  ]
      , button [ onClick NextMonth ] [ text "›" ]
      ]



{-| Splits a list of `td` into 7 day blocks. The length of the input list
should be a multiple of 7. ✔-}
splitWeeks: List (Html Msg) -> List (List (Html Msg))
splitWeeks days =
  if List.length days == 0 then []
  else
    let 
      thisWeek = List.take 7 days |> List.repeat 1
      remainingDays = List.drop 7 days
    in
      List.append thisWeek (splitWeeks remainingDays)


viewDay : Model -> Bool -> Date -> Date -> Int -> Html Msg
viewDay model isExternal today viewedMonth day =
  let
    today = DateFloor.floor DateFloor.Day today
    viewedDay = 
      DateField.fieldToDateClamp (DateField.DayOfMonth day) viewedMonth
        |> DateFloor.floor DateFloor.Day 
    isToday = 
        (&&) (Date.day today == day) (Date.month today == Date.month viewedMonth)
        |> (&&) (Date.year today == Date.year viewedMonth)
  in
    (if List.any (DateCompare.is DateCompare.Same viewedDay) model.completedDays then "✔" else toString day)
      |> text
      |> List.repeat 1
      |> button 
        [ disabled (DateCompare.is DateCompare.After viewedDay today)
        , onClick (Toggle viewedDay)
        ]
      |> List.repeat 1
      |> td 
         [ classList 
           [ ("other-month", isExternal)
           , ("today", DateCompare.is DateCompare.Same today viewedDay) 
           ]]



{-| Craft the Html for a list of days. The class `other-month` is attached if
the `isExternal` is True. -}
viewDays : Model -> List Int -> Date -> Date -> Bool -> List (Html Msg)
viewDays model days viewedMonth today isExternal =
  List.map (viewDay model isExternal today viewedMonth) days


{-| Craft the Html for a month. -}
viewMonth : Model -> Date -> Date -> List (Html Msg)
viewMonth model today date = 
  let
    previousDays = 
      let days = lastWeekOfMonth (prevMonth date)
      in if List.length days < 7 then days else []
    currentDays = [1..(DateExtra.daysInMonthDate date)]
    nextDays = 
      let days = firstWeekOfMonth (nextMonth date)
      in if List.length days < 7 then days else []
  in
    viewDays model nextDays (nextMonth date) today True
      |> List.append (viewDays model currentDays date today False)
      |> List.append (viewDays model previousDays (prevMonth date) today True)
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


prevMonth : Date -> Date
prevMonth date =
  DateDuration.add DateDuration.Month -1 date


nextMonth : Date -> Date
nextMonth date =
  DateDuration.add DateDuration.Month 1 date
