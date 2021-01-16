module Utils exposing (..)


import Dict
import Http

import Api exposing (..)


sumWorks : List ElmWork -> List(String, Float)
sumWorks works =
    let filterByNotes notes = List.filter (byNotes notes) works
        byNotes notes work = work.notes == notes
        hours notes = List.map (\w -> Maybe.withDefault 0 w.hours) (filterByNotes notes)
        summary notes = List.foldl (+) 0 (hours notes)
        sum work = (work.notes, summary work.notes)
    in List.map sum works |> Dict.fromList |> Dict.toList


formatDate : ElmDateTime -> String
formatDate dt = case (dt.day, dt.time) of
    (d, t) -> case [d.year, d.month, d.dom, t.hour, t.min] of
      datetimes -> case List.map (String.pad 2 '0' << String.fromInt) datetimes of
        [year, month, dom, hour, min] -> case [[year, month, dom], [hour, min]] of
          [day, time] -> (String.join "-" day) ++ " " ++ (String.join ":" time)
          _ -> ""
        _ -> ""


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl s ->
            "bad url: " ++ s

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus status ->
            "bad status: " ++ String.fromInt status

        Http.BadBody response ->
            "bad payload: " ++ response


