module Main exposing (FromServer(..), FromUi(..), Model, Msg(..), fromServer, init, main, update, view, viewItem)

import Api exposing (..)
import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (Html, button, div, input, li, option, pre, select, span, table, tr, th, td, text, textarea, ul)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import List exposing (filter, head, map)
import String exposing (fromFloat, fromInt, join, split, toInt)
import Task exposing (andThen)
import Tuple exposing (first, second)


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

-- MODEL

type alias Model =
    { items : Dict Int Item
    , projects : List ElmProject
    , addProjectInput : String
    , addProjectUnitPriceInput : Int
    , addWorkProjectNameInput : String
    , addWorkFromInput : Maybe ElmDateTime
    , addWorkToInput : Maybe ElmDateTime
    , addWorkNotesInput : String
    , currentProject : Maybe ElmProject
    , showSummary : Bool
    , works : Dict ElmProjectId (List ElmWork)
    , notes : String
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( Model Dict.empty [] "" 0 "" Nothing Nothing "" Nothing False Dict.empty "" Nothing
    , Api.getApiItem (fromServer Initial)
    )



-- UPDATE


type Msg
    = FromServer FromServer
    | FromUi FromUi
    | Error String


type FromServer
    = Initial (List ItemId)
    | InitialProjects ()
    | Projects (List ElmProject)
    | NewItem Item
    | Delete ItemId
    | DeleteWork ElmProjectId ElmWorkId
    | Works ElmProjectId  (List ElmWork)


type FromUi
    = AddProjectInputChange String
    | AddProjectUnitPriceInputChange String
    | AddProjectButton
    | AddWorkButton
    | DeleteWorkButton ElmProjectId ElmWorkId
    | Done ItemId
    | SelectProject String
    | ShowSummaryButton Bool
    | WorkInputFrom String
    | WorkInputTo String
    | WorkInputNotes String
    | ClickNotes String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromServer fromServerMsg ->
            case fromServerMsg of
                Initial itemIds ->
                    ( model
                    , itemIds
                        |> List.map (\id -> getApiItemByItemId id (fromServer NewItem))
                        |> \_ -> getApiProject(fromServer Projects)
                    )

                InitialProjects _ ->
                    ( model , getApiProject(fromServer Projects) )
                Projects projects ->
                    ( { model | projects = projects } , Cmd.none )
                NewItem item ->
                    ( { model | items = Dict.insert item.id item model.items }
                    , Cmd.none
                    )

                Delete id ->
                    ( { model | items = Dict.remove id model.items }
                    , Cmd.none
                    )

                DeleteWork projectId id ->
                  let works = case Dict.get projectId model.works of
                        Just ws -> List.filter (\work -> work.workId /= Just id) ws
                        _ -> []
                      updater = \mWorks -> case mWorks of
                        Just _ -> Just works
                        _ -> Nothing
                  in
                    ( { model | works = Dict.update projectId updater model.works }
                    , Cmd.none
                    )

                Works projectId works ->
                        let updater = \_ -> Just works
                        in
                        ( { model | works = Dict.update projectId updater model.works }, Cmd.none)

        FromUi fromUi ->
            case fromUi of
                AddProjectButton ->
                    let
                        projectName = model.addProjectInput
                        projectUnitPrice = model.addProjectUnitPriceInput
                    in
                    if projectName == "" then
                        update (Error "empty field") model

                    else
                        ( { model | addProjectInput = "" }
                        , postApiProject (Project projectName projectUnitPrice) (fromServer InitialProjects)
                        )

                AddWorkButton ->
                    let
                        mProjectId = case model.currentProject of
                            Just project -> project.projectId
                            Nothing -> Nothing
                        from = case model.addWorkFromInput of
                          Nothing -> let d = ElmDay 0 0 0
                                         t = ElmTime 0 0
                                     in ElmDateTime d t
                          Just f -> f
                        to = model.addWorkToInput
                        notes = model.addWorkNotesInput
                        next = case mProjectId of
                            Nothing -> Cmd.none
                            Just projectId -> postApiWork (ElmWork Nothing projectId from to Nothing notes) (fromServer (\(works) -> Works projectId works))
                    in
                    ( model, next)

                AddProjectInputChange t ->
                    ( { model | addProjectInput = t, error = Nothing }
                    , Cmd.none
                    )

                AddProjectUnitPriceInputChange t ->
                    let unitPrice = case toInt(t) of
                            Just price -> price
                            _ -> 0
                    in
                        if unitPrice == 0 then
                            update(Error "invalid price") model
                        else
                            ( { model | addProjectUnitPriceInput = unitPrice, error = Nothing }
                            , Cmd.none
                            )

                DeleteWorkButton projectId id ->
                    ( model
                    , deleteApiWorkByElmWorkId id (fromServer (\() -> DeleteWork projectId id))
                    )

                Done id ->
                    ( model
                    , deleteApiItemByItemId id (fromServer (\() -> Delete id))
                    )
                SelectProject projectIdStr ->
                    let find projectId projects =
                            let
                                filter_ = filter byId projects
                                byId = \p -> case p.projectId of
                                   Nothing -> False
                                   Just pid -> pid == projectId
                            in filter_ |> head
                        mProjectId = toInt projectIdStr
                        maybeFetch projectId = case Dict.get projectId model.works of
                            Just [] -> getApiWorkByProjectId projectId (fromServer (Works projectId))
                            Nothing -> getApiWorkByProjectId projectId (fromServer (Works projectId))
                            Just works -> Cmd.none
                    in
                        case mProjectId of
                            Nothing -> (model, Cmd.none)
                            Just projectId ->
                                ( { model | currentProject = find projectId model.projects }, maybeFetch projectId )
                ShowSummaryButton v ->
                    ({model | showSummary = v}, Cmd.none)
                WorkInputFrom t ->
                    ({ model | addWorkFromInput = parseDate t}, "" ++ (log "debug" t) |> \_ -> Cmd.none)
                WorkInputTo t ->
                    ({ model | addWorkToInput = parseDate t}, "" ++ (log "debug" t) |> \_ -> Cmd.none)
                WorkInputNotes t -> ({ model | addWorkNotesInput = t }, Cmd.none)

                ClickNotes notes -> ({model | notes = notes}, Cmd.none)

        Error error ->
            ( { model | error = Just error }, Cmd.none )


fromServer : (a -> FromServer) -> Result Http.Error a -> Msg
fromServer msgConstructor result =
    case result of
        Ok content ->
            FromServer <| msgConstructor content

        Err error ->
            Error <| httpErrorToString error


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



-- VIEW


view : Model -> Html Msg
view model =
    let
        items =
            List.map (viewItem << Tuple.second) (Dict.toList model.items)
        projects =
            (option
                []
                [text "Please select"]) :: List.map viewProjectOpt model.projects

        error =
            model.error
                |> Maybe.map viewError
                |> Maybe.withDefault (Html.text "")

        works = case model.currentProject of
            Nothing -> []
            Just p -> case p.projectId of
                Nothing -> []
                Just pid -> case Dict.get pid model.works of
                    Nothing -> []
                    Just ws -> ws

    in
    div [ class "section" ]
        [
        div [ class "container" ]
            [ ul [] items
            , myInput [
                placeholder "Project Name",
                onInput (FromUi << AddProjectInputChange), value model.addProjectInput
                ]
            , myInput [
                type_ "number",
                placeholder "Unit Price",
                onInput (FromUi << AddProjectUnitPriceInputChange)
                ]
            , myButton [ onClick (FromUi AddProjectButton) ] [ text "Add project" ]
            , error
            , div []
                [ mySelect [ class "select", onInput (FromUi << SelectProject) ] projects ]
            , case model.currentProject of
                Nothing -> text ""
                Just project -> viewProject project model.works
            , viewNotes model.notes works
            , viewWorks model
            ]
        ]

sumWorks : List ElmWork -> List(String, Float)
sumWorks works =
    let filterByNotes notes = List.filter (byNotes notes) works
        byNotes notes work = work.notes == notes
        hours notes = List.map (\w -> Maybe.withDefault 0 w.hours) (filterByNotes notes)
        summary notes = List.foldl (+) 0 (hours notes)
        sum work = (work.notes, summary work.notes)
    in List.map sum works |> Dict.fromList |> Dict.toList

viewNotes : String -> List ElmWork -> Html Msg
viewNotes mNotes works =
    case mNotes  of
        "" -> text ""
        notes ->
            let filterByNotes = List.filter byNotes works
                byNotes work = work.notes == notes
                hours = List.map (\w -> Maybe.withDefault 0 w.hours) filterByNotes
                sum = format usLocale <| List.foldl (+) 0 hours
            in
                div [] [ text <| (++) notes <| (++) ": " <| sum ]


formControl : Html Msg -> Html Msg
formControl input = 
    div [ class "field" ]
        [
            div [ class "control" ] [ input ]
            ]

myInput : List (Html.Attribute Msg) -> Html Msg
myInput attrs =
    formControl <|
        input (attrs ++ [ class "input is-primary" ]) []

mySelect : List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
mySelect attrs options=
    formControl <|
        select (attrs ++ [ class "select is-primary" ]) options

myButton : List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
myButton attrs content =
    formControl <|
        button (attrs ++ [ class "button is-primary" ]) content

viewWorks : Model -> Html Msg
viewWorks model =
    case model.currentProject of
        Nothing -> text "Please select a project"
        Just project ->
            let maybeProjectId = project.projectId
                works = model.works
                showSummary = model.showSummary
            in
                case maybeProjectId of
                    Nothing -> text "Something wrong"
                    Just projectId ->
                        div [] [
                        myInput [
                          type_ "datetime-local", placeholder "From"
                        , onInput (FromUi << WorkInputFrom)
                        ],
                        myInput [
                          type_ "datetime-local", placeholder "To"
                        , onInput (FromUi << WorkInputTo)
                        ],
                        textarea [
                          class "textarea"
                        , placeholder "Notes"
                        , onInput (FromUi << WorkInputNotes)
                        ] [],
                        myButton [onClick (FromUi AddWorkButton)] [text "Add work"],
                        (if showSummary then
                            viewSummary projectId (Dict.get projectId works)
                        else
                            viewDetail projectId (Dict.get projectId works))
                        
                        ]


viewSummary : ElmProjectId -> Maybe (List ElmWork) -> Html Msg
viewSummary projectId maybeWorks =
    let summaries = case maybeWorks of
            Nothing -> []
            Just works ->
                let
                    sums = sumWorks works
                    show (notes, hours) = tr [] [
                        td [] [ pre [] [ text notes ] ]
                      , td [] [text (format usLocale hours)]
                        ]
                in
                    List.map show sums
    in
        div [] [
            myButton [onClick <| FromUi <| ShowSummaryButton False] [text "Show detail"]
          , table [] summaries
            ]

viewDetail : ElmProjectId -> Maybe (List ElmWork) -> Html Msg
viewDetail projectId maybeWorks = div [] [
    myButton [onClick <| FromUi <| ShowSummaryButton True] [text "Show summary"],
    table [class "table"] ([tr [] [
        th [] [text ""]
        , th [style "min-width" "150px"] [text "From"]
        , th [style "min-width" "150px"] [text "To"]
        , th [] [text "Hours"]
        , th [] [text "Notes"]
        ]] ++ viewWork projectId maybeWorks)
        ]

viewWork : ElmProjectId -> Maybe (List ElmWork) -> List (Html Msg)
viewWork projectId maybeWorks =
        case maybeWorks of
            Nothing -> []
            Just works ->
                let toLi work = tr [] [
                        case work.workId of
                          Just workId -> myButton [
                              onClick (FromUi (DeleteWorkButton projectId workId))
                            ] [ text "Delete"]
                          Nothing -> myButton [] []
                      , td [] [ text (formatDate work.elmFrom)]
                      , td [] [ text (maybeElmTo work.elmTo)]
                      , td [] [ text (maybeHours work.hours)]
                      , td [ onClick (FromUi <| ClickNotes work.notes) ] [ pre [] [ text work.notes ] ]
                      ]
                    maybeElmTo elmTo = case elmTo of
                      Nothing -> ""
                      Just to -> formatDate to
                    maybeHours hours = case hours of
                      Nothing -> ""
                      Just h -> format ({usLocale | decimals = 2}) h
                in
                map toLi works

viewItem : Item -> Html Msg
viewItem item =
    li []
        [ text item.text
        , text " - "
        , myButton [ onClick (FromUi <| Done item.id) ] [ text "done" ]
        ]

viewProjectOpt : ElmProject -> Html Msg
viewProjectOpt project =
    let projectId = case project.projectId of
            Nothing -> ""
            Just pid -> fromInt pid
    in
    option [ value projectId ]
        [ text project.projectName ]

viewProject : ElmProject -> Dict ElmProjectId (List ElmWork) -> Html Msg
viewProject project allWorks =
    let name = project.projectName
        unitPrice = project.projectUnitPrice
        unitPriceStr = format usLocale <| toFloat unitPrice
        totalHours =
            let maybeHours work = case work.hours of
                            Nothing -> 0
                            Just n -> n
                hours = List.map maybeHours works
                works = case project.projectId of
                    Nothing -> []
                    Just id -> case Dict.get id allWorks of
                        Nothing -> []
                        Just ws -> ws
            in
                List.foldl (+) 0 hours
    in div [ class "columns" ] (
        List.map (\t -> div [ class "column" ]  [ text t ])
            [
                name, unitPriceStr, format usLocale totalHours
              , format usLocale <| (*) totalHours <| toFloat unitPrice
            ]
        )


viewError : String -> Html msg
viewError error =
    div
        []
        [ text <| "Error: " ++ error ]

parseDate : String -> Maybe ElmDateTime
parseDate t =
    let datetimes = case split "T" t of
            [ dayStr, timeStr ] -> (split "-" dayStr) ++ (split ":" timeStr)
            _ -> []
    in case map toInt datetimes of
            [Just year, Just month, Just dom, Just hour, Just min ] ->
                let day = ElmDay year month dom
                    time = ElmTime hour min
                in Just (ElmDateTime day time)
            _ -> Nothing

formatDate : ElmDateTime -> String
formatDate dt = case (dt.day, dt.time) of
    (d, t) -> case [d.year, d.month, d.dom, t.hour, t.min] of
      datetimes -> case map (String.pad 2 '0' << fromInt) datetimes of
        [year, month, dom, hour, min] -> case [[year, month, dom], [hour, min]] of
          [day, time] -> (join "-" day) ++ " " ++ (join ":" time)
          _ -> ""
        _ -> ""
