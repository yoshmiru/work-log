module Main exposing (FromServer(..), FromUi(..), Model, Msg(..), fromServer, init, main, update, view, viewItem)

import Api exposing (..)
import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, li, option, select, span, table, tr, th, td, text, textarea, ul)
import Html.Attributes exposing (placeholder, type_, value)
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
    , works : Dict ElmProjectId (List ElmWork)
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( Model Dict.empty [] "" 0 "" Nothing Nothing "" Nothing Dict.empty Nothing
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
    | WorkInputFrom String
    | WorkInputTo String
    | WorkInputNotes String


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
                        , postApiProject (Project projectName 3000) (fromServer InitialProjects)
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
                WorkInputFrom t ->
                    ({ model | addWorkFromInput = parseDate t}, "" ++ (log "debug" t) |> \_ -> Cmd.none)
                WorkInputTo t ->
                    ({ model | addWorkToInput = parseDate t}, "" ++ (log "debug" t) |> \_ -> Cmd.none)
                WorkInputNotes t -> ({ model | addWorkNotesInput = t }, Cmd.none) 
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
            (option [] [text "Please select"]) :: List.map viewProject model.projects

        error =
            model.error
                |> Maybe.map viewError
                |> Maybe.withDefault (Html.text "")

        showProject project =  
            let name = project.projectName
                unitPrice = project.projectUnitPrice
                unitPriceStr = fromInt unitPrice
                mid = project.projectId
                totalHours =
                    let maybeHours work = case work.hours of
                            Nothing -> 0
                            Just n -> n
                        hours = List.map maybeHours works
                        works = case mid of
                            Nothing -> []
                            Just id ->case Dict.get id model.works of
                                Nothing -> []
                                Just ws -> ws
                    in
                        List.foldl (+) 0 hours
             in div [] [
                 span [] [text (name ++ " " ++ unitPriceStr)] 
               , span [] [text (fromFloat totalHours)] 
               , span [] [text " "] 
               , span [] [text
                       (fromFloat (totalHours * (toFloat unitPrice)))]
                ]
    in
    div []
        [ ul [] items
        , input [
            placeholder "Project Name",
            onInput (FromUi << AddProjectInputChange), value model.addProjectInput
            ] []
        , input [
            type_ "number",
            placeholder "Unit Price",
            onInput (FromUi << AddProjectUnitPriceInputChange)
            ] []
        , button [ onClick (FromUi AddProjectButton) ] [ text "add project" ]
        , error
        , div []
            [ select [ onInput (FromUi << SelectProject) ] projects ]
        , case model.currentProject of
            Nothing -> text ""
            Just project -> showProject project
        , viewWorks model
        ]

viewWorks : Model -> Html Msg
viewWorks model =
    case model.currentProject of
        Nothing -> text "Please select a project"
        Just project ->
            let maybeProjectId = project.projectId
                works = model.works
            in
                case maybeProjectId of
                    Nothing -> text "Something wrong"
                    Just projectId ->
                        div [] [
                        input [
                          type_ "datetime-local", placeholder "From"
                        , onInput (FromUi << WorkInputFrom)
                        ] [],
                        input [
                          type_ "datetime-local", placeholder "To"
                        , onInput (FromUi << WorkInputTo)
                        ] [],
                        textarea [
                          placeholder "Notes"
                        , onInput (FromUi << WorkInputNotes)
                        ] [],
                        button [onClick (FromUi AddWorkButton)] [text "add work"],
                        table [] ([tr [] [
                          th [] [text ""]
                        , th [] [text "From"]
                        , th [] [text "To"]
                        , th [] [text "Hours"]
                        , th [] [text "Notes"]
                          ]] ++ viewWork projectId (Dict.get projectId works))
                        ]

viewWork : ElmProjectId -> Maybe (List ElmWork) -> List (Html Msg)
viewWork projectId maybeWorks =
        case maybeWorks of
            Nothing -> []
            Just works ->
                let toLi work = tr [] [
                        case work.workId of
                          Just workId -> button [
                              onClick (FromUi (DeleteWorkButton projectId workId))
                            ] [ text "Delete"]
                          Nothing -> button [] []
                      , td [] [ text (formatDate work.elmFrom)]
                      , td [] [ text (maybeElmTo work.elmTo)]
                      , td [] [ text (maybeHours work.hours)]
                      , td [] [ text work.notes]
                      ]
                    maybeElmTo elmTo = case elmTo of
                      Nothing -> ""
                      Just to -> formatDate to
                    maybeHours hours = case hours of
                      Nothing -> ""
                      Just h -> fromFloat h
                in
                map toLi works

viewItem : Item -> Html Msg
viewItem item =
    li []
        [ text item.text
        , text " - "
        , button [ onClick (FromUi <| Done item.id) ] [ text "done" ]
        ]

viewProject : ElmProject -> Html Msg
viewProject project =
    let projectId = case project.projectId of
            Nothing -> ""
            Just pid -> fromInt pid
    in
    option [ value projectId ]
        [ text project.projectName ]


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
      datetimes -> case map fromInt datetimes of
        [year, month, dom, hour, min] -> case [[year, month, dom], [hour, min]] of
          [day, time] -> (join "-" day) ++ " " ++ (join ":" time)
          _ -> ""
        _ -> ""
