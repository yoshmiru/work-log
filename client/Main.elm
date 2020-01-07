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
    , projects : List Project
    , addProjectInput : String
    , addProjectUnitPriceInput : Int
    , addWorkProjectNameInput : String
    , addWorkFromInput : Maybe ElmDateTime
    , addWorkToInput : Maybe ElmDateTime
    , addWorkNotesInput : String
    , currentProject : Maybe Project
    , works : Dict String (List ElmWork)
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
    | Projects (List Project)
    | NewItem Item
    | Delete ItemId
    | DeleteWork String ElmWorkId
    | PutWork String ElmWork
    | Works String  (List ElmWork)


type FromUi
    = AddProjectInputChange String
    | AddProjectUnitPriceInputChange String
    | AddProjectButton
    | AddWorkButton
    | DeleteWorkButton String ElmWorkId
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

                DeleteWork projectName id ->
                  let works = case Dict.get projectName model.works of
                        Just ws -> List.filter (\work -> work.workId /= Just id) ws
                        _ -> []
                      updater = \mWorks -> case mWorks of
                        Just _ -> Just works
                        _ -> Nothing
                  in
                    ( { model | works = Dict.update projectName updater model.works }
                    , Cmd.none
                    )

                PutWork projectName work ->
                    ( { model | works =
                        let updater = \mCurrent -> case mCurrent of
                                Just current -> Just (work::current)
                                Nothing -> Nothing
                        in Dict.update projectName updater model.works 
                    }
                    , Cmd.none
                    )
                Works projectName works ->
                        let updater = \_ -> Just works
                        in 
                        ( { model | works = Dict.update projectName updater model.works }, Cmd.none)

        FromUi fromUi ->
            case fromUi of
                AddProjectButton ->
                    let
                        projectName =
                            model.addProjectInput
                    in
                    if projectName == "" then
                        update (Error "empty field") model

                    else
                        ( { model | addProjectInput = "" }
                        , postApiProject (Project projectName 3000) (fromServer InitialProjects)
                        )

                AddWorkButton ->
                    let
                        projectName = case model.currentProject of
                            Just project -> project.projectName
                            Nothing -> ""
                        from = case model.addWorkFromInput of
                          Nothing -> let d = ElmDay 0 0 0
                                         t = ElmTime 0 0
                                     in ElmDateTime d t
                          Just f -> f
                        to = model.addWorkToInput
                        notes = model.addWorkNotesInput
                    in
                    ( model, postApiWork (ElmWork Nothing projectName from to Nothing notes) (fromServer (\(works) -> Works projectName works)))

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

                DeleteWorkButton projectName id ->
                    ( model
                    , deleteApiWorkByElmWorkId id (fromServer (\() -> DeleteWork projectName id))
                    )

                Done id ->
                    ( model
                    , deleteApiItemByItemId id (fromServer (\() -> Delete id))
                    )
                SelectProject projectName ->
                    let find projects = filter_ projects |> head
                        filter_ projects = filter byName projects
                        byName = \p -> p.projectName == projectName
                        maybeFetch = case Dict.get projectName model.works of
                            Just [] -> getApiWorkByProjectName projectName (fromServer (Works projectName))
                            Nothing -> getApiWorkByProjectName projectName (fromServer (Works projectName))
                            Just works -> Cmd.none
                    in
                        ( { model | currentProject = find model.projects }, maybeFetch )
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
        projectName = case model.currentProject of
            Just project -> project.projectName
            _ -> ""
        projectUnitPrice = case model.currentProject of
            Just project -> fromInt(project.projectUnitPrice)
            _ -> ""

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
        , div [] [text (projectName ++ " " ++ projectUnitPrice)] 
        , viewWorks model
        ]

viewWorks : Model -> Html Msg
viewWorks model =
    case model.currentProject of
        Just project -> let projectName = project.projectName
                            works = model.works
            in
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
                  ]] ++ viewWork projectName (Dict.get projectName works))
            ]
        _ -> text "Please select a project"

viewWork : String -> Maybe (List ElmWork) -> List (Html Msg)
viewWork projectName maybeWorks =
    case maybeWorks of
        Just works ->
            let toLi work = tr [] [
                    case work.workId of
                      Just workId -> button [
                          onClick (FromUi (DeleteWorkButton projectName workId))
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
        _ -> []

viewItem : Item -> Html Msg
viewItem item =
    li []
        [ text item.text
        , text " - "
        , button [ onClick (FromUi <| Done item.id) ] [ text "done" ]
        ]

viewProject : Project -> Html Msg
viewProject project =
    option [ value project.projectName ]
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
