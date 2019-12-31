module Main exposing (FromServer(..), FromUi(..), Model, Msg(..), fromServer, init, main, update, view, viewItem)

import Api exposing (..)
import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, li, option, select, text, textarea, ul)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import List exposing (filter, head)
import String exposing (fromInt, toInt)
import Task exposing (andThen)


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
    , currentProject : Maybe Project
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( Model Dict.empty [] "" 0 Nothing Nothing
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


type FromUi
    = AddProjectInputChange String
    | AddProjectUnitPriceInputChange String
    | AddProjectButton
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

                Done id ->
                    ( model
                    , deleteApiItemByItemId id (fromServer (\() -> Delete id))
                    )
                SelectProject projectName ->
                    let find projects = filter_ projects |> head
                        filter_ projects = filter byName projects
                        byName = \p -> p.projectName == projectName
                    in
                        ( { model | currentProject = find model.projects }, Cmd.none )
                WorkInputFrom t ->
                    (model, "" ++ (log "debug" t) |> \_ -> Cmd.none)
                WorkInputTo t -> (model, Cmd.none)
                WorkInputNotes t -> (model, Cmd.none)

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
            List.map viewProject model.projects

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
        , viewWork model.currentProject
        ]

viewWork : Maybe Project -> Html Msg
viewWork maybeProject =
    case maybeProject of
        Just project -> div [] [
                input [
                    type_ "time", placeholder "From",
                    onInput (FromUi << WorkInputFrom)
                ] [],
                input [type_ "time", placeholder "To"] [],
                textarea [placeholder "Notes"] []
            ]
        _ -> text "Please select a project"

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
