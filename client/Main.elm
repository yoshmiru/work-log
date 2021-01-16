module Main exposing (FromServer(..), FromUi(..), Model, Msg(..), fromServer, init, main, update, view, viewItem)

import Api exposing (..)
import Browser
import Browser.Navigation as Nav
import Debug exposing (log)
import Dict exposing (Dict)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (Html, button, div, input, label, li, option, pre, select, span, table, tr, th, td, text, textarea, ul)
import Html.Attributes exposing (class, disabled, placeholder, style, type_, value)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput)
import Http
import List exposing (filter, head, map)
import Page
import Page.Bill as Bill
import Page.Works as Works
import Route exposing (Route)
import Session exposing (Session(..))
import String exposing (fromFloat, fromInt, join, split, toInt)
import Task exposing (andThen)
import Tuple exposing (first, second)
import Url  exposing (..)

import Utils exposing (..)



main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

-- MODEL

type Model
  = Default Session Model_
  | Works_  Model Works.Model
  | Bill    Model Bill.Model
  | NotFound Session

type History
  = List Model

type alias Model_ =
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
    , onConfirm : Maybe Msg
    }

initialModel : Model_
initialModel = {
    items = Dict.empty
  , projects = []
  , addProjectInput = ""
  , addProjectUnitPriceInput = 0
  , addWorkProjectNameInput = ""
  , addWorkFromInput = Nothing
  , addWorkToInput   = Nothing
  , addWorkNotesInput = ""
  , currentProject   = Nothing
  , showSummary      = False
  , works = Dict.empty
  , notes = ""
  , error = Nothing
  , onConfirm = Nothing
  }

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    changeRouteTo
        (Route.fromUrl url)
        (Default (Guest key) initialModel
        -- , Api.getApiItem (fromServer Initial)
    )



-- UPDATE


type Msg
    = FromServer FromServer
    | FromUi FromUi
    | Error String
    | UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | GotWorksMsg Works.Msg
    | GotBillMsg Bill.Msg


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
    | Archive ElmProjectId
    | Confirm Msg
    | Cancel
    | DeleteWorkButton ElmProjectId ElmWorkId
    | Done ItemId
    | SelectProject String
    | ShowSummaryButton Bool
    | WorkInputFrom String
    | WorkInputTo String
    | WorkInputNotes String
    | ClickNotes String


toSession : Model -> Session
toSession page =
    case page of
        Default session _ ->
            session
        Works_ _ bill ->
            Works.toSession bill
        Bill _ bill ->
            Bill.toSession bill
        NotFound session ->
            session


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
        case Debug.log "route" maybeRoute of
            Nothing ->
                ( NotFound session, Cmd.none )
            Just Route.Home ->
                case model of
                    Default _ model_ ->
                        ( Default session model_, Api.getApiItem (fromServer Initial) )
                    Works_ prev model_ ->
                        ( prev, Api.getApiItem (fromServer Initial) )
                    Bill prev model_ ->
                        ( prev, Api.getApiItem (fromServer Initial) )
                    _ ->
                        ( Default session initialModel, Api.getApiItem (fromServer Initial) )
            Just (Route.Works projectId) ->
                case model of
                    Bill prev model_ ->
                        ( prev, Cmd.none )
                    _ ->
                        Works.init session projectId
                            |> updateWith (\subModel -> Works_ model subModel) GotWorksMsg model

            Just (Route.Bill projectId) ->
                case model of
                    Bill prev _ ->
                        Bill.init session projectId
                            |> updateWith (\subModel -> Bill prev subModel) GotBillMsg model
                    _ ->
                        Bill.init session projectId
                            |> updateWith (\subModel -> Bill model subModel) GotBillMsg model


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


showModel model =
    case model of
        Default _ _ -> "Default"
        Works_ _ _ -> "Works_"
        Bill _ _ -> "Bill"
        NotFound _ -> "NotFround"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update" msg of

        UrlChanged url ->
            changeRouteTo (Route.fromUrl url) model

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )
                        Just _ ->
                            (model , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url))

                Browser.External href ->
                    (model , Nav.load href)

        GotWorksMsg subMsg ->
            case model of
                Works_ prev subModel ->
                    Works.update subMsg subModel
                        |> updateWith (\subModel_ -> Works_ prev subModel_) GotWorksMsg model
                _ -> (model, Cmd.none)
        GotBillMsg subMsg ->
            case model of
                Bill prev subModel ->
                    Bill.update subMsg subModel
                        |> updateWith (\subModel_ -> Bill (Debug.log "prev" prev) subModel_) GotBillMsg model
                _ -> (model, Cmd.none)

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
                _ ->
                    case model of
                        Default session model_ ->
                            case fromServerMsg of
                                Projects projects ->
                                    ( Default session { model_ | projects = projects } , Cmd.none )
                                NewItem item ->
                                    ( Default session { model_ | items = Dict.insert item.id item model_.items } , Cmd.none)

                                Delete id ->
                                    ( Default session { model_ | items = Dict.remove id model_.items }, Cmd.none )

                                DeleteWork projectId id ->
                                    let works =
                                            case Dict.get projectId model_.works of
                                                Just ws -> List.filter (\work -> work.workId /= Just id) ws
                                                _ -> []
                                        updater = \mWorks ->
                                            case mWorks of
                                                Just _ -> Just works
                                                _ -> Nothing
                                    in
                                        ( Default session { model_ | works = Dict.update projectId updater model_.works }
                                        , Cmd.none
                                        )

                                Works projectId works ->
                                        let updater = \_ -> Just works
                                        in
                                            ( Default session { model_ | works = Dict.update projectId updater model_.works }, Cmd.none)
                                _ ->
                                    ( NotFound (toSession model), Cmd.none )
                        _ ->
                            ( NotFound (toSession model), Cmd.none )

        FromUi fromUi ->
            case model of
                Default session model_ ->
                    case fromUi of
                        AddProjectButton ->
                            let
                                projectName = model_.addProjectInput
                                projectUnitPrice = model_.addProjectUnitPriceInput
                            in
                            if projectName == "" then
                                update (Error "empty field") model

                            else
                                ( Default session { model_ | addProjectInput = "" }
                                , postApiProject (Project projectName projectUnitPrice False) (fromServer InitialProjects)
                                )

                        AddWorkButton ->
                            let
                                mProjectId = case model_.currentProject of
                                    Just project -> project.projectId
                                    Nothing -> Nothing
                                from = case model_.addWorkFromInput of
                                  Nothing -> let d = ElmDay 0 0 0
                                                 t = ElmTime 0 0
                                             in ElmDateTime d t
                                  Just f -> f
                                to = model_.addWorkToInput
                                notes = model_.addWorkNotesInput
                                next = case mProjectId of
                                    Nothing -> Cmd.none
                                    Just projectId -> postApiWork (ElmWork Nothing projectId from to Nothing notes) (fromServer (\(works) -> Works projectId works))
                            in
                            ( model, next)

                        AddProjectInputChange t ->
                            ( Default session { model_ | addProjectInput = t, error = Nothing }
                            , Cmd.none
                            )

                        AddProjectUnitPriceInputChange t ->
                            let unitPrice = case toInt(t) of
                                    Just price -> price
                                    _ -> -1
                            in
                                if unitPrice < 0 then
                                    update (Error "invalid price") model
                                else
                                    ( Default session { model_ | addProjectUnitPriceInput = unitPrice, error = Nothing }
                                    , Cmd.none
                                    )

                        Archive projectId ->
                            (Default session { model_ |
                                projects = []
                              , currentProject = Nothing}
                            , Api.deleteApiProject projectId (fromServer InitialProjects))

                        Confirm msg_ ->
                            ( Default session {model_ | onConfirm = Just msg_ }, Cmd.none )

                        Cancel ->
                            ( Default session {model_ | onConfirm = Nothing }, Cmd.none )

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
                                maybeFetch projectId = case Dict.get projectId model_.works of
                                    Just [] -> getApiWorkByProjectId projectId (fromServer (Works projectId))
                                    Nothing -> getApiWorkByProjectId projectId (fromServer (Works projectId))
                                    Just works -> Cmd.none
                            in
                                case mProjectId of
                                    Nothing -> (model, Cmd.none)
                                    Just projectId ->
                                        ( Default session { model_ | currentProject = find projectId model_.projects }, maybeFetch projectId )
                        ShowSummaryButton v ->
                            (Default session {model_ | showSummary = v}, Cmd.none)
                        WorkInputFrom t ->
                            (Default session { model_ | addWorkFromInput = parseDate t}, "" ++ (log "debug" t) |> \_ -> Cmd.none)
                        WorkInputTo t ->
                            (Default session { model_ | addWorkToInput = parseDate t}, "" ++ (log "debug" t) |> \_ -> Cmd.none)
                        WorkInputNotes t -> (Default session { model_ | addWorkNotesInput = t }, Cmd.none)

                        ClickNotes notes -> (Default session {model_ | notes = notes}, Cmd.none)
                _ ->
                    ( NotFound (toSession model), Cmd.none )

        Error error ->
            case model of
                Default session model_ ->
                    ( Default session { model_ | error = Just error }, Cmd.none )
                _ ->
                    ( NotFound (toSession model), Cmd.none )


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


view : Model -> Browser.Document Msg
view model_ =
    let viewPage toMsg config =
            let
                { title, body } =
                    Page.view config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model_ of
        NotFound session ->
            { title = "Not found", body = [ div [] [ text "Not found" ] ] }
        Works_ _ worksModel ->
            viewPage GotWorksMsg (Works.view worksModel)
        Bill _ bill ->
            viewPage GotBillMsg (Bill.view bill)
        Default _ model ->
            let
                items =
                    List.map (viewItem << Tuple.second) (Dict.toList model.items)
                projects =
                    (option
                        []
                        [text "Please select"]) :: List.map (viewProjectOpt model.currentProject) model.projects

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
                confirm
                  = let onConfirm = model.onConfirm
                        column = div [ class "column" ]
                    in  case onConfirm of
                          Nothing      -> div [] []
                          Just confirm_ -> div [ class "columns" ] [
                              column <| [ label [] [ text "Really"] ]
                            , column <| [ myButton [ onClick confirm_ ] [ text "Yes"] ]
                            , column <| [ myButton [ onClick (FromUi Cancel) ] [ text "No"] ]
                              ]

                maybeWorksLink = case model.currentProject of
                    Just project -> case project.projectId of
                        Just pId -> Html.a [ Route.href (Route.Works pId), A.class "button is-primary" ] [ Html.text "show works in new page" ]
                        _ -> Html.span [] []
                    _ -> Html.span [] []
            in
                { title = "Work log"
                , body =
                    [ div [ class "section" ]
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
                                [ mySelect [ class "select", onInput (FromUi << SelectProject) ] projects
                                , maybeWorksLink ]
                            , case model.currentProject of
                                Nothing -> text ""
                                Just project ->
                                    div
                                      []
                                      [ viewProject project model.works
                                      , confirm
                                      ]
                            , viewNotes model.notes works
                            , viewWorks model
                            ]
                        ]
                    ]
                }


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

viewWorks : Model_ -> Html Msg
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
                                myButton [onClick (FromUi AddWorkButton)] [text "Add work"]
                              , Html.a [ class "button is-primary", Route.href (Route.Bill projectId) ] [ text "Bill" ]
                              , (if showSummary then
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
          , table [class "table"] summaries
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
                        td [] [
                          case work.workId of
                          Just workId -> myButton [
                              onClick (FromUi (DeleteWorkButton projectId workId))
                            ] [ text "Delete"]
                          Nothing -> myButton [] []
                          ]
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

viewProjectOpt : Maybe ElmProject -> ElmProject -> Html Msg
viewProjectOpt currentProject project =
    let projectId = case project.projectId of
            Nothing -> ""
            Just pid -> fromInt pid
        selected =
            case currentProject of
                Just p  ->
                    case (p.projectId, project.projectId) of
                        (Just a, Just b) -> a == b
                        _ -> False
                _ -> False
    in
    option [ value projectId, A.selected selected]
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
        archiveButtonAttr = case project.projectId of
          Just id -> [ onClick (FromUi (Confirm (FromUi (Archive id)))) ]
          _       -> [ disabled True ]
        texts = List.map (\t -> div [ class "column" ]  [ text t ])
            [
                name, unitPriceStr, format usLocale totalHours
              , format usLocale <| (*) totalHours <| toFloat unitPrice
            ]
    in div [ class "columns" ] (
              texts ++ [ myButton archiveButtonAttr [ text "Archive" ]]
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
