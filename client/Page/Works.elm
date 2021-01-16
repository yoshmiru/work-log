module Page.Works exposing (Model, Msg, init, toSession, update, view)

import Browser
import Browser.Navigation as Nav
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html as H
import Html.Attributes as A
import Html.Events as E
import Http

import Api exposing (..)
import Route exposing (Route)
import Session exposing (Session)
import Utils exposing (..)


type Model
    = Default Session ElmProjectId (List ElmWork)
    | Error Session String


type Msg
  = Init
  | Back
  | FromUi FromUi
  | FromServer FromServer
  | GotError String

type FromUi
  = ClickNotes String
  | DeleteWorkButton ElmProjectId ElmWorkId

type FromServer
  = GotWorks (List ElmWork)


fromServer : (a -> FromServer) -> Result Http.Error a -> Msg
fromServer msgConstructor result =
    case result of
        Ok content ->
            FromServer <| msgConstructor content

        Err error ->
            GotError <| httpErrorToString error


toSession : Model -> Session
toSession model =
    case model of
        Default session _ _ ->
          session
        Error session _ ->
          session


init : Session -> ElmProjectId -> ( Model, Cmd Msg )
init session projectId =
    (Default session projectId [], getApiWorkByProjectId projectId (fromServer GotWorks))


view : Model -> { title : String, content : H.Html Msg }
view model =
    { title = "Works"
    , content = viewContent model
    }



viewContent : Model -> H.Html Msg
viewContent model =
    case model of
        Default session projectId works ->
            H.div [ A.class "container" ] [ H.div [] [ H.text "三浦屋" ]
            , H.div [] 
                [ H.a
                    [ A.class "delete"
                    --, E.onClick Back
                    , Route.href Route.Home
                    ] []
                , H.a
                    [ Route.href (Route.Bill projectId)
                    , A.class "button is-primary"
                    ] [ H.text "Bill" ]
                ]
            , H.table [ A.class "table" ] ([H.tr [] [
                  H.th [] [H.text ""]
                , H.th [A.style "min-width" "150px"] [H.text "From"]
                , H.th [A.style "min-width" "150px"] [H.text "To"]
                , H.th [] [H.text "Hours"]
                , H.th [] [H.text "Notes"]
                ] ] ++ (viewWork projectId works))
            ]
        Error session message -> H.div [ A.class "error" ] [ H.text message ]


viewWork : ElmProjectId -> List ElmWork -> List (H.Html Msg)
viewWork projectId works =
    let toLi work = H.tr [] [
            H.td [] [
              case work.workId of
                  Just workId -> H.button [
                      E.onClick (FromUi (DeleteWorkButton projectId workId))
                    , A.class "button is-primary" 
                    ] [ H.text "Delete"]
                  Nothing -> H.button [ A.class "button is-primary" ] []
              ]
          , H.td [] [ H.text (formatDate work.elmFrom)]
          , H.td [] [ H.text (maybeElmTo work.elmTo)]
          , H.td [] [ H.text (maybeHours work.hours)]
          , H.td [ E.onClick (FromUi <| ClickNotes work.notes) ] [ H.pre [] [ H.text work.notes ] ]
          ]
        maybeElmTo elmTo = case elmTo of
          Nothing -> ""
          Just to -> formatDate to
        maybeHours hours = case hours of
          Nothing -> ""
          Just h -> format ({usLocale | decimals = 2}) h
    in
    List.map toLi works

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let session = toSession model
    in
    case msg of
        Init -> (model, Cmd.none)
        Back -> (model, Nav.back (Session.navKey session) 1)
        GotError message -> (Error session message, Cmd.none)
        FromUi fromUiMsg ->
            case fromUiMsg of
                ClickNotes notes ->
                   (model, Cmd.none)
                DeleteWorkButton pId wId ->
                   (model, Cmd.none)
        FromServer fromServerMsg ->
            case fromServerMsg of
                GotWorks works ->
                   case model of
                       Default _ pId _ ->
                           (Default session pId works, Cmd.none)
                       _ ->
                           (model, Cmd.none)
