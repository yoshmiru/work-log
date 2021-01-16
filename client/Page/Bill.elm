module Page.Bill exposing (Model, Msg, init, toSession, update, view)

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
import Session exposing (Session(..))
import Utils exposing (..)


type Model
    = Initial Session ElmProjectId
    | Default Session ElmProjectId ElmProject (List ElmWork)

type Msg
    = FromServer FromServer
    | GotError String
    | Back

type FromServer
    = GotWorks (List ElmWork)
    | GotProject (Maybe ElmProject)

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
        Initial session _ -> session
        Default session _ _ _ -> session


init : Session -> ElmProjectId -> ( Model, Cmd Msg )
init session projectId =
    (Initial session projectId
    , getApiProjectByProjectId projectId (fromServer GotProject))


view : Model -> { title : String, content : H.Html Msg }
--view : Model -> Browser.Document Msg
view model =
    { title = "御請求書"
    , content = viewContent model
    }

viewContent : Model -> H.Html Msg
viewContent model =
    case model of
        Initial session _ -> H.div [] [ H.text "Loading" ]
           
        Default session _ project works ->
            let show (notes, hours) = H.tr [] [
                    H.td [] [ H.pre [] [ H.text notes ] ]
                  , H.td [] [ H.text (format usLocale hours)]
                  , H.td [] [ H.text (String.fromInt project.projectUnitPrice)]
                    ]
            in
                H.div [] [ H.div [] [ H.text "三浦屋" ]
                , H.div [] <| List.map show <| sumWorks works
                , H.button
                    [ A.class "delete"
                    , E.onClick Back
                    --, Route.href (prev)
                    ] []
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model) of
        (FromServer fromServerMsg, _) ->
            case (fromServerMsg, model) of
                (GotProject mProject, Initial session projectId) ->
                    case mProject of
                        Nothing -> (model, Cmd.none)
                        Just project ->
                            (Default session projectId project [], getApiWorkByProjectId projectId (fromServer GotWorks)) 
                (GotWorks works, Default session projectId project _)  ->
                    (Default session projectId project works, Cmd.none)
                (_, _) -> (model, Cmd.none)
        (GotError message, _) -> (model, Cmd.none)
        (Back, _) -> (model, Nav.back (Session.navKey (toSession model)) 1)
