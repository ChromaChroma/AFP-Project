port module Page.CodeProblem exposing (Model, Msg, init, subscriptions, update, view, openPlainTextTabPort, toSession)

import Browser
import File                  exposing (File)
import Html                  exposing (..)
import Html.Attributes       exposing (attribute, class, disabled, href, id, placeholder, value, type_)
import Html.Events           exposing (onClick, onInput, onSubmit, on)
import Http
import Session               exposing (getCred, getNavKey)
import Utils.Types exposing (..)
import Utils.Transcoder exposing (..)

-- MODEL

type alias Model = CodeProblemModel
-- type alias Model =
--     { session            : Session
--     , state              : Status
--     , uploadedSubmission : Maybe String -- File
--     }

-- type Status 
--   = Failure
--   | Loading
--   | Success CodingProblem

init : Session -> (Model, Cmd Msg)
-- init = (Loading, getCodeProblem)
init session = ( { session            = session
                 , state              = Loading
                 , uploadedSubmission = Nothing
                 }
               , getCodeProblem
               )


-- UPDATE

type Msg
  = GotCodeProblem      (Result Http.Error CodingProblem)
  | Reload
  | DownloadTemplate    String
  | UploadSubmission
  | CompletedSubmission (Result Http.Error String)
  | GotFile             String -- File
  | GotSession Session


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reload                           ->
        ( { model | state = Loading }
        , getCodeProblem 
        )

    GotCodeProblem (Ok codeProblem)  -> 
        updateState (\_ -> Success codeProblem ) model 
            |> Debug.log "coding problem received"

    GotCodeProblem (Err tt)          -> 
        updateState (\_ -> Failure ) model 
        |> Debug.log ("error: " ++ Debug.toString tt)

    DownloadTemplate content         -> 
        ( model
        , openPlainTextTabPort content
        )
    UploadSubmission                 -> 
        case model.uploadedSubmission of
            Just file ->
                case getCred model.session of
                    Just cred ->
                        ( model, submitFile cred.access file )   -- TODO:handle this case

                    Nothing ->
                        ( model, Cmd.none ) -- TODO should give use some feedback about not logged in
            
            Nothing   ->
                ( model, Cmd.none )

    CompletedSubmission (Ok response) ->
        ( model, Cmd.none ) 
            |> Debug.log "yes" -- TODO:handle this case

    CompletedSubmission (Err error)   ->
        ( model, Cmd.none ) |> Debug.log "err"-- TODO:handle this case

    GotFile file                      -> 
        ( { model | uploadedSubmission = Just file }
        , Cmd.none
        )

    GotSession session ->
        ( { model | session = session }
            , Cmd.none -- Route.replaceUrl (Session.navKey session) Route.Home
            ) |> Debug.log "got session have key?"

    -- GotCodeProblem (Ok codeProblem) -> (Success codeProblem, Cmd.none)
    -- GotCodeProblem (Err _) -> (Failure, Cmd.none)

updateState : (Status -> Status) -> Model -> ( Model, Cmd Msg )
updateState transform model =
    ( { model | state = transform model.state }
    , Cmd.none 
    )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Session.changes GotSession (getNavKey model.session) |> Debug.log "codeeeeeeeeeeeeeeeeeeee"

-- VIEW

view : Model -> { title : String, content : Html Msg }
view model =
    { title   = "CodeProblem"
    , content =
        div [ class "container.page"]
            [ viewCodeProblem model ]
    }

viewCodeProblem : Model -> Html Msg
viewCodeProblem model =
    case model.state of
        Failure ->
            div [ class "problem-container" ]
                [ p [ class "error-message" ]
                    [ text "Failed to load problem case! "
                    , button [ class "reload-button", onClick Reload ]
                        [ text "Reload" ]
                    ]
                ]

        Loading ->
            div [ class "problem-container" ]
                [ p [ class "loading-message" ] [ text "Loading..." ] ]

        Success problem ->
            div [ class "problem-container" ]
                [ h2 [ class "title" ] [ text problem.title ]
                , ul [ class "problem-tags" ] (List.map tagToHtml problem.problemTags)
                , p [ class "info-line" ]
                    [ text "Deadline: "
                    , span [ class "info-value" ] [ text problem.deadline ]
                    ]
                , p [ class "info-line" ]
                    [ text "Difficulty: "
                    , span [ class "info-value" ] [ text (diffToStr problem.difficulty) ]
                    ]
                , hr [] []
                , div [ class "description" ] 
                    [ text problem.description
                    , div [ class "button-group" ]
                        [ button [ class "button download-button", onClick (DownloadTemplate problem.templateCode) ]
                            [ text "Template" ]
                        ]
                    ]
                , hr [] []
                , div [ class "button-group" ]
                    [ input
                        [ type_ "file"
                        , onInput GotFile
                        ]
                        []
                    , button [ class "upload-button", onClick UploadSubmission ]
                        [ text "Submit" ]
                    ]
                ]

-- Helper functions
tagToHtml : String -> Html Msg
tagToHtml tag =
    li [] [ text tag ]

diffToStr : ProblemDifficulty -> String
diffToStr difficulty =
    case difficulty of
        Easy         ->
            "Easy"

        Intermediate ->
            "Intermediate"

        Difficult    ->
            "Difficult"

        Extreme      ->
            "Extreme"

-- fileDecoder : Decode.Decoder File
-- fileDecoder =
--   Decode.at ["target","file"] File.decoder


-- PORT
port openPlainTextTabPort : String -> Cmd msg

-- HTTP

getCodeProblem : Cmd Msg
getCodeProblem =
    Http.get
    { url = "http://localhost:8080/coding-problems/123"
    , expect = Http.expectJson GotCodeProblem codeProblemDecoder
    }






submitFile : String -> String -> Cmd Msg
submitFile access file =
    -- Http.post
    --     { body = Http.jsonBody (submissionEncoder file)
    --     , expect = Http.expectString CompletedSubmission
    --     , url = "http://localhost:8080/coding-problems/123/attempts"
    --     }


    let
        hs : List Http.Header
        hs =
            [ Http.header "Access-Control-Allow-Origin" "*",
              Http.header "Accept" "*/*",
              Http.header "Authorization" ("Bearer " ++ access)
            ]

        request =
            { method = "POST"
            , headers = hs
            , url = "http://localhost:8080/coding-problems/123/attempts"
            , body = Http.jsonBody (submissionEncoder file)
            , expect = Http.expectString CompletedSubmission
            , timeout = Nothing
            , tracker = Nothing
            }
    in
    Http.request request


toSession : Model -> Session
toSession model =
    model.session