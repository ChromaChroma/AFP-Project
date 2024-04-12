port module Page.CodeProblem exposing (Model, Msg, init, subscriptions, update, view, openPlainTextTabPort, toSession)

import Browser
import File                         exposing (File)
import Html                         exposing (..)
import Html.Attributes              exposing (attribute, class, disabled, href, id, placeholder, value, type_)
import Html.Events                  exposing (onClick, onInput, onSubmit, on)
import Http
import Utils.Session    as Session  exposing (getCred, getNavKey)
import Utils.Types                  exposing (..)
import Utils.Transcoder             exposing (..)
import Utils.Error      as Error    exposing (..)
-- import Api                          exposing (..)


-- MODEL


type alias Model = CodeProblemModel


init : Session -> (Model, Cmd Msg)
init session = ( { session            = session
                 , state              = Loading
                 , uploadedSubmission = Nothing
                 , submissionState    = "No submission uploaded..."
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
  | GotFile             String 
  | GotSession          Session


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reload                           ->
        ( { model | state = Loading }
        , getCodeProblem 
        )

    GotCodeProblem (Ok codeProblem)   -> 
        updateState (\_ -> Success codeProblem ) model 

    GotCodeProblem (Err message)      -> 
        updateState (\_ -> Failure ) model 
        |> Debug.log (Error.errorToStr message)

    DownloadTemplate content          -> 
        ( model
        , openPlainTextTabPort content
        )

    UploadSubmission                  -> 
        case model.uploadedSubmission of
            Just file ->
                case getCred model.session of
                    Just cred ->
                        ( { model | submissionState = "Verifying implementation..." }
                        , submitFile cred.access file 
                        )   -- TODO:handle this case

                    Nothing ->
                        ( { model | submissionState = "Not logged in: Cannot upload submission!" }
                        , Cmd.none 
                        )
            
            Nothing   ->
                ( { model | submissionState = "No file to submit selected!" }
                , Cmd.none 
                )

    CompletedSubmission (Ok response) ->
        ( { model | submissionState = response }
        , Cmd.none 
        ) 

    CompletedSubmission (Err message)  ->
        ( { model | submissionState = Error.errorToStr message }
        , Cmd.none 
        ) 

    GotFile file                      -> 
        ( { model | uploadedSubmission = Just file }
        , Cmd.none
        )

    GotSession session                ->
        ( { model | session = session }
        , Cmd.none 
        )


updateState : (Status -> Status) -> Model -> ( Model, Cmd Msg )
updateState transform model =
    ( { model | state = transform model.state }
    , Cmd.none 
    )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = 
    Session.changes GotSession (getNavKey model.session)


-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title   = "CodeProblem"
    , content =
        div [ class "container.page"]
            [ viewHandler model ]
    }


viewHandler : Model -> Html Msg
viewHandler model =
    case model.state of
        Failure         ->
            viewFailedToLoad

        Loading         ->
            viewLoadingCodingProblem

        Success problem ->
            viewLoadedCodeProblem model problem


viewFailedToLoad : Html Msg
viewFailedToLoad =
    div [ class "problem-container" ]
        [ p [ class "error-message" ]
            [ text "Failed to load problem case! "
            , button [ class "reload-button", onClick Reload ]
                [ text "Reload" ]
            ]
        ]


viewLoadingCodingProblem : Html Msg
viewLoadingCodingProblem =
    div [ class "problem-container" ]
                [ p [ class "loading-message" ] [ text "Loading..." ] ]


viewLoadedCodeProblem : Model -> CodingProblem -> Html Msg
viewLoadedCodeProblem model problem = 
    div [ class "problem-container" ]
        [ h2 [ class "title" ] [ text problem.title ]
        , ul [ class "problem-tags" ] (List.map tagToHtml problem.problemTags)
        , p [ class "info-line" ]
            [ text "Deadline: "
            , span [ class "info-value" ] [ text problem.deadline ]
            ]
        , p [ class "info-line" ]
            [ text "Difficulty: "
            , span [ class "info-value" ] [ text (problemDifficultyToStr problem.difficulty) ]
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
        , div [ class "output-container"] [ text (model.submissionState) ]
        ]


tagToHtml : String -> Html Msg
tagToHtml tag =
    li [] [ text tag ]


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


-- EXPORT


toSession : Model -> Session
toSession model =
    model.session