module Main exposing (main)

import Browser                           exposing (Document, UrlRequest)
import Html                              exposing (..)
import Html.Events                       exposing (..)
import Url                               exposing (Url)
import Debug
import Browser.Navigation as Nav
import Page                              exposing (Page(..))
import Page.Login         as Login
import Page.CodeProblem   as CodeProblem
import Page.NotFound      as NotFound
import Json.Decode        as Decode      exposing (Value)
import Route                             exposing (Route)
import Session                           exposing (Session, Cred, getNavKey)

-- MODEL

type alias Model =
    { route   : Route
    , page    : Page
    , session : Session
    }

init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        model = 
            { route  = Route.parseUrl url
            , page   = NotFound
            , session = Session.Unauthenticated navKey
            }
    in
    changeRouteTo (Route.parseUrl url) model -- TODO: we should read on init if we have creds stored
   

-- UPDATE

type Msg 
    = GotPageNotFound
    | GotLoginMsg       Login.Msg
    | GotCodeProblemMsg CodeProblem.Msg
    | LinkClicked       UrlRequest
    | UrlChanged        Url 



toSession : Model -> Session
toSession model =
    case model.page of
        NotFound ->
            model.session

        Login login ->
            Login.toSession login

        CodeProblem codeproblem ->
            CodeProblem.toSession codeproblem

        

changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    let
        session =
            toSession model
    in
    case route of
        Route.PageNotFound ->
            ( { model | page = NotFound } 
            , Cmd.none
            )

        Route.Login ->
            let
                (updatedPageModel, upCmd) =
                    Login.init session
            in
            ( { model | page = Login updatedPageModel}, Cmd.map GotLoginMsg upCmd)
            
                
                -- |> updateWith Login GotLoginMsg model

        Route.CodeProblem ->
            -- CodeProblem.init session
            let
                (updatedPageModel, upCmd) =
                    CodeProblem.init session
            in
            ( { model | page = CodeProblem updatedPageModel}, Cmd.map GotCodeProblemMsg upCmd)
                -- |> updateWith CodeProblem GotCodeProblemMsg model
        -- TODO: also add logout option


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ )                 ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (getNavKey model.session) (Url.toString url) 
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ )                         ->
            changeRouteTo (Route.parseUrl url) model

        ( GotPageNotFound, NotFound )                 ->
            ( { model | page = NotFound } 
            , Cmd.none
            )

        ( GotLoginMsg subMsg, Login login )           -> 
            -- Login.update subMsg login 
            --     |> updateWith Login GotLoginMsg model
            let
                (updatedPageModel, upCmd) =
                    Login.update subMsg login
            in
            ( { model | page = Login updatedPageModel}, Cmd.map GotLoginMsg upCmd)

        ( GotCodeProblemMsg subMsg, CodeProblem cp )  -> 
            -- CodeProblem.update subMsg cp 
            --     |> updateWith CodeProblem GotCodeProblemMsg model
            let
                (updatedPageModel, upCmd) =
                    CodeProblem.update subMsg cp
            in
            ( { model | page = CodeProblem updatedPageModel}, Cmd.map GotCodeProblemMsg upCmd)

        ( _, _ )                                      -> 
            ( model, Cmd.none )

updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )

-- VIEW

view : Model -> Document Msg
view model =
    let 
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view page config 
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model.page of
        NotFound                ->
            Page.view Page.NotFound NotFound.view

        Login login             ->
            viewPage (Login login) GotLoginMsg (Login.view login)

        CodeProblem codeProblem ->
            viewPage (CodeProblem codeProblem) GotCodeProblemMsg (CodeProblem.view codeProblem)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        NotFound                ->
            Sub.none |> Debug.log ("notFound")

        Login login             ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        CodeProblem codeProblem ->
            Sub.map GotCodeProblemMsg (CodeProblem.subscriptions codeProblem)
    
    
    


-- MAIN

main : Program Value Model Msg
main =
  Browser.application 
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    , onUrlRequest  = LinkClicked
    , onUrlChange   = UrlChanged
    }