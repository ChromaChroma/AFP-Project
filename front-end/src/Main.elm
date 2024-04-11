module Main exposing (main)

import Browser                           exposing (Document, UrlRequest)
import Html                              exposing (..)
import Html.Events                       exposing (..)
import Url                               exposing (Url)
import Debug
import Browser.Navigation as Nav
import Page   
import Utils.Types exposing (..)
import Page.Login         as Login
import Page.CodeProblem   as CodeProblem
import Page.NotFound      as NotFound
import Json.Decode        as Decode      exposing (Value)
import Utils.Route  as Route                           
import Utils.Session as Session                          exposing ( getNavKey, decodeCred)

-- MODEL

type alias Model = MainModel
-- type alias Model =
--     { route   : Route
--     , page    : Page
--     , session : Session
--     }

init : Maybe String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init cred url navKey =
    let
        model = 
            { route  = Route.parseUrl url
            , page   = NotFoundPage
            , session = Unauthenticated navKey -- decodeCred navKey cred
            }
    in
    changeRouteTo (Route.parseUrl url) model --|> Debug.log ss -- TODO: we should read on init if we have creds stored
   

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
        NotFoundPage ->
            model.session

        LoginPage login ->
            Login.toSession login

        CodingProblemPage codeproblem ->
            CodeProblem.toSession codeproblem

        

changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    let
        session =
            toSession model
    in
    case route of
        PageNotFoundRoute ->
            ( { model | page = NotFoundPage } 
            , Cmd.none
            )

        LoginRoute ->
            let
                (updatedPageModel, upCmd) =
                    Login.init session
            in
            ( { model | page = LoginPage updatedPageModel}, Cmd.map GotLoginMsg upCmd)
            
                
                -- |> updateWith Login GotLoginMsg model

        CodingProblemRoute ->
            -- CodeProblem.init session
            let
                (updatedPageModel, upCmd) =
                    CodeProblem.init session
            in
            ( { model | page = CodingProblemPage updatedPageModel}, Cmd.map GotCodeProblemMsg upCmd)
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

        ( GotPageNotFound, NotFoundPage )                 ->
            ( { model | page = NotFoundPage } 
            , Cmd.none
            )

        ( GotLoginMsg subMsg, LoginPage login )           -> 
            -- Login.update subMsg login 
            --     |> updateWith Login GotLoginMsg model
            let
                (updatedPageModel, upCmd) =
                    Login.update subMsg login
            in
            ( { model | page = LoginPage updatedPageModel}, Cmd.map GotLoginMsg upCmd)

        ( GotCodeProblemMsg subMsg, CodingProblemPage cp )  -> 
            -- CodeProblem.update subMsg cp 
            --     |> updateWith CodeProblem GotCodeProblemMsg model
            let
                (updatedPageModel, upCmd) =
                    CodeProblem.update subMsg cp
            in
            ( { model | page = CodingProblemPage updatedPageModel}, Cmd.map GotCodeProblemMsg upCmd)

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
        NotFoundPage                ->
            Page.view NotFoundPage NotFound.view

        LoginPage login             ->
            viewPage (LoginPage login) GotLoginMsg (Login.view login)

        CodingProblemPage codeProblem ->
            viewPage (CodingProblemPage codeProblem) GotCodeProblemMsg (CodeProblem.view codeProblem)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        NotFoundPage                ->
            Sub.none

        LoginPage login             ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        CodingProblemPage codeProblem ->
            Sub.map GotCodeProblemMsg (CodeProblem.subscriptions codeProblem)
    
    
    


-- MAIN

main : Program (Maybe String) Model Msg
main =
  Browser.application 
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    , onUrlRequest  = LinkClicked
    , onUrlChange   = UrlChanged
    }