module Utils.Types exposing (..)

-- import Utils.Types exposing (..)
-- import Session exposing (..)
import Browser.Navigation as Nav
-- Code Problem 

type ProblemDifficulty 
  = Easy 
  | Intermediate 
  | Difficult 
  | Extreme

type alias CodingProblem =
  { id           : String
  , deadline     : String
  , problemTags  : List String
  , difficulty   : ProblemDifficulty
  , title        : String
  , description  : String
--   , testCases    : List String
  , templateCode : String
  }


-- User 

type alias User =
  { username : String
  , password : String
  }

type Field
  = Username
  | Password

-- Pages / Models

type Page 
    = NotFoundPage
    | LoginPage         LoginModel --Login.Model
    | CodingProblemPage CodeProblemModel --CodeProblem.Model


type alias LoginModel =
  { session : Session 
  , user    : User
  }

type Status 
  = Failure
  | Loading
  | Success CodingProblem

type alias CodeProblemModel =
    { session            : Session
    , state              : Status
    , uploadedSubmission : Maybe String -- File
    }


type alias MainModel =
    { route   : Route
    , page    : Page
    , session : Session
    }


-- Route

type Route 
    = PageNotFoundRoute
    | LoginRoute
    | CodingProblemRoute

-- Session 

type alias Cred =
    { access  : String
    , refresh : String   
    }

type Session
    = Authenticated   Nav.Key Cred
    | Unauthenticated Nav.Key

