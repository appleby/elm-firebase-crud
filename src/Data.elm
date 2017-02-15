module Data exposing (..)


type Frequency
    = Daily
    | Weekly
    | Monthly


type alias TaskId =
    String


type alias Task =
    { id : TaskId
    , title : String
    , tags : List String
    , freq : Frequency
    }


type alias UserId =
    String


type alias UserInfo =
    { displayName : String
    , email : String
    , photoURL : String
    , providerId : String
    , uid : UserId
    }


type alias User =
    { displayName : String
    , email : String
    , photoURL : String
    , providerId : String
    , uid : UserId
    , providerData : List UserInfo
    , emailVerified : Bool
    , isAnonymous : Bool
    , refreshToken : String
    }


emptyTask : Task
emptyTask =
    { id = "", title = "", tags = [], freq = Daily }


freqOfString : String -> Result String Frequency
freqOfString str =
    case str of
        "daily" ->
            Ok Daily

        "weekly" ->
            Ok Weekly

        "monthly" ->
            Ok Monthly

        _ ->
            Err (str ++ " is not a valid Frequency")


freqToString : Frequency -> String
freqToString freq =
    case freq of
        Daily ->
            "daily"

        Weekly ->
            "weekly"

        Monthly ->
            "monthly"
