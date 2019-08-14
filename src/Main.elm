import Browser
import Html exposing (Html, text, pre, div)
import Http
import Json.Decode exposing (Decoder, field, string, list, at, map, map3, map2)

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL

type Model
    = Failure
    | Loading
    | Success (List Repo)

type alias Repo =
    { name: String
    , url: String
    , gemlock: String
    }


init : () -> (Model, Cmd Msg)
init _ =
  let
      query = "q=user:mc-squared"
  in
  ( Loading
  , Http.get
    { url = "https://api.github.com/search/repositories?" ++ query
    , expect = Http.expectJson GotRepos repoDecoder
    }
  )


-- UPDATE

type Msg
    = GotRepos (Result Http.Error (List Repo))
    | GotGemlock (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotRepos result ->
        case result of
            Ok repoList ->
                (Success repoList, (gemlockExists repoList))

            Err _ ->
                (Failure, Cmd.none)

    GotGemlock result ->
        case result of
            Ok content ->
                (Success content, Cmd.none)

            Err _ ->
                (Failure, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "Unable to load"

        Loading ->
            text "Loading..."

        Success repoList ->
            repoList
                |> List.map (\l -> pre [] [ text ("name: " ++ l.name ++ "\n")
                                          , text ("url: " ++ l.url ++ "\n")
                                          , text ("Gemlock: " ++ l.url ++ "/Gemfile.lock - Exists? ")
                                          ])
                |> div []

        -- SuccessLock content ->
        --         pre [] [ text content ]

-- HTTP

repoDecoder : Decoder (List Repo)
repoDecoder =
    field "items" (list repoUrlDecoder)

repoUrlDecoder : Decoder Repo
repoUrlDecoder =
    map3 Repo
        (field "name" string)
        (field "html_url" string)
        (field "notrealfield" string)

isRepoChecked : Repo -> Bool
isRepoChecked repo =
    not (String.isEmpty repo.gemlock)
    -- case repo.gemlock of
    --     Just r ->
    --         True

    --     Nothing ->
    --         False
gemlockDecoder : Decoder String
gemlockDecoder =
    "Test"
    -- field "items" (list repoUrlDecoder)

checkRepoVersions : Maybe Repo -> Cmd Msg
checkRepoVersions repo =
    case repo of
        Just r ->
          Http.get
              { url = r.url ++ "/Gemfile.lock"
              , expect = Http.expectString GotGemlock gemlockDecoder
              }

        Nothing ->
            Cmd.none


gemlockExists : (List Repo) -> Cmd Msg
gemlockExists repoList =
    repoList
    |> List.filter isRepoChecked
    |> List.head
    |> checkRepoVersions
