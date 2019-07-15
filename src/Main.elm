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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotRepos result ->
        case result of
            Ok repoList ->
                (Success repoList, Cmd.none)

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
                |> List.map (\l -> pre [] [ text ("name:" ++ l.name ++ "\n")
                                          , text ("url:" ++ l.url)
                                          ])
                |> div []


-- HTTP

repoDecoder : Decoder (List Repo)
repoDecoder =
    field "items" (list repoUrlDecoder)

repoUrlDecoder : Decoder Repo
repoUrlDecoder =
    map2 Repo
        (field "name" string)
        (field "html_url" string)
