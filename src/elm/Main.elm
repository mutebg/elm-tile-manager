module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck)


-- APP


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }


type Page
    = Login
    | Home
    | AddTile
    | EditTile
    | AddGroup
    | EditGroup


type TileType
    = Link
    | News


type TileGroupsPosition
    = Main
    | Context


type alias Tile =
    { id : Int
    , created : String
    , modified : String
    , name : String
    , image_url : String
    , type_ : TileType
    , action : String
    , active : Bool
    }


type alias TileGroup =
    { id : Int
    , created : String
    , modified : String
    , name : String
    , slug : String
    , position : TileGroupsPosition
    }


type alias TypeConnection =
    { id : Int
    , created : String
    , modified : String
    , target : Int
    , sort_order : Int
    , nav_group_id : Int
    , nav_tile_id : Int
    , store_id : Int
    }



-- MODEL


type alias Model =
    { page : Page
    , tiles : List Tile
    , currentTile : Maybe Tile
    , groups : List TileGroup
    , connections : List TypeConnection
    }


model : Model
model =
    { page = Home
    , tiles = [ emptyTile, emptyTile, emptyTile ]
    , currentTile = Just emptyTile
    , groups = []
    , connections = []
    }



-- UPDATE


type Msg
    = NoOp
    | OpenNewTile
    | SaveNewTile Tile
    | OpenExistingTile Tile
    | SaveExistingTile Tile
    | UpdateTileField String String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTileField field value ->
            let
                newTile =
                    case model.currentTile of
                        Just tile ->
                            Just (updateTile tile field value)

                        _ ->
                            model.currentTile
            in
                { model | currentTile = newTile }

        _ ->
            model


updateTile : Tile -> String -> String -> Tile
updateTile tile inputName inputValue =
    case inputName of
        "name" ->
            { tile | name = inputValue }

        "image_url" ->
            { tile | image_url = inputValue }

        "action" ->
            { tile | action = inputValue }

        "active" ->
            { tile
                | active =
                    if inputValue == "True" then
                        True
                    else
                        False
            }

        _ ->
            tile



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ case model.page of
            Home ->
                div []
                    [ listTiles model.tiles
                    , listGroups model.groups
                    ]

            _ ->
                text "No selected page"
        ]


emptyTile : Tile
emptyTile =
    { id = 0
    , created = ""
    , modified = ""
    , name = ""
    , image_url = ""
    , type_ = Link
    , action = ""
    , active = False
    }


tileForm : Tile -> Html Msg
tileForm tile =
    div []
        [ div [ class "form-group" ]
            [ label [] [ text "Name" ]
            , input [ class "form-control", type_ "text", value tile.name, onInput (UpdateTileField "name") ] []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Image URL" ]
            , input [ class "form-control", type_ "url", value tile.image_url, onInput (UpdateTileField "image_url") ] []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Action" ]
            , input [ class "form-control", type_ "text", value tile.action, onInput (UpdateTileField "action") ] []
            ]
        , div [ class "form-check" ]
            [ label [ class "form-check-label" ]
                [ input [ class "form-check-input", type_ "checkbox", checked tile.active, onCheck (\val -> UpdateTileField "active" (toString val)) ]
                    []
                , text "Active"
                ]
            ]
        ]


listTiles : List Tile -> Html Msg
listTiles tiles =
    ul [] (List.map listTileItem tiles)


listTileItem : Tile -> Html Msg
listTileItem tile =
    li []
        [ h1 [] [ text tile.name ]
        , button [] [ text "Edit" ]
        , button [] [ text "Delete" ]
        ]


listGroups : List TileGroup -> Html Msg
listGroups groups =
    ul [] (List.map listGroupItem groups)


listGroupItem : TileGroup -> Html Msg
listGroupItem group =
    li []
        [ h1 [] [ text group.name ]
        , button [] [ text "Edit" ]
        , button [] [ text "Delete" ]
        ]
