module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck)
import Select


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


type FormMessage
    = TextInput String String
    | Checkbox String Bool
    | SelectPosition TileGroupsPosition
    | SelectTileType TileType



-- MODEL


type alias Model =
    { page : Page
    , tiles : List Tile
    , currentTile : Maybe Tile
    , groups : List TileGroup
    , currentGroup : Maybe TileGroup
    , connections : List TypeConnection
    }


model : Model
model =
    { page = AddTile
    , tiles = [ emptyTile, emptyTile, emptyTile ]
    , currentTile = Just emptyTile
    , groups = []
    , currentGroup = Just emptyGroup
    , connections = []
    }



-- UPDATE


type Msg
    = NoOp
    | OpenNewTile
    | SaveNewTile Tile
    | OpenExistingTile Tile
    | SaveExistingTile Tile
    | UpdateTileField FormMessage
    | UpdateGroupField FormMessage


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTileField message ->
            let
                newTile =
                    case model.currentTile of
                        Just tile ->
                            Just (updateTile tile message)

                        _ ->
                            model.currentTile
            in
                { model | currentTile = newTile }

        UpdateGroupField message ->
            let
                newGroup =
                    case model.currentGroup of
                        Just g ->
                            Just (updateGroup g message)

                        _ ->
                            model.currentGroup
            in
                { model | currentGroup = newGroup }

        _ ->
            model


updateTile : Tile -> FormMessage -> Tile
updateTile tile message =
    case message of
        TextInput "name" inputValue ->
            { tile | name = inputValue }

        TextInput "image_url" inputValue ->
            { tile | image_url = inputValue }

        TextInput "action" inputValue ->
            { tile | action = inputValue }

        Checkbox "active" inputValue ->
            { tile | active = inputValue }

        _ ->
            tile


updateGroup : TileGroup -> FormMessage -> TileGroup
updateGroup g message =
    case message of
        TextInput "name" inputValue ->
            { g | name = inputValue }

        TextInput "slug" inputValue ->
            { g | slug = inputValue }

        SelectPosition position ->
            { g | position = position }

        _ ->
            g



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

            AddGroup ->
                case model.currentGroup of
                    Just g ->
                        groupForm g

                    _ ->
                        text "None"

            AddTile ->
                case model.currentTile of
                    Just g ->
                        tileForm g

                    _ ->
                        text "None"

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
    let
        selectOptions =
            [ Link, News ]
    in
        div []
            [ div [ class "form-group" ]
                [ label [] [ text "Name" ]
                , input [ class "form-control", type_ "text", value tile.name, onInput (\val -> UpdateTileField (TextInput "name" val)) ] []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Image URL" ]
                , input [ class "form-control", type_ "url", value tile.image_url, onInput (\val -> UpdateTileField (TextInput "image_url" val)) ] []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Action" ]
                , input [ class "form-control", type_ "text", value tile.action, onInput (\val -> UpdateTileField (TextInput "action" val)) ] []
                ]
            , div [ class "form-check" ]
                [ label [ class "form-check-label" ]
                    [ input [ class "form-check-input", type_ "checkbox", checked tile.active, onCheck (\val -> UpdateTileField (Checkbox "active" val)) ]
                        []
                    , text "Active"
                    ]
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Type" ]
                , Select.fromSelected selectOptions (\val -> UpdateTileField (SelectTileType val)) tile.type_
                ]
            ]


emptyGroup : TileGroup
emptyGroup =
    { id = 0
    , created = ""
    , modified = ""
    , name = ""
    , slug = ""
    , position = Main
    }


groupForm : TileGroup -> Html Msg
groupForm g =
    let
        selectOptions =
            [ Main, Context ]
    in
        div []
            [ div [ class "form-group" ]
                [ label [] [ text "Name" ]
                , input [ class "form-control", type_ "text", value g.name, onInput (\val -> UpdateGroupField (TextInput "name" val)) ] []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Slug" ]
                , input [ class "form-control", type_ "text", value g.slug, onInput (\val -> UpdateGroupField (TextInput "slug" val)) ] []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Position" ]
                , Select.fromSelected selectOptions (\val -> UpdateGroupField (SelectPosition val)) g.position
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
