module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onCheck, onSubmit)
import Select
import Dict
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import Button exposing (..)
import Types exposing (..)
import Data exposing (..)


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    ( model
    , Cmd.batch [ loadTiles, loadGroups, loadConnections ]
    )


type Page
    = Login
    | Home
    | AddTile
    | EditTile String
    | ConnectTile String String
    | AddGroup
    | EditGroup String
    | ConnectGroup String
    | Delete String String


routeParse : Url.Parser (Page -> a) a
routeParse =
    Url.oneOf
        [ Url.map Home top
        , Url.map AddTile (Url.s "tile-add")
        , Url.map EditTile (Url.s "tile-edit" </> Url.string)
        , Url.map ConnectTile (Url.s "connect" </> Url.string </> Url.string)
        , Url.map AddGroup (Url.s "group-add")
        , Url.map EditGroup (Url.s "group-edit" </> Url.string)
        , Url.map ConnectGroup (Url.s "group-connect" </> Url.string)
        , Url.map Delete (Url.s "delete" </> Url.string </> Url.string)
        ]



-- MODEL


type alias Model =
    { page : Page
    , tiles : List Tile
    , currentTile : Maybe Tile
    , groups : List TileGroup
    , currentGroup : Maybe TileGroup
    , connections : List TileConnection
    , currentConnection : Maybe TileConnection
    }


model : Model
model =
    { page = Home
    , tiles = []
    , currentTile = Nothing
    , groups = []
    , currentGroup = Nothing
    , connections = []
    , currentConnection = Nothing
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            let
                newPage =
                    case Url.parseHash routeParse location of
                        Just page ->
                            page

                        _ ->
                            Home

                newModel =
                    case newPage of
                        AddTile ->
                            { model | currentTile = Just emptyTile }

                        EditTile id ->
                            { model
                                | currentTile =
                                    model.tiles
                                        |> List.filter (\t -> t.id == id)
                                        |> List.head
                            }

                        ConnectTile tile_id group_id ->
                            let
                                conn =
                                    { emptyConnection | nav_tile_id = tile_id, nav_group_id = group_id }
                            in
                                { model | currentConnection = Just conn }

                        AddGroup ->
                            { model | currentGroup = Just emptyGroup }

                        EditGroup id ->
                            { model
                                | currentGroup =
                                    model.groups
                                        |> List.filter (\t -> t.id == id)
                                        |> List.head
                            }

                        _ ->
                            model

                msgs =
                    case newPage of
                        Home ->
                            [ loadTiles, loadGroups, loadConnections ]

                        _ ->
                            [ Cmd.none ]
            in
                ( { newModel | page = newPage }
                , Cmd.batch msgs
                )

        UpdateTileField message ->
            let
                newTile =
                    case model.currentTile of
                        Just tile ->
                            Just (updateTile tile message)

                        _ ->
                            model.currentTile
            in
                ( { model | currentTile = newTile }, Cmd.none )

        UpdateGroupField message ->
            let
                newGroup =
                    case model.currentGroup of
                        Just g ->
                            Just (updateGroup g message)

                        _ ->
                            model.currentGroup
            in
                ( { model | currentGroup = newGroup }
                , Cmd.none
                )

        UpdateConnectionField message ->
            let
                newConn =
                    case model.currentConnection of
                        Just g ->
                            Just (updateConnection g message)

                        _ ->
                            model.currentConnection
            in
                ( { model | currentConnection = newConn }
                , Cmd.none
                )

        LoadTiles (Ok tiles) ->
            ( { model | tiles = tiles }
            , Cmd.none
            )

        LoadGroups (Ok groups) ->
            ( { model | groups = groups }
            , Cmd.none
            )

        LoadConnections (Ok conns) ->
            ( { model | connections = conns }
            , Cmd.none
            )

        SaveTile tile ->
            ( model, saveTile tile )

        SaveGroup g ->
            ( model, saveGroup g )

        SaveConnection c ->
            ( model, saveConnection c )

        DeleteItem item id ->
            ( model, deleteItem item id )

        ReqSaveTile (Ok tile) ->
            ( model, Navigation.newUrl "#" )

        ReqSaveTile (Err error) ->
            ( model, Debug.log (toString error) Cmd.none )

        ReqSaveGroup (Ok group) ->
            ( model, Navigation.newUrl "#" )

        ReqSaveConnection (Ok conn) ->
            let
                newConn =
                    case model.page of
                        ConnectTile tile_id group_id ->
                            { emptyConnection | nav_tile_id = tile_id, nav_group_id = group_id }

                        _ ->
                            emptyConnection
            in
                ( { model | currentConnection = Just newConn, connections = conn :: model.connections }, Cmd.none )

        ReqSaveConnection (Err error) ->
            ( model, Debug.log (toString error) Cmd.none )

        ReqDelete itemType (Ok msg) ->
            ( model, Navigation.newUrl "#" )

        ReqDelete itemType (Err error) ->
            ( model, Cmd.none )

        _ ->
            ( model
            , Cmd.none
            )


updateTile : Tile -> FormMessage -> Tile
updateTile tile message =
    case message of
        TextInput "name" inputValue ->
            { tile | name = inputValue }

        TextInput "image_url" inputValue ->
            { tile | image_url = inputValue }

        TextInput "action" inputValue ->
            { tile | action = inputValue }

        SelectTileType type_ ->
            { tile | type_ = type_ }

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


updateConnection : TileConnection -> FormMessage -> TileConnection
updateConnection g message =
    case message of
        TextInput "nav_group_id" inputValue ->
            { g | nav_group_id = inputValue }

        TextInput "nav_tile_id" inputValue ->
            { g | nav_tile_id = inputValue }

        TextInput "target" inputValue ->
            { g | target = Result.withDefault 0 (String.toInt inputValue) }

        TextInput "sort_order" inputValue ->
            { g | sort_order = Result.withDefault 0 (String.toInt inputValue) }

        _ ->
            g



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header
        , div [ class "container" ]
            [ case model.page of
                Home ->
                    div [ class "row" ]
                        [ div [ class "col-md-8" ]
                            [ h2 [] [ text "Tiles" ]
                            , listTiles model.tiles
                            ]
                        , div [ class "col-md-4" ]
                            [ h2 [] [ text "Pages" ]
                            , listGroups model.groups
                            ]
                        ]

                AddTile ->
                    case model.currentTile of
                        Just g ->
                            tileForm g

                        _ ->
                            text "None"

                EditTile id ->
                    case model.currentTile of
                        Just g ->
                            tileForm g

                        _ ->
                            text "None"

                ConnectTile tile_id group_id ->
                    let
                        tileConns =
                            getTileConns tile_id model.connections

                        groupConns =
                            getGroupConns group_id model.connections

                        groupsDict =
                            groupsToDict model.groups

                        tilesDict =
                            tilesToDict model.tiles

                        tilesList =
                            tileConns
                                |> List.filter (\c -> Dict.get c.nav_group_id groupsDict /= Nothing)
                                |> List.map
                                    (\c ->
                                        case Dict.get c.nav_group_id groupsDict of
                                            Just d ->
                                                tr []
                                                    [ td [] [ text <| showTarget c.target ]
                                                    , td [] [ text <| toString c.sort_order ]
                                                    , td [] [ text <| toString d.position ]
                                                    , td [] [ text <| toString d.name ]
                                                    , td [] [ linkRemove <| "#delete/connections/" ++ c.id ]
                                                    ]

                                            _ ->
                                                text ""
                                    )

                        grpupList =
                            groupConns
                                |> List.filter (\c -> Dict.get c.nav_tile_id tilesDict /= Nothing)
                                |> List.map
                                    (\c ->
                                        case Dict.get c.nav_tile_id tilesDict of
                                            Just d ->
                                                tr []
                                                    [ td [] [ text <| showTarget c.target ]
                                                    , td [] [ text <| toString c.sort_order ]
                                                      --, td [] [ text <| toString d.position ]
                                                    , td [] [ text <| toString d.name ]
                                                    , td [] [ linkRemove <| "#delete/connections/" ++ c.id ]
                                                    ]

                                            _ ->
                                                text ""
                                    )
                    in
                        div [ class "row" ]
                            [ div [ class "col-md-8" ]
                                [ h2 [] [ text "Connections" ]
                                , table [ class "table table-striped table-bordered table-hover" ]
                                    (if group_id == "-" then
                                        [ thead []
                                            [ tr []
                                                [ th [] [ text "Target" ]
                                                , th [] [ text "Sort" ]
                                                , th [] [ text "Position" ]
                                                , th [] [ text "Name" ]
                                                , th [ style [ ( "width", "100px" ) ] ] [ text "Action" ]
                                                ]
                                            ]
                                        , tbody []
                                            tilesList
                                        ]
                                     else
                                        [ thead []
                                            [ tr []
                                                [ th [] [ text "Target" ]
                                                , th [] [ text "Sort" ]
                                                , th [] [ text "Name" ]
                                                , th [ style [ ( "width", "100px" ) ] ] [ text "Action" ]
                                                ]
                                            ]
                                        , tbody []
                                            grpupList
                                        ]
                                    )
                                ]
                            , div
                                [ class "col-md-4" ]
                                [ case model.currentConnection of
                                    Just conn ->
                                        connectionForm model.tiles model.groups conn

                                    _ ->
                                        text ""
                                ]
                            ]

                AddGroup ->
                    case model.currentGroup of
                        Just g ->
                            groupForm g

                        _ ->
                            text "None"

                EditGroup id ->
                    case model.currentGroup of
                        Just g ->
                            groupForm g

                        _ ->
                            text "None"

                Delete itemType id ->
                    deleteModal itemType id

                _ ->
                    text "No selected page"
            ]
        ]


emptyTile : Tile
emptyTile =
    { id = ""
    , created = ""
    , modified = ""
    , name = ""
    , image_url = ""
    , type_ = Link
    , action = ""
    , active = False
    }


emptyGroup : TileGroup
emptyGroup =
    { id = ""
    , created = ""
    , modified = ""
    , name = ""
    , slug = ""
    , position = Main
    }


emptyConnection : TileConnection
emptyConnection =
    { id = ""
    , created = ""
    , modified = ""
    , target = 1
    , sort_order = 0
    , nav_group_id = ""
    , nav_tile_id = ""
    , store_id = 0
    }


header : Html Msg
header =
    nav [ class "navbar navbar-default" ]
        [ div [ class "container-fluid" ]
            [ a [ class "navbar-brand", href "#" ]
                [ text "Dashboard" ]
            , ul [ class "nav navbar-nav" ]
                [ li [] [ a [ href "#" ] [ text "Home" ] ]
                , li [] [ a [ href "#tile-add" ] [ text "Add tile" ] ]
                , li [] [ a [ href "#group-add" ] [ text "Add group" ] ]
                ]
            ]
        ]


deleteModal : String -> String -> Html Msg
deleteModal itemType id =
    div [ class "remove-dialog" ]
        [ h1 [] [ text "Are you sure?" ]
        , div [ class "btn-group" ]
            [ linkCancelRemove "#"
            , btnConfirmRemove (DeleteItem itemType id)
            ]
        ]


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
            , div []
                [ button [ onClick (SaveTile tile), class "btn btn-primary" ] [ text "Save" ]
                ]
            ]


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
            , div []
                [ button [ onClick (SaveGroup g), class "btn btn-primary" ] [ text "Save" ]
                ]
            ]


connectionForm : List Tile -> List TileGroup -> TileConnection -> Html Msg
connectionForm tiles groups conn =
    let
        targets =
            [ ( "1", "Desktop" ), ( "2", "Mobile" ) ]

        tilesTuple =
            List.map (\t -> ( t.id, t.name )) tiles

        tileValues =
            if conn.nav_tile_id == "" || conn.nav_tile_id == "-" then
                ( "", "---" ) :: tilesTuple
            else
                tilesTuple

        groupTuple =
            List.map (\v -> ( v.id, v.name )) groups

        groupValues =
            if conn.nav_group_id == "" || conn.nav_group_id == "-" then
                ( "", "---" ) :: groupTuple
            else
                groupTuple
    in
        div []
            [ h2 [] [ text "Add connection" ]
            , Html.form [ onSubmit (SaveConnection conn), class "br" ]
                [ div [ class "form-group" ]
                    [ label [] [ text "Tiles" ]
                    , select
                        [ onInput (\val -> UpdateConnectionField (TextInput "nav_tile_id" val))
                        , required True
                        ]
                        (tupleToSelectOptions tileValues conn.nav_tile_id)
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Groups" ]
                    , select
                        [ onInput (\val -> UpdateConnectionField (TextInput "nav_group_id" val))
                        , required True
                        ]
                        (tupleToSelectOptions groupValues conn.nav_group_id)
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Sort order" ]
                    , input
                        [ class "form-control"
                        , type_ "number"
                        , value <| toString conn.sort_order
                        , onInput (\val -> UpdateConnectionField (TextInput "sort_order" val))
                        ]
                        []
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Target" ]
                    , select
                        [ onInput (\val -> UpdateConnectionField (TextInput "target" val))
                        ]
                        (tupleToSelectOptions targets (toString conn.target))
                    ]
                , div []
                    [ button [ class "btn btn-primary" ] [ text "Save" ]
                    ]
                ]
            ]


listTiles : List Tile -> Html Msg
listTiles tiles =
    ul [ class "list-group" ] (List.map listTileItem tiles)


listTileItem : Tile -> Html Msg
listTileItem tile =
    li [ class "list-group-item" ]
        [ img [ class "img-rounded", src tile.image_url ] []
        , h4 [] [ text tile.name ]
        , div [ class "btn-group" ]
            [ linkEdit <| "#tile-edit/" ++ tile.id
            , linkConnect <| "#connect/" ++ tile.id ++ "/-"
            , linkRemove <| "#delete/tiles/" ++ tile.id
            ]
        ]


listGroups : List TileGroup -> Html Msg
listGroups groups =
    ul [ class "list-group" ] (List.map listGroupItem groups)


listGroupItem : TileGroup -> Html Msg
listGroupItem group =
    li [ class "list-group-item" ]
        [ h4 [] [ text group.name ]
        , div [ class "btn-group" ]
            [ linkEdit <| "#group-edit/" ++ group.id
            , linkConnect <| "#connect/-/" ++ group.id
            , linkRemove <| "#delete/groups/" ++ group.id
            ]
        ]


getTileConns : String -> List TileConnection -> List TileConnection
getTileConns tileId conns =
    conns
        |> List.filter (\c -> c.nav_tile_id == tileId)


getGroupConns : String -> List TileConnection -> List TileConnection
getGroupConns group_id conns =
    conns
        |> List.filter (\c -> c.nav_group_id == group_id)


groupsToDict : List TileGroup -> Dict.Dict String TileGroup
groupsToDict groups =
    groups
        |> List.map (\k -> ( k.id, k ))
        |> Dict.fromList


tilesToDict : List Tile -> Dict.Dict String Tile
tilesToDict tiles =
    tiles
        |> List.map (\k -> ( k.id, k ))
        |> Dict.fromList


tupleToSelectOptions : List ( String, String ) -> String -> List (Html Msg)
tupleToSelectOptions values selectedValue =
    values
        |> List.map (\z -> option [ value <| Tuple.first z, selected (selectedValue == (Tuple.first z)) ] [ text <| Tuple.second z ])
