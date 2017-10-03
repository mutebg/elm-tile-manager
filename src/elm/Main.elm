module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import Types exposing (..)
import Data exposing (..)
import Views exposing (..)


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


routeParse : Url.Parser (Page -> a) a
routeParse =
    Url.oneOf
        [ Url.map Home top
        , Url.map Login (Url.s "login")
        , Url.map AddTile (Url.s "tile-add")
        , Url.map EditTile (Url.s "tile-edit" </> Url.string)
        , Url.map ConnectTile (Url.s "connect" </> Url.string </> Url.string)
        , Url.map AddGroup (Url.s "group-add")
        , Url.map EditGroup (Url.s "group-edit" </> Url.string)
        , Url.map ConnectGroup (Url.s "group-connect" </> Url.string)
        , Url.map Delete (Url.s "delete" </> Url.string </> Url.string)
        ]



-- MODEL


model : Model
model =
    { page = Home
    , tiles = []
    , currentTile = Nothing
    , groups = []
    , currentGroup = Nothing
    , connections = []
    , currentConnection = Nothing
    , user =
        { name = ""
        , pass = ""
        , token = Nothing
        , error = Nothing
        }
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            let
                newPage =
                    case model.user.token of
                        Just token ->
                            case Url.parseHash routeParse location of
                                Just page ->
                                    page

                                _ ->
                                    Home

                        Nothing ->
                            Login

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

        UpdateLoginField message ->
            let
                newUser =
                    updateUser model.user message
            in
                ( { model | user = newUser }, Cmd.none )

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

        LoginAction ->
            ( model, loginReq model.user.name model.user.pass )

        LoginResponse (Ok token) ->
            let
                user =
                    model.user

                newUser =
                    { user | token = Just token, error = Nothing }
            in
                ( { model | user = newUser }, Navigation.newUrl "#" )

        LoginResponse (Err error) ->
            let
                user =
                    model.user

                newUser =
                    { user | token = Nothing, error = Just "error" }
            in
                ( { model | user = newUser }, Cmd.none )

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


updateUser : User -> FormMessage -> User
updateUser user message =
    case message of
        TextInput "name" inputValue ->
            { user | name = inputValue }

        TextInput "pass" inputValue ->
            { user | pass = inputValue }

        TextInput "token" inputValue ->
            { user | token = Just inputValue }

        _ ->
            user


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
    if model.page == Login then
        loginPage model
    else
        div []
            [ Views.header
            , div [ class "container" ]
                [ case model.page of
                    Home ->
                        homePage model

                    AddTile ->
                        addTilePage model

                    EditTile id ->
                        addTilePage model

                    ConnectTile tile_id group_id ->
                        connectTilePage model tile_id group_id

                    AddGroup ->
                        addGroupPage model

                    EditGroup id ->
                        addGroupPage model

                    Delete itemType id ->
                        deleteModal itemType id

                    _ ->
                        h1 [] [ text "404" ]
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
