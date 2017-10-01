module Data exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as DecodePipe
import Json.Encode as Encode
import Http
import Types exposing (..)


apiBase : String
apiBase =
    --"https://us-central1-tilesmanager-70e8d.cloudfunctions.net/api/"
    "http://localhost:5000/tilesmanager-70e8d/us-central1/api/"


tilesDecoder : Decode.Decoder (List Tile)
tilesDecoder =
    Decode.list tileDecoder


tileDecoder : Decode.Decoder Tile
tileDecoder =
    DecodePipe.decode Tile
        |> DecodePipe.required "id" Decode.string
        |> DecodePipe.required "created" Decode.string
        |> DecodePipe.required "modified" Decode.string
        |> DecodePipe.required "name" Decode.string
        |> DecodePipe.required "image_url" Decode.string
        |> DecodePipe.required "type_" tileTypeDecoder
        |> DecodePipe.required "action" Decode.string
        |> DecodePipe.required "active" Decode.bool


tileEncoder : Tile -> Encode.Value
tileEncoder tile =
    let
        attributes =
            [ ( "id", Encode.string tile.id )
            , ( "name", Encode.string tile.name )
            , ( "image_url", Encode.string tile.image_url )
            , ( "type_", Encode.string <| toString tile.type_ )
            , ( "action", Encode.string tile.action )
            , ( "active", Encode.bool tile.active )
            ]
    in
        Encode.object attributes


tileTypeDecoder : Decode.Decoder TileType
tileTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Link" ->
                        Decode.succeed Link

                    "News" ->
                        Decode.succeed News

                    _ ->
                        Decode.fail <| "Unknown type"
            )


groupsDecoder : Decode.Decoder (List TileGroup)
groupsDecoder =
    Decode.list groupDecoder


groupDecoder : Decode.Decoder TileGroup
groupDecoder =
    DecodePipe.decode TileGroup
        |> DecodePipe.required "id" Decode.string
        |> DecodePipe.required "created" Decode.string
        |> DecodePipe.required "modified" Decode.string
        |> DecodePipe.required "name" Decode.string
        |> DecodePipe.required "slug" Decode.string
        |> DecodePipe.required "position" groupPositionDecoder


groupPositionDecoder : Decode.Decoder TileGroupsPosition
groupPositionDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Main" ->
                        Decode.succeed Main

                    "Context" ->
                        Decode.succeed Context

                    _ ->
                        Decode.fail <| "Unknown type"
            )


groupEncoder : TileGroup -> Encode.Value
groupEncoder g =
    let
        attributes =
            [ ( "id", Encode.string g.id )
            , ( "name", Encode.string g.name )
            , ( "slug", Encode.string g.slug )
            , ( "position", Encode.string <| toString g.position )
            ]
    in
        Encode.object attributes


connectionsDecoder : Decode.Decoder (List TileConnection)
connectionsDecoder =
    Decode.list connectionDecoder


connectionDecoder : Decode.Decoder TileConnection
connectionDecoder =
    DecodePipe.decode TileConnection
        |> DecodePipe.required "id" Decode.string
        |> DecodePipe.required "created" Decode.string
        |> DecodePipe.required "modified" Decode.string
        |> DecodePipe.required "target" Decode.int
        |> DecodePipe.required "sort_order" Decode.int
        |> DecodePipe.required "nav_group_id" Decode.string
        |> DecodePipe.required "nav_tile_id" Decode.string
        |> DecodePipe.required "store_id" Decode.int


connectionEncoder : TileConnection -> Encode.Value
connectionEncoder g =
    let
        attributes =
            [ ( "id", Encode.string g.id )
            , ( "target", Encode.int g.target )
            , ( "sort_order", Encode.int g.sort_order )
            , ( "nav_group_id", Encode.string g.nav_group_id )
            , ( "nav_tile_id", Encode.string g.nav_tile_id )
            , ( "store_id", Encode.int g.store_id )
            ]
    in
        Encode.object attributes


messageDecoder : Decode.Decoder MessageResponse
messageDecoder =
    DecodePipe.decode MessageResponse
        |> DecodePipe.required "code" Decode.int
        |> DecodePipe.required "message" Decode.string


loadTiles : Cmd Msg
loadTiles =
    let
        url =
            apiBase ++ "tiles"

        request =
            Http.get url tilesDecoder
    in
        Http.send LoadTiles request


loadGroups : Cmd Msg
loadGroups =
    let
        url =
            apiBase ++ "groups"

        request =
            Http.get url groupsDecoder
    in
        Http.send LoadGroups request


loadConnections : Cmd Msg
loadConnections =
    let
        url =
            apiBase ++ "connections"

        request =
            Http.get url connectionsDecoder
    in
        Http.send LoadConnections request


saveTile : Tile -> Cmd Msg
saveTile tile =
    let
        url =
            apiBase ++ "tiles"

        body =
            tileEncoder tile |> Http.jsonBody

        request =
            Http.post url body tileDecoder
    in
        Http.send ReqSaveTile request


saveGroup : TileGroup -> Cmd Msg
saveGroup g =
    let
        url =
            apiBase ++ "groups"

        body =
            groupEncoder g |> Http.jsonBody

        request =
            Http.post url body groupDecoder
    in
        Http.send ReqSaveGroup request


saveConnection : TileConnection -> Cmd Msg
saveConnection g =
    let
        url =
            apiBase ++ "connections"

        body =
            connectionEncoder g |> Http.jsonBody

        request =
            Http.post url body connectionDecoder
    in
        Http.send ReqSaveConnection request


deleteItem : String -> String -> Cmd Msg
deleteItem itemType id =
    let
        url =
            apiBase ++ itemType ++ "/" ++ id

        req =
            Http.request
                { method = "DELETE"
                , body = Http.emptyBody
                , url = url
                , expect = Http.expectJson messageDecoder
                , headers = []
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send (\res -> ReqDelete itemType res) req
