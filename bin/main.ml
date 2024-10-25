open Async
open Base
module Log = Dolog.Log

let show_game_state state =
  let open Lib.Game in
  let open Core in
  let room_names = List.map state.rooms ~f:(fun r -> r.name) in
  let player_names = List.map state.players ~f:(fun p -> p.name) in
  Log.debug
    "Rooms: %s\nPlayers: %s\n"
    (String.concat ~sep:", " room_names)
    (String.concat ~sep:", " player_names)
;;

let handle_connection
  (reader : Reader.t)
  (writer : Writer.t)
  (state : Lib.Game.game_state)
  =
  let echo message = Writer.write_line writer message in
  Writer.write_line writer "Welcome to the game!\nWho are you? ";
  Reader.read_line reader
  >>= function
  | `Eof -> return ()
  | `Ok raw_name ->
    let player_name = raw_name |> String.lowercase |> String.capitalize in
    Log.info "%s connected.\n" player_name;
    (* TODO: implement a way to create a player etc *)
    state.players <- Lib.Game.add_player state player_name echo;
    show_game_state state;
    (* Force a look so the player sees the room their in upon logging in *)
    Lib.Commands.handle_look state player_name;
    let rec game_loop (state : Lib.Game.game_state) =
      Writer.write_line writer "> ";
      Reader.read_line reader
      >>= function
      | `Eof ->
        Log.info "%s disconnected.\n" player_name;
        (* Remove logged out players from the state *)
        let remaining =
          List.filter state.players ~f:(fun p -> not (String.equal p.name player_name))
        in
        state.players <- remaining;
        show_game_state state;
        return ()
      | `Ok input_line ->
        let open Lib.Commands in
        let new_state = parse_command input_line |> execute_command state player_name in
        game_loop new_state (* Continue the game loop *)
    in
    game_loop state
;;

let start_server port initial_state =
  let%bind _server =
    Tcp.Server.create
      ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.of_port port)
      (fun address reader writer ->
         Log.info "Client connected from %s\n" (Socket.Address.Inet.to_string address);
         handle_connection reader writer initial_state)
  in
  Log.info "Server listening on port %d\n" port;
  return ()
;;

let () =
  Log.set_log_level Log.DEBUG;
  Log.set_output Stdio.stdout;
  let port = 4000 in
  let _ = start_server port Lib.Game.initial_game_state in
  Core.never_returns (Scheduler.go ())
;;
