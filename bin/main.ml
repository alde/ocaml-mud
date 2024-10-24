open Async

let handle_connection
  (reader : Reader.t)
  (writer : Writer.t)
  (state : Lib.Game.game_state)
  =
  let player_name = "Player" in
  (* TODO: implement a way to create a player etc *)
  let state = Lib.Game.add_player state player_name in
  (* Force a look so the player sees the room their in upon logging in *)
  Lib.Commands.handle_look state player_name (Writer.write_line writer);
  let rec game_loop state =
    Writer.write_line writer "> ";
    Reader.read_line reader
    >>= function
    | `Eof ->
      Core.printf "%s disconnected.\n" player_name;
      return ()
    | `Ok input_line ->
      let open Lib.Commands in
      let new_state =
        parse_command input_line |> execute_command writer state player_name
      in
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
         printf "Client connected from %s\n" (Socket.Address.Inet.to_string address);
         handle_connection reader writer initial_state)
  in
  printf "Server listening on port %d\n" port;
  return ()
;;

let () =
  let port = 4000 in
  let _ = start_server port Lib.Game.initial_game_state in
  Core.never_returns (Scheduler.go ())
;;
