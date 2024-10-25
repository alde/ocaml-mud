open Base
open Game

type command =
  | Look
  | Move of string (* direction *)
  | Say of string (* message *)
  | Unknown of string (* command *)
  | Exit

let parse_command input =
  let parts = String.split ~on:' ' input in
  match parts with
  | [] -> Unknown "Empty command"
  | cmd :: args ->
    (match String.lowercase cmd with
     | "look" -> Look
     | "north" | "south" | "east" | "west" | "up" | "down" -> Move cmd
     | "say" -> Say (String.concat ~sep:" " args)
     | "exit" -> Exit
     | _ -> Unknown input)
;;

let current_room_and_player state player_name =
  let player =
    List.find_exn state.players ~f:(fun p -> String.equal p.name player_name)
  in
  List.find_exn state.rooms ~f:(fun r -> r.id = player.current_room), player
;;

let handle_look (state : Game.game_state) (player_name : string) =
  let current_room, player = current_room_and_player state player_name in
  Core.sprintf "_%s_" current_room.name |> player.echo;
  current_room.description |> player.echo;
  let exits =
    List.map current_room.exits ~f:(fun (dir, _) -> dir) |> String.concat ~sep:", "
  in
  Core.sprintf "\tExits: [%s]" exits |> player.echo;
  "Players here:" |> player.echo;
  List.iter state.players ~f:(fun p ->
    if p.current_room = player.current_room
    then
      if not (String.equal p.name player_name)
      then p.name |> player.echo
      else "you" |> player.echo)
;;

let handle_say state player_name message =
  let player = Game.find_player state player_name in
  let others =
    List.filter state.players ~f:(fun p -> p.current_room = player.current_room)
    |> List.filter ~f:(fun p -> not (String.equal p.name player_name))
  in
  Core.sprintf "You say: %s" message |> player.echo;
  List.iter others ~f:(fun p -> Core.sprintf "%s says: %s" player_name message |> p.echo)
;;

let execute_command (state : Game.game_state) (player_name : string) (command : command) =
  match command with
  | Look ->
    handle_look state player_name;
    state
  | Move direction ->
    let new_state = move_player state player_name direction in
    (* force player to look when they enter a new room *)
    handle_look state player_name;
    new_state
  | Say message ->
    handle_say state player_name message;
    state
  | Unknown input ->
    let player = Game.find_player state player_name in
    Core.sprintf "Unknown command: %s\n" input |> player.echo;
    state
  | Exit -> state
;;
