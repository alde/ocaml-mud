open Base
open Game
open Async

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

let handle_look (state : Game.game_state) (player_name : string) echo =
  let current_room, player = current_room_and_player state player_name in
  Core.sprintf "_%s_" current_room.name |> echo;
  current_room.description |> echo;
  let exits =
    List.map current_room.exits ~f:(fun (dir, _) -> dir) |> String.concat ~sep:", "
  in
  Core.sprintf "\tExits: [%s]" exits |> echo;
  "Players here:" |> echo;
  List.iter state.players ~f:(fun p ->
    if p.current_room = player.current_room
    then if not (String.equal p.name player_name) then p.name |> echo else "you" |> echo)
;;

let execute_command
  (writer : Writer.t)
  (state : Game.game_state)
  (player_name : string)
  (command : command)
  =
  let echo message = Writer.write_line writer message in
  match command with
  | Look ->
    handle_look state player_name echo;
    state
  | Move direction ->
    let new_state = move_player echo state player_name direction in
    (* force player to look when they enter a new room *)
    handle_look state player_name echo;
    new_state
  | Say message ->
    Core.sprintf "%s says: %s\n" player_name message |> echo;
    state
  | Unknown input ->
    Core.sprintf "Unknown command: %s\n" input |> echo;
    state
  | Exit -> state
;;
