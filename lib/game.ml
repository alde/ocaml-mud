open Base

type room =
  { id : int
  ; name : string
  ; description : string
  ; exits : (string * int) list
  ; items : string list
  }

type player =
  { name : string
  ; mutable current_room : int
  ; inventory : string list
  }

type game_state =
  { rooms : room list
  ; players : player list
  }

(* TODO: read rooms from a file or database *)
let rooms =
  [ { id = 0
    ; name = "Entrance"
    ; description = "You are at the entrance of the dungeon."
    ; exits = [ "north", 1 ]
    ; items = []
    }
  ; { id = 1
    ; name = "Hallway"
    ; description = "You are in a long hallway."
    ; exits = [ "south", 0; "north", 2 ]
    ; items = []
    }
  ; { id = 2
    ; name = "Treasure Room"
    ; description = "You are in a room filled with treasure."
    ; exits = [ "south", 1 ]
    ; items = [ "gold coin" ]
    }
  ]
;;

let initial_game_state = { rooms; players = [] }

let add_player state name =
  let player = { name; current_room = 0; inventory = [] } in
  { state with players = player :: state.players }
;;

let move_player echo state player_name direction =
  let player_opt =
    List.find state.players ~f:(fun p -> String.equal p.name player_name)
  in
  match player_opt with
  | None ->
    Core.sprintf "Player not found: %s\n" player_name |> echo;
    state
  | Some player ->
    let current_room = List.find state.rooms ~f:(fun r -> r.id = player.current_room) in
    (match current_room with
     | None ->
       Core.sprintf "Room not found: %d\n" player.current_room |> echo;
       state
     | Some room ->
       (match List.find room.exits ~f:(fun (dir, _) -> String.equal dir direction) with
        | None ->
          Core.sprintf "No exit in that direction: %s\n" direction |> echo;
          state
        | Some (_, new_room_id) ->
          player.current_room <- new_room_id;
          Core.sprintf "Player %s moved %s\n" player_name direction |> echo;
          state))
;;