open! Core

let load_rotations_file file =
  let parse_line l =
    Scanf.sscanf l "%c%d" (fun direction magnitude ->
        match direction with
        | 'R' ->
            magnitude
        | 'L' ->
            -1 * magnitude
        | _ ->
            magnitude )
  in
  In_channel.with_file file ~f:(fun ic ->
      In_channel.input_lines ic |> List.map ~f:parse_line |> Array.of_list )

(* normal `mod` doesn't handle negative mods in the way we need *)
let floored_mod x n =
  let result = x mod n in
  if result >= 0 then result else result + n

let rec rotate_dial ~dial rotations i =
  let new_dial_pos = floored_mod (dial.(i) + rotations.(i)) 100 in
  dial.(i + 1) <- new_dial_pos ;
  if Array.length rotations - 1 <= i then ()
  else rotate_dial ~dial rotations (i + 1)

let print_array ~f arr =
  print_string "[" ;
  Array.iteri
    ~f:(fun i elem ->
      match i with 0 -> printf " %s" (f elem) | _ -> printf ", %s" (f elem) )
    arr ;
  print_string " ]"

let command =
  Command.basic ~summary:"Safe Combination Decoder (AOC-2025-1)"
    [%map_open.Command
      let file = anon (maybe_with_default "input.txt" ("FILE" %: string))
      and verbose = flag "verbose" no_arg ~doc:"Enable verbose output." in
      fun () ->
        let rotations = load_rotations_file file in
        if verbose then (
          print_string "Rotations: " ;
          print_array ~f:Int.to_string rotations ;
          print_endline "" ) ;
        let dial = Array.create ~len:(Array.length rotations + 1) 50 in
        rotate_dial ~dial rotations 0 ;
        if verbose then (
          print_string "Dial Position: " ;
          print_array ~f:Int.to_string dial ;
          print_endline "" ) ;
        let password =
          Array.fold_right
            ~f:(fun i acc -> if i = 0 then acc + 1 else acc)
            ~init:0 dial
        in
        printf "Password: %d\n" password]

let () = Command_unix.run command
