open String
open Printf

let get_score_ex1 l r =
  match l with
  | 'A' -> (
    match r with
    | 'X' -> 3 + 1
    | 'Y' -> 6 + 2
    | 'Z' -> 0 + 3
    | _ -> 0)
  | 'B' -> (
    match r with
    | 'X' -> 0 + 1
    | 'Y' -> 3 + 2
    | 'Z' -> 6 + 3
    | _ -> 0)
  | 'C' -> (
    match r with
    | 'X' -> 6 + 1
    | 'Y' -> 0 + 2
    | 'Z' -> 3 + 3
    | _ -> 0)
  | _ -> 0

let get_score_ex2 l r =
  match l with
  | 'A' -> (
    match r with
    | 'X' -> 0 + 3 (** Scissor *)
    | 'Y' -> 3 + 1 (** Rock *)
    | 'Z' -> 6 + 2 (** Paper *)
    | _ -> 0)
  | 'B' -> (
    match r with
    | 'X' -> 0 + 1 (** Rock *)
    | 'Y' -> 3 + 2 (** Paper *)
    | 'Z' -> 6 + 3 (** Scissor *)
    | _ -> 0)
  | 'C' -> (
    match r with
    | 'X' -> 0 + 2 (** Paper *)
    | 'Y' -> 3 + 3 (** Scissor *)
    | 'Z' -> 6 + 1 (** Rock *)
    | _ -> 0)
  | _ -> 0

let run filename input_fn =
  let sum = ref 0 in
  let chan = open_in filename in
  try
    while true do
      let line = input_line chan in
      let opponent = get line 0 in
      let we = get line 2 in
      sum := !sum + input_fn opponent we
    done;
    !sum
  with End_of_file ->
    close_in chan;
    !sum
;;

let r = run "day2/input1.txt" get_score_ex1 in
printf "%d\n" r
;;

let r = run "day2/input1.txt" get_score_ex2 in
printf "%d\n" r
