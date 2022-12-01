open Printf

let ex1 filename =
  let count = ref 0 in
  let max = ref 0 in
  let chan = open_in filename in
  try
    while true do
      let line = input_line chan in
      count :=
        if line <> "" then !count + int_of_string line
        else (
          if !count > !max then max := !count;
          0)
    done
  with End_of_file ->
    close_in chan;
    if !count > !max then max := !count;
    print_endline (string_of_int !max)

let rec sum_largest_inner l n acc =
  match n with
  | 0 -> acc
  | x -> (
    match l with
    | [] -> 0
    | h :: t -> sum_largest_inner t (x - 1) (h + acc))

let ex2 filename =
  let count = ref 0 in
  let max_nums = ref [] in
  let chan = open_in filename in
  try
    while true do
      let line = input_line chan in
      count :=
        if line <> "" then !count + int_of_string line
        else (
          max_nums := !count :: !max_nums;
          0)
    done
  with End_of_file ->
    close_in chan;
    max_nums := !count :: !max_nums;
    let z = List.sort compare !max_nums in
    print_int (sum_largest_inner (List.rev z) 3 0)
;;

ex1 "day1/input1.txt";;

ex2 "day1/input1.txt"
