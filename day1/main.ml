open Printf

let max_number_list lst = List.fold_left max 0 lst

let rec sum_largest_inner lst n acc =
  match n with
  | 0 -> acc
  | x -> (
    match lst with
    | [] -> 0
    | h :: t -> sum_largest_inner t (x - 1) (h + acc))

let max_list filename =
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
    done;
    !max_nums
  with End_of_file ->
    close_in chan;
    max_nums := !count :: !max_nums;
    !max_nums

let ex1 filename = max_number_list (max_list filename)

let ex2 filename =
  let z = List.sort compare (max_list filename) in
  sum_largest_inner (List.rev z) 3 0
;;

let r = ex1 "day1/input1.txt" in
printf "%d\n" r
;;

let r = ex2 "day1/input1.txt" in
printf "%d\n" r
