(* This is a port of the Scheml Sudoku solver to OCaml.
 * The functions should pretty much correspond one-to-one
 * with the Scheml version with just a few exceptions.
 * For example, fold in Scheml has type 'a -> 'b -> 'b
 * while in OCaml it is 'a -> 'b -> 'a (i.e. the arguments
 * are reversed) so remove_from_available's arguments are
 * reversed here since it is used with List.fold_left.
 * There are a few functions I had to add here that are in
 * Scheml's standard library but not in OCaml.
 * This file is written to automatically run, if you want
 * to play with it in the OCaml repl, comment out the last line.
 *
 * To compile:
 * ocamlopt -o ocaml_solver sudoku.ml
 * then run:
 * ocaml_solver
 *)
let sudoku_display = false;;

type square = Fixed of int
            | Unfixed of int list;;

let rec range i j =
      if i > j then []
      else i :: range (i + 1) j;;  
  
let make_square n =
  if n = 0 then
    Unfixed (range 1 9)
  else
    Fixed n;;

let num_available square =
  match square with
    (Fixed n) -> 1
  | (Unfixed l) -> List.length l;;

let get_available square =
  match square with
    (Fixed _) -> raise (Failure "Tried to get available from fixed square")
  | (Unfixed available) -> available;;

let rec take n l =
  match l with
    [] -> []
  | (x::xs) -> if n == 0 then [] else x :: take (n-1) xs;;

let rec drop n l =
  match l with
    [] -> []
  | (_::xs) -> if n == 0 then l else drop (n-1) xs;;

let row n sud =
  take 9 (drop (n*9) sud);;

let col n sud =
  let rec col' sud =
    match sud with
      [] -> []
    | (h::t) -> h :: col' (drop 9 sud)
  in
  col' (drop n sud);;

let box n sud =
  let rec box' n sud =
    if n <= 0 then []
    else List.append (take 3 sud) (box' (n - 1) (drop 9 sud))
  in
    box' 3 (drop (27 * (n / 3) + 3 * (n mod 3)) sud);;               

let row_num idx = idx / 9;;
let col_num idx = idx mod 9;;
let box_num idx = 3 * (idx / 27) + (idx mod 9) / 3;;

let square_to_char sq =
  match sq with
    (Fixed n) -> Char.chr (n + 48)
  | (Unfixed _) -> '?';;

let print_sudoku_row r =
  Printf.printf "%c %c %c   %c %c %c   %c %c %c\n"
    (square_to_char (List.nth r 0))
    (square_to_char (List.nth r 1))
    (square_to_char (List.nth r 2))
    (square_to_char (List.nth r 3))
    (square_to_char (List.nth r 4))
    (square_to_char (List.nth r 5))
    (square_to_char (List.nth r 6))
    (square_to_char (List.nth r 7))
    (square_to_char (List.nth r 8));;

let print_sudoku sud =
  print_sudoku_row (row 0 sud);
  print_sudoku_row (row 1 sud);
  print_sudoku_row (row 2 sud);
  Printf.printf("\n");
  print_sudoku_row (row 3 sud);
  print_sudoku_row (row 4 sud);
  print_sudoku_row (row 5 sud);
  Printf.printf("\n");
  print_sudoku_row (row 6 sud);
  print_sudoku_row (row 7 sud);
  print_sudoku_row (row 8 sud);;


let is_fixed sq =
  match sq with
    (Fixed _) -> true
  | (Unfixed _) -> false;;

let is_set_correct vals =
  let add_fixed n v =
    match v with
      (Fixed v) -> n + (Int.shift_left 1 v)
    | (Unfixed _) -> 0
  in                
  1022 == List.fold_left add_fixed 0 vals;;

let is_set_fixed vals =
  List.for_all is_fixed vals;;

let are_sets_correct sud n =
  is_set_correct (row n sud) &&
    is_set_correct (col n sud) &&
      is_set_correct (box n sud);;

let id x = x;;

let is_correct sud =
  List.for_all id (List.map (are_sets_correct sud) (range 0 8));;

let is_failed sud =
  List.exists ((==) 0) (List.map num_available sud);;

let remove a l =
  let rec remove' a l acc =
    match l with
      [] -> List.rev acc
    | (x::xs) ->
       if x == a then
         List.append (List.rev acc) xs
       else
         remove' a xs (x::acc)
  in
  remove' a l [];;

let remove_from_available sq other_square =
  match sq with
    (Fixed _) -> sq
  | (Unfixed available) ->
     match other_square with
       (Fixed n) -> Unfixed (remove n available)
     | (Unfixed _) -> sq;;

let occurs_free n sq =
  match sq with
    (Fixed _) -> false
  | (Unfixed available) -> List.mem n available;;
  

let rec count_occurrences n square_list acc =
  match square_list with
    [] -> acc
  | (h::t) ->
     if occurs_free n h then
       count_occurrences n t (acc + 1)
     else
       count_occurrences n t acc;;

let rec try_fix_list sq vals square_list =
  match vals with
    [] -> sq
  | (h::t) ->
     if 1 == count_occurrences h square_list 0 then
       Fixed h
     else
       try_fix_list sq t square_list;;

let try_fix sq square_list =
  match sq with
    (Fixed _) -> sq
  | (Unfixed available) ->
     try_fix_list sq available square_list;;

let fix_if_one sq =
  match sq with
    (Fixed _) -> sq
  | (Unfixed available) ->
     if 1 == List.length available then
       Fixed (List.hd available)
     else
       sq;;

let replace_nth n v l =
  List.append (take n l)
    (List.append [v] (drop (n+1) l));;

let set_square v pos sud =
  replace_nth pos v sud;;

let reduce_square sud sq n =
  if is_fixed sq then
    sq
  else
    let row_squares = row (row_num n) sud in
    let col_squares = col (col_num n) sud in
    let box_squares = box (box_num n) sud in
    let reduced = fix_if_one (List.fold_left remove_from_available sq
                                (List.append row_squares (List.append col_squares box_squares)))
    in
    if is_fixed reduced then
      reduced
    else
      try_fix (try_fix (try_fix reduced row_squares) col_squares) box_squares;;


let reduce_squares sud =
  if is_failed sud then
    sud
  else
    let rec reduce_squares' n curr_sud =
      if n < 0 then
        (if sudoku_display then
           (Printf.printf "%c[H" (Char.chr 27);
            print_sudoku curr_sud;
           curr_sud)
         else
           curr_sud
        )
      else
        let orig_sq = List.nth sud n in
        let reduced_sq = reduce_square curr_sud orig_sq n in
        if is_fixed orig_sq then
          reduce_squares' (n-1) curr_sud
        else
          reduce_squares' (n-1) (set_square reduced_sq n curr_sud)
    in
    reduce_squares' 80 sud;;
           
let squares_equal s1 s2 =
  match s1 with
    (Fixed n) ->
     (match s2 with
       (Fixed n2) -> n == n2
      | _ -> false)
  | (Unfixed av) ->
     (match s2 with
        (Fixed _) -> false
      | (Unfixed av2) ->
         if (List.length av) == (List.length av2) then
           List.for_all2 (==) av av2
         else
           false);;
         
let rec reduce sud =
  let new_sud = reduce_squares sud in
  match new_sud with
    [] -> []
  | _ ->
     if List.for_all2 squares_equal new_sud sud then
       new_sud
     else
       reduce new_sud;;

let is_done sud =
  if List.for_all is_fixed sud then
    is_correct sud
  else
    false;;

let try_solve_val_at_pos pos v sud try_func =
  let reduced = reduce (set_square (Fixed v) pos sud) in
  if is_done reduced then
    reduced
  else
    if is_failed reduced then
      []
    else
      try_func reduced;;

let rec try_solve_vals pos available sud try_func =
  match available with
    [] -> []
  | (h::t) ->
     let tried = try_solve_val_at_pos pos h sud try_func in
     match tried with
       [] -> try_solve_vals pos t sud try_func
     | _ -> tried;;

type next_square = NextSquare of int * square;;

let rec get_next_to_try' sud_rest pos curr_min curr_min_pos curr_min_sq =
  match sud_rest with
    [] -> NextSquare (curr_min_pos, curr_min_sq)
  | (h::t) ->
     match h with
       (Fixed _) -> get_next_to_try' t (pos+1) curr_min curr_min_pos curr_min_sq
     | (Unfixed available) ->
        if 2 == List.length available then
          NextSquare (pos, h)
        else if (List.length available) < curr_min then
          get_next_to_try' t (pos+1) (List.length available) pos h
        else
          get_next_to_try' t (pos+1) curr_min curr_min_pos curr_min_sq;;

let get_next_to_try sud =
  get_next_to_try' sud 0 10 (-1) (Fixed 0);;

let rec solve_list sud =
  match get_next_to_try sud with
    NextSquare (n,sq) ->
     if n < 0 then
       []
     else
       try_solve_vals n (get_available sq) sud solve_list;;

     
let solve_sudoku sud_puzzle =
  let sud = List.map make_square sud_puzzle in
  let reduced = reduce sud in
  solve_list reduced;;

let example_sudoku = [
    8; 5; 0;  0; 0; 2;  4; 0; 0;
    7; 2; 0;  0; 0; 0;  0; 0; 9;
    0; 0; 4;  0; 0; 0;  0; 0; 0;

    0; 0; 0;  1; 0; 7;  0; 0; 2;
    3; 0; 5;  0; 0; 0;  9; 0; 0;
    0; 4; 0;  0; 0; 0;  0; 0; 0;

    0; 0; 0;  0; 8; 0;  0; 7; 0;
    0; 1; 7;  0; 0; 0;  0; 0; 0;
    0; 0; 0;  0; 3; 6;  0; 4; 0];;

let string_to_list s = List.init (String.length s) (String.get s);;

let line_to_sudoku line =
  List.map make_square
    (List.map (function ch ->
                 (Char.code (if ch == '.' then '0' else ch)) - 48)
       (string_to_list line));;

let fixed_to_int sq =
  match sq with
    (Fixed v) -> v
  | (Unfixed _) -> raise (Failure "Tried to read unfixed value");;

let list_to_string l = String.of_seq (List.to_seq l)
                     
let sudoku_to_line sudoku =
  list_to_string (List.map (function sq -> Char.chr (48 + (fixed_to_int sq)))
                    sudoku);;

                          
let read_lines filename =
  let ic = open_in filename in
  let rec build_list acc =
    try
      let line = input_line ic in
      build_list (line::acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  build_list [];;

let write_lines filename lines =
  let oc = open_out filename in
  List.iter (function l -> Printf.fprintf oc "%s\n" l) lines;
  close_out oc;;

let solve_sudoku_line line =
  let sud = line_to_sudoku line in
  let reduced = reduce sud in
  let solved = solve_list reduced in
  Printf.printf "Solved: %s\n" (sudoku_to_line solved);
  solved;;

let solve_sudoku_file filename out_filename =
  let lines = read_lines filename in
  if sudoku_display then
    Printf.printf "%c[2J" (Char.chr 27)
  else
    ();
  let results = List.map solve_sudoku_line lines in
  write_lines out_filename (List.map sudoku_to_line results);;

solve_sudoku_file "top95.txt" "foobar.txt";;
