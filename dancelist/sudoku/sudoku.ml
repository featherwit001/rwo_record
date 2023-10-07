open Dancelist


let _sudoku0 = [|
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|]; 
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|];
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|]; 
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|];
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|]; 
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|];
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|]; 
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|];
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|]|]


let sudoku1 = [|
  [|8; 0; 0; 0; 0; 0; 0; 0; 0; |]; 
  [|0; 0; 3; 6; 0; 0; 0; 0; 0; |];
  [|0; 7; 0; 0; 9; 0; 2; 0; 0; |];
  [|0; 5; 0; 0; 0; 7; 0; 0; 0; |];
  [|0; 0; 0; 0; 4; 5; 7; 0; 0; |];
  [|0; 0; 0; 1; 0; 0; 0; 3; 0; |];
  [|0; 0; 1; 0; 0; 0; 0; 6; 8; |];
  [|0; 0; 8; 5; 0; 0; 0; 1; 0; |];
  [|0; 9; 0; 0; 0; 0; 4; 0; 0; |];
|]

let sudoku1' = [|
  [|8; 1; 2; 7; 5; 3; 6; 4; 9; |]; 
  [|9; 4; 3; 6; 8; 2; 1; 7; 5; |];
  [|6; 7; 5; 4; 9; 1; 2; 8; 3; |];
  [|1; 5; 4; 2; 3; 7; 8; 9; 6; |];
  [|3; 6; 9; 8; 4; 5; 7; 2; 1; |];
  [|2; 8; 7; 1; 6; 9; 5; 3; 4; |];
  [|5; 2; 1; 9; 7; 4; 3; 6; 8; |];
  [|4; 3; 8; 5; 2; 6; 9; 1; 7; |];
  [|7; 9; 6; 3; 1; 8; 4; 5; 2; |];
|]

(* sudoku2 is hard for DFS *)
let sudoku2 = [|
  [|9; 0; 0; 8; 0; 0; 0; 0; 0|]; 
  [|0; 0; 0; 0; 0; 0; 5; 0; 0|];
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|]; 
  [|0; 2; 0; 0; 1; 0; 0; 0; 3|];
  [|0; 1; 0; 0; 0; 0; 0; 6; 0|]; 
  [|0; 0; 0; 4; 0; 0; 0; 7; 0|];
  [|7; 0; 8; 6; 0; 0; 0; 0; 0|]; 
  [|0; 0; 0; 0; 3; 0; 1; 0; 0|];
  [|4; 0; 0; 0; 0; 0; 2; 0; 0|]|]

let sudoku3 = [|
  [|0; 0; 9; 0; 0; 7; 4; 0; 0|]; 
  [|0; 0; 0; 0; 0; 0; 0; 3; 2|];
  [|0; 8; 0; 3; 0; 0; 0; 0; 0|]; 
  [|0; 2; 0; 8; 4; 0; 0; 0; 6|];
  [|8; 0; 0; 0; 0; 0; 0; 0; 1|]; 
  [|9; 0; 0; 0; 1; 5; 0; 7; 0|];
  [|0; 0; 0; 0; 0; 6; 0; 9; 0|]; 
  [|5; 1; 0; 0; 0; 0; 0; 0; 0|];
  [|0; 0; 4; 2; 0; 0; 5; 0; 0|]|]

let sudoku4 = [|
  [|0; 0; 0; 0; 4; 0; 0; 0; 9|]; 
  [|0; 0; 2; 0; 1; 0; 0; 0; 0|];
  [|5; 0; 0; 0; 0; 0; 0; 7; 3|]; 
  [|0; 9; 0; 0; 0; 0; 0; 0; 0|];
  [|0; 0; 4; 0; 0; 0; 1; 0; 0|]; 
  [|0; 0; 0; 5; 0; 7; 0; 0; 0|];
  [|0; 0; 1; 0; 2; 0; 0; 0; 0|]; 
  [|0; 0; 0; 0; 0; 3; 0; 8; 5|];
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|]|]

let sudoku5 = [|
  [|0; 0; 0; 0; 0; 0; 0; 0; 0|]; 
  [|0; 0; 0; 0; 0; 3; 0; 8; 5|];
  [|0; 0; 1; 0; 2; 0; 0; 0; 0|]; 
  [|0; 0; 0; 5; 0; 7; 0; 0; 0|];
  [|0; 0; 4; 0; 0; 0; 1; 0; 0|]; 
  [|0; 9; 0; 0; 0; 0; 0; 0; 0|];
  [|5; 0; 0; 0; 0; 0; 0; 7; 3|]; 
  [|0; 0; 2; 0; 1; 0; 0; 0; 0|];
  [|0; 0; 0; 0; 4; 0; 0; 0; 9|]|]

(* hard for human*)
let sudoku6 = [|
  [|0; 1; 0; 0; 0; 0; 0; 0; 9|]; 
  [|0; 0; 0; 3; 0; 0; 8; 0; 0|];
  [|0; 0; 0; 0; 0; 0; 6; 0; 0|]; 
  [|0; 0; 0; 0; 1; 2; 4; 0; 0|];
  [|7; 0; 3; 0; 0; 0; 0; 0; 0|]; 
  [|5; 0; 0; 0; 0; 0; 0; 0; 0|];
  [|8; 0; 0; 6; 0; 0; 0; 0; 0|]; 
  [|0; 0; 0; 0; 4; 0; 0; 2; 0|];
  [|0; 0; 0; 7; 0; 0; 0; 5; 0|]|]
  
(* hardest for find all *)
let sudoku7 = [|
  [|0; 0; 0; 8; 0; 1; 0; 0; 0|]; 
  [|0; 0; 0; 0; 0; 0; 0; 4; 3|];
  [|5; 0; 0; 0; 0; 0; 0; 0; 0|]; 
  [|0; 0; 0; 0; 7; 0; 8; 0; 0|];
  [|0; 0; 0; 0; 0; 0; 1; 0; 0|]; 
  [|0; 2; 0; 0; 3; 0; 0; 0; 0|];
  [|6; 0; 0; 0; 0; 0; 0; 7; 5|]; 
  [|0; 0; 3; 4; 0; 0; 0; 0; 0|];
  [|0; 0; 0; 2; 0; 0; 6; 0; 0|]|]

(* has 13 answers *)
let sudoku8 = [|
  [|7; 0; 4; 0; 8; 9; 0; 0; 0|]; 
  [|0; 0; 0; 0; 0; 0; 3; 4; 0|];
  [|0; 0; 2; 0; 0; 0; 0; 0; 1|]; 
  [|0; 5; 0; 0; 0; 4; 0; 0; 8|];
  [|0; 0; 0; 3; 0; 2; 0; 0; 0|]; 
  [|3; 0; 0; 9; 0; 0; 0; 5; 0|];
  [|2; 0; 0; 0; 0; 0; 0; 0; 0|]; 
  [|0; 4; 6; 0; 0; 0; 0; 0; 0|];
  [|0; 0; 0; 6; 7; 0; 4; 0; 5|]|]


(* has 105 answers *)  
let sudoku9 = [|
  [|0; 0; 0; 3; 4; 0; 0; 5; 0|]; 
  [|3; 0; 2; 0; 0; 0; 0; 6; 0|];
  [|7; 0; 0; 8; 0; 0; 0; 0; 0|]; 
  [|0; 0; 1; 0; 0; 5; 7; 0; 6|];
  [|0; 2; 0; 0; 0; 0; 0; 8; 0|]; 
  [|6; 0; 4; 1; 0; 0; 5; 0; 0|];
  [|0; 5; 0; 0; 0; 1; 0; 0; 3|]; 
  [|0; 6; 0; 0; 0; 0; 8; 0; 5|];
  [|0; 7; 0; 0; 5; 8; 0; 0; 0|]|]  


let constrain row col v dl : unit= 
  let dx = (row - 1) / 3 + 1 in
  let dy = (col - 1) / 3 + 1 in
  let room = (dx - 1) * 3 + dy in
  let f1 = (row - 1) * 9 + v in
  let f2 = 81 + (col - 1) * 9 + v in
  let f3 = 81 * 2 + (room - 1) * 9 + v in
  let f4 = 81 * 3 + (row - 1) * 9 + col in
  let dlrow = (row - 1) * 9 * 9 + (col - 1) * 9 + v in
  insert dlrow f1 dl;
  insert dlrow f2 dl;
  insert dlrow f3 dl;
  insert dlrow f4 dl


let zero_constrain row col dl : unit = 
  for i = 1 to 9 do
    constrain row col i dl;
  done

let not_zero_constrain row col v dl : unit = 
  constrain row col v dl


let sudoku_to_dl su =
  let dl = build 729 324 in
  for i = 1 to 9 do
    for j = 1 to 9 do
      (if su.(i - 1).(j - 1) = 0 then zero_constrain i j dl
      else not_zero_constrain i j su.(i - 1).(j - 1) dl);
    done;
  done;
  dl 

let sudoku_to_string su =
  let res = ref "" in 
  for i = 0 to 8 do
    for j = 0 to 8 do
      res := !res ^ (string_of_int su.(i).(j)) ^ " ";
    done;
    res := !res ^ "\n";
  done ;
  !res

let answer_to_sudoku dep ans =
  let sudoku = Array.make_matrix 9 9 0 in
  for i = 1 to dep - 1 do
    (* Printf.printf "resolve the %dth ans which is %d \n" i dl.ans.(i); *)
    let x = (ans.(i) - 1) / 9 / 9 + 1 in
    let y = (ans.(i) - 1) / 9 mod 9 + 1 in
    let v = (ans.(i) - 1) mod 9 + 1 in
    sudoku.(x - 1).(y - 1) <- v;
  done;
  sudoku

let resolve_one_sudoku ans dep = 
  answer_to_sudoku dep ans
  |> sudoku_to_string
  |> print_endline

let test2 () = 
  Printf.printf "start test2: \n";
  let dl = sudoku_to_dl sudoku1 in
  Printf.printf "dance...\n";
  ignore(dance 1 dl);
  Printf.printf "resolve\n";
  resolve_dl resolve_one_sudoku dl

let solve_sudoku su = 
  let dl = sudoku_to_dl su in
  ignore(dance 1 dl);
  dl
  |> resolve_dl resolve_one_sudoku

let solve_sudoku_all su = 
  let dl = sudoku_to_dl su in
  ignore(dance ~find_one:false 1 dl);
  dl
  |> resolve_dl resolve_one_sudoku


let time f =
  let open Core in 
  let start = Time.now() in
  let x = f () in
  let stop = Time.now() in
  Printf.printf "Time: %F ms\n" (Time.diff stop start |> Time.Span.to_ms);
  x
