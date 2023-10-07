type node = {
  id   : int;
  col  : int;
  row  : int;
  mutable up   : node ref option;
  mutable down : node ref option; 
  mutable left : node ref option;
  mutable right: node ref option;
}


type answer_node = {
  ans : int array;
  dep : int;
  mutable next : answer_node ref option
}

type answers = {
  mutable length : int;
  mutable first : answer_node ref option
}

(* let nodes : node ref option array = Array.make 100 None *)
type dancelist = {
  colen : int;
  rowlen : int;
  head : node; 

  (* index from 1 *)
  cols : node ref option array;
  col_sz : int array;
  rows : node ref option array;
  (* answer is the row index list, index from 1 *)
  ans : int array;
  mutable dep : int;
  answers : answers;
}
let some = function
  | Some x -> x
  | None -> failwith "some get None" 

let right node = !(some node.right)
let left node = !(some node.left)
let up node = !(some node.up)
let down node = !(some node.down)
let col node = node.col
let row node = node.row
let column dl node = (some dl.cols.(col node)) 

let colsz dl col =
  dl.col_sz.(col.col)

let next =
  let count = ref (-1) in
  let f () =
    count := !count + 1;
    !count
  in
    f

let create_node ?(col = 0) ?(row = 0) () =  
  {id = next(); col; row;
  up = None; down = None; left = None; right = None; }

let build rowlen colen = 
  let res = {
    rowlen; colen;
    head = create_node () ;
    cols = Array.make (colen + 10) None;
    col_sz = Array.make (colen + 10) 0;
    rows = Array.make (rowlen + 10) None;
    ans = Array.make (rowlen + 10) 0;
    dep = 0;
    answers = {length = 0; first = None};
  } in
  (* register *)
  (* nodes.(res.head.id) <- Some (ref res.head); *)
  
  res.cols.(0) <- Some (ref res.head);

  let i = ref 1 in
  while !i <= colen do
    let col = create_node ~col:!i () in
    (* let col = {col with col = !i} in *)
    col.up <- Some (ref col);
    col.down <- Some (ref col);
    col.left <- res.cols.(!i - 1);
    res.cols.(!i) <- Some (ref col);
    i := !i + 1;
  done;

  let i = ref 1 in
  while !i <= colen do
    !(some res.cols.(!i)).right <- res.cols.(!i + 1);
    i := !i + 1;
  done;

  !(some res.cols.(colen)).right <- Some (ref res.head);
  res.head.right <- res.cols.(1);
  res.head.left <- res.cols.(colen);
  res

let insert row col dl : unit = 
  let n = create_node ~row ~col () in

  (* register the node for easy access *)
  (* nodes.(n.id) <- Some (ref n); *)
  
  dl.col_sz.(col) <- dl.col_sz.(col) + 1;

  let column = !(some dl.cols.(col)) in
  n.down <- column.down;
  (down column).up <- Some (ref n);
  n.up <- Some (ref column);
  column.down <- Some (ref n);

  let rowheadptr = dl.rows.(row) in
  match rowheadptr with
  | None -> 
    (n.left <- Some (ref n);
     n.right <- Some (ref n);
     dl.rows.(row) <- Some (ref n);)
  | Some rowhead -> 
    (n.right <- !rowhead.right;
     n.left <- rowheadptr;
     (right !rowhead).left <-  Some (ref n);
     !rowhead.right <- Some (ref n);
    )


let remove dl (c: node ref) : unit = 
  let c = !c in
  (left c).right <- c.right;
  (right c).left <- c.left;

  let i = ref (down c) in
  while !i != c do
    
    let j = ref (right !i) in
    while !j != !i do
      (down !j).up <- !j.up;
      (up !j).down <- !j.down;
      dl.col_sz.(col !j) <- dl.col_sz.(col !j) - 1;

      j := right !j
    done;

    i := down !i;
  done

let print_ref_address ref_var =
  let addr = Obj.magic ref_var in
  Printf.printf "Reference address: %d\n" addr 


let recover dl (c : node ref) : unit =
  let c = !c in
  let i = ref (up c) in
  while !i != c do

    let j = ref (left !i) in
    (while !j != !i do
      (up !j).down <- Some (ref !j);
      (down !j).up <- Some (ref !j);
      dl.col_sz.(!j.col) <- dl.col_sz.(!j.col) + 1;

      j := !(some !j.left);
    done);
    i := !(some !i.up)
  done;

  (right c).left <- Some (ref c);
  (left c).right <- Some (ref c)

let record_ans dep dl= 
  let ans_node = {
    ans = Array.sub dl.ans 0 (dep + 1);
    dep;
    next = dl.answers.first;
  } in
  dl.answers.first <- Some (ref ans_node);
  dl.answers.length <- dl.answers.length + 1

let make_space n =
  String.make (n*4) ' '

let to_id_array dl = 
  let r = dl.rowlen in
  let c = dl.colen in
  let arr = Array.make_matrix (r + 1) c 0 in
  let column = ref (right dl.head) in
  while !column != dl.head do
    arr.(!column.row).(!column.col - 1) <- !column.id;

    let j = ref (down !column) in
    while !j != !column do
      arr.(!j.row).(!j.col - 1) <- !j.id;
      j := down !j
    done;
    column := (right !column);
  done; 
  arr

let colored_num n =
  if n <> 0 then 
    Printf.sprintf "\027[38;5;3m%-5d\027[0m " n
  else Printf.sprintf "%-5d " n

let arr_to_string arr =
  let rlen = Array.length arr in
  let clen = Array.length arr.(0) in
  let res = ref "" in 
  for i = 0 to rlen - 1 do
    for j = 0 to clen - 1 do
      res := !res ^ (colored_num arr.(i).(j));
    done;
    if i = 0 then res := !res ^ "\n" ^ (String.make (clen*5) '-');
    res := !res ^ "\n";
  done ;
  !res

let print_dl dl =
  dl
  |> to_id_array
  |> arr_to_string  
  |> print_endline

let rows_in_column (col) =
  let i = ref (down col) in
  let res = ref "" in
  while !i != col do
    res := !res ^ (Printf.sprintf "%-3d " !i.row);
    i := down !i;
  done;
  !res

let rec dance  ?(find_one=true) dep dl =
  (* Printf.printf "\n%sdep = %d find_one %b \n" (make_space (dep)) dep find_one; a *)

  if right dl.head == dl.head then (
    (* Printf.printf "%s\027[38;5;4m[^]\027[0m find one answer\n" (make_space (dep)); *)
    record_ans dep dl; 
    find_one)
  else begin
    (* Printf.printf "%stry to solve int dep %d\n" (make_space (dep)) dep;  *)
    let colmin = ref (right dl.head) in
    let cur = ref (right dl.head)  in
    while !cur != dl.head do
      (if colsz dl !cur < colsz dl !colmin then colmin := !cur);

      cur := right !cur
    done;

    remove dl colmin;
    (* Printf.printf "%s\027[38;5;1m[-]\027[0m remove col %d\n" (make_space (dep)) !colmin.id; *)
    (* Printf.printf "%scol %d has row %s\n" (make_space (dep)) !colmin.col (rows_in_column !colmin); *)


    let find_one_ans = ref false in
    let i = ref (down !colmin)  in
    while !i != !colmin && not !find_one_ans do
      dl.ans.(dep) <- row !i;
      (* Printf.printf "%scould go to next loop : %b\n" (make_space (dep)) (not !find_one_ans); *)
      (* Printf.printf "%s\027[38;5;4m[?]\027[0m choose row %d\n" (make_space (dep)) !i.row; *)
      
      (* Printf.printf "%sremove row %d elt\n" (make_space (dep)) !i.row; *)

      let j = ref (right !i) in
      while !j != !i do
        remove dl (column dl !j);
        (* Printf.printf "%s\027[38;5;1m[-]\027[0m remove col %d\n" (make_space (dep)) (!j.col); *)
        
        j := right !j
      done;

      if dance ~find_one (dep + 1) dl && find_one then (
          (* Printf.printf "%sModified out loop flag\n" (make_space (dep)); *)
        find_one_ans := true);
      
      let j = ref (left !i) in
      while !j != !i do
        recover dl (column dl !j);
        (* Printf.printf "%s\027[38;5;2m[+]\027[0m recover col %d\n" (make_space (dep)) (!j.col); *)

        j := left !j
      done;

      (* Printf.printf "%srecover row %d elt\n" (make_space (dep)) !i.row; *)

      i := down !i;
      (* Printf.printf "%snext choosed row %d could go to next loop : %b \n" (make_space (dep)) !i.row (not !find_one_ans); *)
    done;  
  
  recover dl colmin;
  (* Printf.printf "%s\027[38;5;2m[+]\027[0m recover col %d\n" (make_space (dep)) (!colmin.col); *)
  
  !find_one_ans
  end

(* test *)

let arr_easy = [|
  [|1; 0; 0|];
  [|0; 0; 1|];
  [|1; 1; 0|];
  |]


let arr = [|             
  [|0; 0; 1; 0; 1; 1; 0|];
  [|1; 0; 0; 1; 0; 0; 1|];
  [|0; 1; 1; 0; 0; 1; 0|];
  [|1; 0; 0; 1; 0; 0; 0|];
  [|0; 1; 0; 0; 0; 0; 1|];
  [|0; 0; 0; 1; 1; 0; 1|];|]

(* has 2 answers *)
let arr' = [|
  [|0; 0; 1; 0; 1; 1; 0|];
  [|1; 0; 0; 1; 0; 0; 1|];
  [|0; 1; 1; 0; 0; 1; 0|];
  [|1; 0; 0; 1; 0; 0; 0|];
  [|0; 1; 0; 0; 0; 0; 1|];
  [|0; 0; 0; 1; 1; 0; 1|];
  [|0; 0; 0; 0; 1; 0; 1|];
|] 



let to_array dl = 
  let r = dl.rowlen in
  let c = dl.colen in
  let arr = Array.make_matrix r c 0 in
  for i = 1 to c do
    let column = !(some dl.cols.(i)) in

    let j = ref (down column) in
    while !j != column do
      arr.(!j.row - 1).(!j.col - 1) <- 1;
      j := down !j
    done
  done; 
  arr


(* only for testing build  *)
let to_array_by_row dl =
  let rlen = dl.rowlen in
  let clen = dl.colen in
  let arr = Array.make_matrix rlen clen 0 in
  for i = 1 to rlen do
    let row = !(some dl.rows.(i)) in

    let j = ref (row) in
    arr.(!j.row - 1).(!j.col - 1) <- 1;
    j := right !j;
    while !j != row do
      arr.(!j.row - 1).(!j.col - 1) <- 1;
      j := right !j
    done
  done; 
  arr

let to_array_by_rcol dl = 
  let rlen = dl.rowlen in
  let clen = dl.colen in
  let arr = Array.make_matrix rlen clen 0 in

  let i = ref (right dl.head) in
  while !i != dl.head do
    let column = !i in
    Printf.printf "at col = %d\n" column.col;

    let j = ref (down column) in
    while !j != column do
      Printf.printf "to array id = %d row = %d col = %d\n" !j.id !j.row !j.col;
      arr.(!j.row - 1).(!j.col - 1) <- 1;
      j := down !j
    done;

    i := right !i;
  done; 
  arr

let to_array_by_lcol dl = 
  let rlen = dl.rowlen in
  let clen = dl.colen in
  let arr = Array.make_matrix rlen clen 0 in

  let i = ref (left dl.head) in
  while !i != dl.head do
    let column = !i in
    Printf.printf "at col = %d\n" column.col;

    let j = ref (down column) in
    while !j != column do
      Printf.printf "to array id = %d row = %d col = %d\n" !j.id !j.row !j.col;
      arr.(!j.row - 1).(!j.col - 1) <- 1;
      j := down !j
    done;

    i := left !i;
  done; 
  arr
  

let of_array arr = 
  let r = Array.length arr in
  let c = Array.length (arr.(0)) in
  let dl = build r c in
  for i = 1 to r do
    for j = 1 to c do 
      if arr.(i - 1).(j - 1) <> 0 then insert i j dl 
    done
  done;
  dl

let print_rows ans dep =
  Printf.printf "answer start:\n";
  for i = 1 to dep - 1 do
    Printf.printf "row %d\n" ans.(i)
  done;
  Printf.printf "answer end\n"

let rec resolve_anss resolve_one (answer_node: answer_node ref option)  = 
  match answer_node with
  | None -> ()
  | Some answer ->
    resolve_one !answer.ans !answer.dep;
    resolve_anss resolve_one !answer.next

let resolve_dl  resolve_one dl : unit =
  let answer_node = dl.answers.first in
  match answer_node with
  | None -> Printf.printf "no answer\n";
  | Some _ as a-> 
    (Printf.printf "tot: %d answer(s)\n" dl.answers.length;
    resolve_anss resolve_one a;
    Printf.printf "tot: %d answer(s)\n" dl.answers.length;
    )

let test1 () =
  let dl = of_array arr in
  ignore (dance 1 dl);
  resolve_dl print_rows dl 

let test_find_all ()=
  let dl = of_array arr' in
  ignore (dance ~find_one:false 1 dl);
  resolve_dl print_rows dl

