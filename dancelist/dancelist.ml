type node = {
  id   : int;
  col  : int;
  row  : int;
  mutable up   : node ref option;
  mutable down : node ref option; 
  mutable left : node ref option;
  mutable right: node ref option;
}

let nodes : node ref option array = Array.make 100 None

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
  } in
  (* register *)
  nodes.(res.head.id) <- Some (ref res.head);
  
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
  nodes.(n.id) <- Some (ref n);
  
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

  Printf.printf "after remove column\n";
  let i = ref (down c) in
  while !i != c do
    
    Printf.printf "remove from row %d\n" !i.row;
    let j = ref (right !i) in
    while !j != !i do

      Printf.printf "remove node id = %d row = %d col = %d\n" !j.id !j.row !j.col;
      let jdown = (down !j) in
      let jup = (up !j) in
      Printf.printf "        node id = %d up = %d down = %d\n" !j.id jup.id jdown.id;

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

let recover' dl (c: node ref) : unit = 
  let c = !c in

  let i = ref (up c) in
  while !i != c do
    
    let j = ref (left !i) in
    while !j != !i do

      (down !j).up <- Some j;
      (up !j).down <- Some j;
      dl.col_sz.(col !j) <- dl.col_sz.(col !j) + 1;
      Printf.printf "      \027[38;5;2m[-]\027[0m n21 down = %d\n" (down !(some nodes.(21))).id;
      Printf.printf "      update j to node %d\n" (left !j).id;
      print_ref_address j;
      print_ref_address (some !(some nodes.(21)).down);
      j := left !j;
      Printf.printf "      \027[38;5;1m[+]\027[0m n21 down = %d\n" (down !(some nodes.(21))).id;
    done;

    i := up !i;
  done;
  (left c).right <- c.right;
  (right c).left <- c.left
  


let recover dl (c : node ref) : unit =
  let c = !c in

  let i = ref (up c) in
  (* Printf.printf "i init = node %d\n" !i.id; *)
  while !i != c do
    (* Printf.printf "\n  i each = node %d\n" !i.id; *)

    let j = ref (left !i) in
    (* Printf.printf "    j init = node %d\n" !j.id;
    Printf.printf "    j ";
    print_ref_address j; *)

    (while !j != !i do
      (* Printf.printf "\n      j each = node %d\n" !j.id;
      Printf.printf "      j ";
      print_ref_address j;
      Printf.printf "      n ";
      print_ref_address (some !(some nodes.(21)).down);
      Printf.printf "      write node %d down = %d\n" (up !j).id !j.id; *)
      (up !j).down <- Some (ref !j);
      
      (* Printf.printf "      write node %d up   = %d\n" (down !j).id !j.id; *)
      (down !j).up <- Some (ref !j);
      
      (* Printf.printf "      plus 1 in col %d size\n" !j.col; *)
      dl.col_sz.(!j.col) <- dl.col_sz.(!j.col) + 1;

      (* Printf.printf "      \027[38;5;2m[-]\027[0m n21 down = %d\n" (down !(some nodes.(21))).id;
      Printf.printf "      j ";
      print_ref_address j;
      Printf.printf "      n ";
      print_ref_address (some !(some nodes.(21)).down); *)

      (* Printf.printf "      update j to node %d\n" (left !j).id; *)
      j := !(some !j.left);

      (* Printf.printf "      j ";
      print_ref_address j;
      Printf.printf "      n ";
      print_ref_address (some !(some nodes.(21)).down);
      Printf.printf "      \027[38;5;1m[+]\027[0m n21 down = %d\n" (down !(some nodes.(21))).id; *)
    done);
    (* Printf.printf "    update i to node %d\n" (up !i).id; *)
    i := !(some !i.up)
  done;

  (* Printf.printf "write node %d left = %d\n" (right c).id c.id; *)
  (right c).left <- Some (ref c);

  (* Printf.printf "write node %d right = %d\n" (left c).id c.id; *)
  (left c).right <- Some (ref c)


let answer dep dl : unit = 
  for i = 1 to dep do
    Printf.printf "%d\n" dl.ans.(i);
  done

let rec dance dep dl =
  Printf.printf "dep = %d\n" dep; 

  if right dl.head == dl.head then (answer dep dl; true)
  else begin
    Printf.printf "try to solve int dep %d\n" dep; 

    let colmin = ref (right dl.head) in

    let cur = colmin in
    while !cur != dl.head do
      (if colsz dl !cur < colsz dl !colmin then colmin := !cur);
      cur := right !cur
    done;

    Printf.printf "before remove\n";
    remove dl colmin;
    Printf.printf "after remove\n";

    let find_one_ans = ref false in
    let i = ref (down !colmin)  in
    while !i != !colmin && not !find_one_ans do
      dl.ans.(dep) <- row !i;
      
      Printf.printf "choose row %d\n" !i.row;

      let j = ref (right !i) in
      while !j != !i do
        remove dl (column dl !j);
        j := right !j
      done;

      (if dance (dep + 1) dl then find_one_ans := true);
      
      let j = ref (left !i) in
      while !j != !i do
        recover dl (column dl !j);
        j := left !j
      done;

      i := down !i
    done;  

    recover dl colmin;
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

let to_id_array dl = 
  let r = dl.rowlen in
  let c = dl.colen in
  let arr = Array.make_matrix r c 0 in
  for i = 1 to c do
    let column = !(some dl.cols.(i)) in

    let j = ref (down column) in
    while !j != column do
      arr.(!j.row - 1).(!j.col - 1) <- !j.id;
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

let solver (arr : int array array)= 
  let dl = of_array arr in
  ignore (dance 1 dl)
  (* failwith "todo" *)

let test1 () =
  solver arr



let dl = of_array arr

let col1 = some dl.cols.(1)
let n17 = (down !col1)

let n18 = !(some n17.left) 
let col3 = some dl.cols.(3)


let col4 = !(some dl.cols.(4))
let n21 = down col4
let n18 = down n21
let n12 = down n18


let col7 = !(some dl.cols.(7))
let n23 = down col7
let n20 = down n23
let n13 = down n20
