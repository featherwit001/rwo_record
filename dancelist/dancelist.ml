type node = {
  id   : int;
  mutable up   : node ref option;
  mutable down : node ref option; 
  mutable left : node ref option;
  mutable right: node ref option;
  col  : int;
  row  : int;
}

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

let next () =
  let count = ref (-1) in
   let f () =
    count := !count + 1;
    !count
  in
    f ()

let n0 = {
  id = next (); col = 0; row = 0;
  up = None; down = None; left = None; right = None; }

let n1 = {
  id = next (); col = 1; row = 0;
  up = None; down = None; left = None; right = None; }  

let n2 = {
  id = next (); col = 2; row = 0;
  up = None; down = None; left = None; right = None; }  

let _ = 
  n0.right <- Some (ref n1); n1.right <- Some (ref n2); n2.right <- Some (ref n0);
  n0.left <- Some (ref n2);  n1.left <- Some (ref n0); n2.left <- Some (ref n1)

let some = function
  | Some x -> x
  | None -> failwith "some get None" 

let r node = !(some node.right)
let l node = !(some node.left)
let u node = !(some node.up)
let d node = !(some node.down)

let col node = node.col
let row node = node.row
let column dl node = !(some dl.cols.(col node)) 

let colsz dl col =
  dl.col_sz.(col.col)

(** process the node list except the init node  *)
let rec iter : node -> node -> (node -> node) -> (node -> unit) -> unit =
  fun init last direct f -> 
  let cur = direct last in
  if cur == init then ()
  else 
    f cur; 
    iter init cur direct f



let build rowlen colen = failwith "todo"

let insert row col dl = failwith "todo"

let remove (col: node) : unit = failwith "todo"

let recover (col : node): unit = failwith "todo"

let answer dep dl : unit = failwith "todo"

let rec dance dep dl =
  if r dl.head == dl.head then (answer dep dl; true)
  else begin
    let colmin = ref (r dl.head) in

    let cur = colmin in
    while !cur != dl.head do
      (if colsz dl !cur < colsz dl !colmin then colmin := !cur);
      cur := r !cur
    done;

    (* todo *)

    false
  end
