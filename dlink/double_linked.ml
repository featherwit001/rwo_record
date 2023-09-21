type 'a elt = {
  value : 'a;
  mutable prev : 'a elt option;
  mutable next : 'a elt option;
}

type 'a dlink = {
  mutable head : 'a elt option;
  mutable len : int
}

let create () = {
  head = None;
  len = 0
}

let is_empty dl = 
  dl.len = 0 && dl.head = None

let first dl = dl.head
let next elt = elt.next
let prev elt = elt.prev
let value elt = elt.value

let iter dl ~f =
  let rec loop = function
    | None -> ()
    | Some elt -> f (value elt); loop (next elt)
  in loop dl.head

let find_el dl ~f =
  let rec loop = function
    | None -> None 
    | Some elt as e -> if f (value elt) then e else loop (next elt)
  in loop dl.head

let insert_first dl value =
  let new_elt =  {value; prev = None; next = dl.head}in
  (match dl.head with
    | None -> ()
    | Some elt -> elt.prev <- Some new_elt);
  dl.head <- Some new_elt;
  dl.len <- dl.len + 1;
  new_elt

let insert_after dl elt value =
  let new_elt = {value; prev = Some elt; next = elt.next} in
  (match elt.next with
    | None -> ()
    | Some ne -> ne.prev <- Some new_elt);
  elt.next <- Some new_elt;
  dl.len <- dl.len + 1;
  new_elt

(* elt must be the member of dlink *)
let remove dl value =
  match find_el dl ~f:(fun v -> v = value) with
    | None -> ()
    | Some {prev; next; _} -> begin
        (match prev with None -> dl.head <- next 
                       | Some pe -> pe.next <- next);
        (match next with None -> () 
                       | Some ne -> ne.prev <- prev);
        dl.len <- dl.len - 1
      end
  
let of_list lst =
  let dl = create () in
  let rec aux = function
    | [] -> ()
    | h :: t -> ignore(insert_first dl h); aux t
  in aux (List.rev lst);
  dl

let to_list dl = 
  let rec aux acc = function
    | None -> List.rev acc
    | Some e -> aux (e.value :: acc) (e.next)
  in aux [] dl.head   