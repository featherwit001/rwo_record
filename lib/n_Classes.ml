open Base

(* in OCaml, class must be defined as toplevel statments in a module *)
module ClassTest = struct

  class istack = object
    val mutable v = [0; 2]

    method pop =
      match v with
      | [] -> None
      | h :: t -> v <- t; Some h

    method push hd =
      v <- hd :: v
  end

  let s = new istack

  type 'a iterator = < get : 'a; has_value : bool; next : unit >
  
  class ['a] list_iterator init = object
    val mutable current : 'a list = init

    method has_value = not (List.is_empty current)

    method get =
      match current with
      | hd :: _ -> hd
      | [] -> raise (Invalid_argument "no value")

    method next = 
      match current with
      | _ :: tl -> current <- tl
      | [] -> raise (Invalid_argument "no value")
  end


  (* ['a] 类似于 GADT 中的 type a. 或者是 ‘a. 预先定义一个type在body中使用 
     只是这种声明不一致，让人烦恼罢了 *)
  class ['a] stack init = object
    val mutable v : 'a list = init

    method pop =
      match v with
      | [] -> None
      | h :: t -> v <- t; Some h
    
    method push hd =
      v <- hd :: v

    method iterator : 'a iterator = new list_iterator v
    
    method iter f =
      List.iter ~f v
    
    (* type quantifier *)
    (* 'b. can be read as for all 'b *)
    method fold : 'b. ('b -> 'a -> 'b) -> 'b -> 'b =
      fun f init -> List.fold ~f ~init v
    
    method fold' : type b. (b -> 'a -> b) -> b -> b =  
      fun f init -> List.fold ~f ~init v
    
    (* method fold'' (type b) (f:(b -> 'a -> b)) (init:b) : b =  
      List.fold ~f ~init v   *)
  end

  (* inheritance *)
  class sstack init = object
    inherit [string] stack init 

    method print =
      List.iter ~f:Stdio.print_endline v

  end

  (* override *)
  class double_stack init = object
    inherit [int] stack init as super

    method push hd =
      super#push (hd * 2)
  end

end


(* class type *)

module Stack = struct
  (* define type 'a stack = < obj > *)
  class ['a] stack init = object
    val mutable v : 'a list = init

    method pop =
      match v with
      | [] -> None
      | h :: t -> v <- t; Some h
    
    method push hd =
      v <- hd :: v
  end

  type 'a t = 'a stack

  let make init = new stack init
end

module AbstractStack : sig
  type 'a t = < pop: 'a option; push: 'a -> unit >
  val make : 'a list -> 'a t 

end = Stack


module VisibleStack : sig

  type 'a t = < pop: 'a option; push: 'a -> unit >

  class ['a] stack : 'a list -> object
    val mutable v : 'a list
    method pop : 'a option
    method push : 'a -> unit
  end

  val make : 'a list -> 'a t
end = Stack


(* open recursive *)
type doc =
  | Heading of string
  | Paragraph of text_item list
  | Definition of string list_item list

and text_item =
  | Raw of string
  | Bold of text_item list
  | Enumerate of int list_item list
  | Quote of doc

and 'a list_item = 
  { tag : 'a;
    text: text_item list }

(* 不用class 也不是不可以做 *)
let rec doc acc d = match d with
  | Heading _ -> acc
  | Paragraph text -> List.fold ~f:(text_item) ~init:acc text
  | Definition list -> List.fold ~f:(list_item) ~init:acc list
and text_item acc t =
  match t with
  | Raw _ -> acc
  | Bold text -> List.fold ~f:text_item ~init:acc text
  | Enumerate list -> List.fold ~f:list_item ~init:acc list
  | Quote d -> doc acc d
and list_item : 'b. 'a -> 'b list_item -> 'a =
fun acc {tag = _;text} ->
  List.fold ~f:text_item ~init:acc text


module OpenRecursion = struct
(* bind self to current object *)
  class ['a] folder = object(self)
  (* do nothing with acc *)
    method doc acc = function
      | Heading _ -> acc
      | Paragraph text -> List.fold ~f:self#text_item ~init:acc text
      | Definition list -> List.fold ~f:self#list_item ~init:acc list

    method list_item : 'b. 'a -> 'b list_item -> 'a =
      fun acc {tag = _;text} ->
        List.fold ~f:self#text_item ~init:acc text
        
    method text_item acc = function
      | Raw _ -> acc
      | Bold text -> List.fold ~f:self#text_item ~init:acc text
      | Enumerate list -> List.fold ~f:self#list_item ~init:acc list
      | Quote doc -> self#doc acc doc

  end
  
  class counter = object
    inherit [int] folder as super

    method list_item acc li = acc

    (* count Bold *)
    method text_item acc ti = 
      let acc = super#text_item acc ti in
      match ti with
      | Bold _ -> acc + 1
      | _ -> acc
  end

  let count_doc = (new counter)#doc


  class ['a] folder2 = object(self)
  (* do nothing with acc *)
    method doc acc = function
      | Heading str -> self#heading acc str
      | Paragraph text -> self#paragraph acc text
      | Definition list -> self#definition acc list

    method list_item : 'b. 'a -> 'b list_item -> 'a =
      fun acc {tag = _;text} ->
        List.fold ~f:self#text_item ~init:acc text
        
    method text_item acc = function
      | Raw str -> self#raw acc str
      | Bold text -> self#bold acc text
      | Enumerate list -> self#enumerate acc list
      | Quote doc -> self#quote acc doc
    
    method private heading acc _str = acc 
    method private paragraph acc text = 
      List.fold ~f:self#text_item ~init:acc text
    method private definition acc list =
      List.fold ~f:self#list_item ~init:acc list

    method private raw acc _str = acc
    method private bold acc text = 
      List.fold ~f:self#text_item ~init:acc text
    method private enumerate acc list = 
      List.fold ~f:self#list_item ~init:acc list
    method private quote acc doc = self#doc acc doc
  end
  
  let f : 
    < doc : int -> doc -> int;
      list_item : 'a . int -> 'a list_item -> int;
      text_item : int -> text_item -> int >  = new folder2

  
  class counter_with_private_method = object
    inherit [int] folder2 as super
    
    method list_item acc li = acc

    method private bold acc txt =
      let acc = super#bold acc txt in
      acc + 1
  end

  class counter_with_sig : object
    (* without ;   !!!! *)
    method doc : int -> doc -> int
    method list_item : 'a . int -> 'a list_item -> int
    method text_item : int -> text_item -> int
  end =object
    inherit [int] folder2 as super
    
    method list_item acc li = acc

    method private bold acc txt =
      let acc = super#bold acc txt in
      acc + 1
  end



end


