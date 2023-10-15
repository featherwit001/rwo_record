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

(* explicit classs type to omit private method *)
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


module BinaryMethod = struct
  class square w = object(self : 'self)
    method width = w
    method area = Float.of_int (self#width * self#width)
    method equals (other : 'self) = 
        other#width = self#width
  end

  class circle r = object(self : 'self)
    method radius = r
    method area = 3.14 *. Float.of_int (self#radius) **. 2.0
    method equals (other : 'self) = 
        other#radius = self#radius
  end
end



module Shape : sig 
  (* shape_repr are also exposed for the method repr,
      but it is also hidden its definition *)
  type shape_repr
  (* method repr have to expose but can be hidden its definition *)
  type shape =  
    < repr : shape_repr; equals : shape -> bool; area : float >
  class square : int -> 
    object
      method width : int
      method area : float
      method repr : shape_repr
      method equals : shape -> bool
      method larger : shape -> bool
    end
  class circle : int -> 
    object
      method radius : int
      method area : float
      method repr : shape_repr
      method equals : shape -> bool
    end
  class square' :
  int ->
  object
    method area : float
    method larger : shape -> bool
    method width : int
  end
end = struct
  (* extensible variants *)
  type shape_repr = ..

  type shape_repr += Square of int

  type shape =  
    < repr : shape_repr; equals : shape -> bool; area : float >

  class square w = object(self)
    method width = w
    method area = Float.of_int (self#width * self#width)
    method repr = Square self#width
    (* not 'self but shape *)
    method equals (other : shape) =
      match (self#repr, other#repr) with
      | Square x, Square x' -> Int.(=) x x'
      | _ -> false

    (* just use partial information about the object 
       not need method repr and type shape_repr *)
    method larger (other : shape ) = Float.(self#area > other#area)
  end

  type shape_repr += Circle of int 

  class circle r = object(self : 'self)
    method radius = r
    method area = 3.14 *. Float.of_int (self#radius) **. 2.0
    method repr = Circle self#radius
    method equals (other : shape) = 
      match self#repr, other#repr with
      | Circle r, Circle r' -> Int.(=) r r'
      | _ -> false
  end

  let () = assert (not ((new square 10)#equals ((new square 5) :> shape)))
  let () = assert ((new square 10)#equals ((new square 10) :> shape))
  let () = assert (not ((new square 10)#equals ((new circle 5) :> shape)))


  class square' w = object(self)
    method width = w
    method area = Float.of_int (self#width * self#width)
    (* just use partial information about the object 
       not need method repr and type shape_repr *)
    method larger (other : shape ) = Float.(self#area > other#area)
  end

  let () = assert ((new square' 10)#larger ((new circle 5) :> shape))
  let () = assert (not ((new square' 10)#larger ((new circle 10) :> shape)))
  let () = assert (not ((new square' 10)#larger ((new square 10) :> shape)))
end


(* async_graphics need Core 14.0 but the whole book depends on Core 15.0 
   包管理精确到版本号是很重要的 like Rust cargo.toml*)

(* module Virtual = struct
  open Core
  open Async
  open Async_graphics

  type drawable = < draw: unit >

  class virtual shape x y = object(self)
  method virtual private contains: int -> int -> bool

  val mutable x: int = x
  method x = x

  val mutable y: int = y
  method y = y

  method on_click ?start ?stop f =
    on_click ?start ?stop
      (fun ev ->
         if self#contains ev.mouse_x ev.mouse_y then
           f ev.mouse_x ev.mouse_y)

  method on_mousedown ?start ?stop f =
    on_mousedown ?start ?stop
      (fun ev ->
         if self#contains ev.mouse_x ev.mouse_y then
           f ev.mouse_x ev.mouse_y)
  end

  class square w x y = object
    inherit shape x y
  
    val mutable width = w
    method width = width
  
    method draw = fill_rect x y width width
  
    method private contains x' y' =
      x <= x' && x' <= x + width &&
      y <= y' && y' <= y + width
  end

  class circle r x y = object
    inherit shape x y
  
    val mutable radius = r
    method radius = radius
  
    method draw = fill_circle x y radius
  
    method private contains x' y' =
      let dx = x' - x in
      let dy = y' - y in
        dx * dx + dy * dy <= radius * radius
  end

  class growing_circle r x y = object(self)
    inherit circle r x y
    (* use an object's method during instantiation
      initializer is an expression 
      that will be executed during instantiation 
      but after the object has been created
      so it can use object's methods and val s*)
    initializer
      self#on_click (fun _x _y -> radius <- radius * 2) 

    initializer
      radius <- radius * 2 
  end

  (* Mixin : 
     a virtual class that 
      implements a feature based on another one.
     its initializer is the loader handler for the subclasses *)
  (* class draggable based on class shape *)
  class virtual draggable = object(self)
  method virtual on_mousedown:
    ?start:unit Deferred.t ->
    ?stop:unit Deferred.t ->
    (int -> int -> unit) -> unit
  val virtual mutable x: int
  val virtual mutable y: int

  (* -------above is virtual methods and fields--------- *)
  (* -------under is method to add -------------------- *)
  val mutable dragging = false
  method dragging = dragging

  initializer
    self#on_mousedown
      (fun mouse_x mouse_y ->
         let offset_x = x - mouse_x in
         let offset_y = y - mouse_y in
         let mouse_up = Ivar.create () in
         let stop = Ivar.read mouse_up in
         dragging <- true;
         on_mouseup ~stop
           (fun _ ->
              Ivar.fill mouse_up ();
              dragging <- false);
         on_mousemove ~stop
           (fun ev ->
              x <- ev.mouse_x + offset_x;
              y <- ev.mouse_y + offset_y))
  end

  class small_square = object
    inherit square 20 40 40
    inherit draggable
  end

  (* class animated is a mixin based on class shape *)
  class virtual animated span = object(self)
    method virtual on_click:
      ?start:unit Deferred.t ->
      ?stop:unit Deferred.t ->
      (int -> int -> unit) -> unit
    val mutable updates: (int -> unit) list = []
    val mutable step = 0
    val mutable running = false

    method running = running

    method animate =
      step <- 0;
      running <- true;
      let stop =
        Clock.after span
        >>| fun () -> running <- false
      in
      Clock.every ~stop (Time.Span.of_sec (1.0 /. 24.0))
        (fun () ->
          step <- step + 1;
          List.iter ~f:(fun f -> f step) updates
        )

    initializer
      self#on_click (fun _x _y -> if not self#running then self#animate)
  end

  class my_circle = object
    inherit circle 20 50 50
    inherit animated Time.Span.second
    (* initailizer to add list *)
    initializer updates <- [fun _ -> x <- x + 5]
  end
  
  (* or mixin to add list  *)
  class virtual linear x' y' = object
    val virtual mutable updates: (int -> unit) list
    val virtual mutable x: int
    val virtual mutable y: int
  
    initializer
      let update _ =
        x <- x + x';
        y <- y + y'
      in
      updates <- update :: updates
  end

  let pi = (Float.atan 1.0) *. 4.0 

  class virtual harmonic offset x' y' = object
    val virtual mutable updates: (int -> unit) list
    val virtual mutable x: int
    val virtual mutable y: int
  
    initializer
      let update step =
        let m = Float.sin (offset +. ((Float.of_int step) *. (pi /. 64.))) in
        let x' = Float.to_int (m *. Float.of_int x') in
        let y' = Float.to_int (m *. Float.of_int y') in
        x <- x + x';
        y <- y + y'
      in
      updates <- update :: updates
  end

  class my_square x y = object
    inherit square 40 x y
    inherit draggable
    inherit animated (Time.Span.of_int_sec 5)
    inherit linear 5 0
    inherit harmonic 0.0 7 ~- 10
  end

  let my_circle = object
    inherit circle 30 250 250
    inherit animated (Time.Span.minute)
    inherit harmonic 0.0 10 0
    inherit harmonic (pi /. 2.0) 0 10
  end
  
end





module Initializers = struct
  class obj x = 
    (* before creating object *)
    let () = Stdio.printf "Creating obj %d\n" x in
    object
      (* during creating object *)
      val field = Stdio.printf "Initializing field\n"; x
    end 
end


module Test_for_val = struct
  (* val define a field which could be function and value *)
  class value = object
    val f = fun () -> Printf.sprintf "hello"
  end

end
 *)
