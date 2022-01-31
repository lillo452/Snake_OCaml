
(* ------------------------ *)
(*        listes            *)
(* ------------------------ *)



let len(l : 'a list) : int = List.length l ;;

let fst(l : 'a list) : 'a =
  match l with
    [] -> failwith "error fst : list is empty"
  | hd::_ -> hd
;;

let rec lst(l : 'a list) : 'a =
  match l with
    [] -> failwith "error lst : list is empty"
    | hd::[] -> hd
    | _::tail -> lst(tail)
;;

let nth(l, k : 'a list * int) : 'a = 
  let rec nth1(l, k) =
    match l with
      []->  failwith "error nth : index out of bounds"
    | hd::tail -> if k = 0 then hd else nth1(tail,k-1)
  in
    if k < 0
    then failwith "error  nth : index must be positive"
    else nth1(l,k)
;;

let add_fst(l, e : 'a list * 'a) : 'a list = e::l ;;

let rec add_lst(l, e : 'a list * 'a) : 'a list =
  match l with
    [] -> [e]
  | hd::tail -> hd::add_lst(tail,e)
;;

let add_nth(l, e, k  : 'a list * 'a * int) : 'a list =
  let rec add_nth1(l, e, k) =
    match l with
      [] -> [e]
    | hd ::tail -> if k = 0 then e::l else hd::add_nth1(tail, e, k-1)
  in 
    if k < 0
    then failwith "error add_nth : index must be positive"
    else
      if k > len(l)
      then failwith "error add_nth : index out of bounds"
      else add_nth1(l,e,k)
;;

let rem_fst(l : 'a list) : 'a list = 
  match l with
    [] -> failwith "error rem_fst : list is empty"
    | _::tail -> tail
;;

let rec rem_lst(l : 'a list) : 'a list =
  match l with
    [] -> failwith "error rem_lst : list is empty"
    | [x] -> []
    | x::tail -> x::rem_lst(tail)
 ;;

let rem_nth(l, k : 'a list * int) : 'a list =
  let rec rem_nth1(l, k) =
    match l with
    | [] -> failwith "error rem_nth : index out of bounds"
    | hd:: tail -> if k = 0 then tail else hd::rem_nth1(tail, k-1)
  in
    if k < 0 
    then failwith "error rem_nth : index must be positive"
    else rem_nth1(l,k)
;;

let concat(l1, l2 : 'a list * 'a list) = l1 @ l2 ;;


(* ------------------------ *)
(*        tableaux          *)
(* ------------------------ *)

let arr_len(t : 'a array) : int = Array.length t ;;

let arr_make(n, v : int * 'a) : 'a array = 
  if n < 0
  then failwith("erreur arr_make ; parametre invalide")
  else Array.make n v 
;;

type 'a matrix = 'a array array ;;

let mat_make(n, m, v : int * int * 'a) : 'a matrix = 
  if n < 0 || m < 0
  then failwith("erreur mat_make ; parametre invalide")
  else Array.make_matrix n m v 
;;


(* ------------------- *)
(*      aleatoire      *)
(* ------------------- *)

let rand_init() : unit = Random.self_init() ;;

let rand_init_expl(n : int) : unit = Random.init(n) ;;

let rand_int_0(n : int) : int = Random.int(n+1) ;;

let rand_int(n, p : int * int) : int = Random.int(p-n + 1) + n ;;


(* ------------------------ *)
(*    lecture caractere     *)
(* ------------------------ *)

let read_char() : char =
  let s : string ref = ref "" and the_end : bool ref = ref false in
    (
    while not(!the_end) 
    do
      s:= read_line() ; 
      if String.length(!s) = 0
      then 
        (
        print_string("erreur read_char : aucun caractere saisi") ;
        print_newline() ;
        )
      else the_end := true;
    done ;
    (!s).[0] ;
    )
;;

(* ------------------------- *)
(* conversion char -> string *)
(* ------------------------- *)

let string_of_char(c : char) : string = Char.escaped c ;;


(* ------------------------ *)
(*    longueur string       *)
(* ------------------------ *)

let string_length(s : string) : int = String.length s ;;


(* ------------------------ *)
(*  pause durant execution  *)
(* ------------------------ *)
(* ------------------------ *)
let wait(n : int) : unit =
 Unix.sleep(n)
;;



(* ------------------------ *)
(*        graphique         *)
(* ------------------------ *)

let open_graph(dx, dy : int * int) : unit = 
  if Sys.os_type = "Unix" then  
    let s = ":0 "^string_of_int(dx)^"x"^string_of_int(dy) in
      Graphics.open_graph s
  else
    let s = string_of_int(dx)^"x"^string_of_int(dy) in
      Graphics.open_graph s
;;

let close_graph() : unit = Graphics.close_graph() ;;

let clear_graph() : unit = Graphics.clear_graph() ;;

let resize_window(x, y : int * int) : unit = Graphics.resize_window x y ;;


let moveto(x, y : int * int) : unit = Graphics.moveto x y ;;

let lineto(x, y : int * int) : unit = Graphics.lineto x y ;;

let plot(x, y : int * int) : unit = Graphics.plot x y ;;

let current_point() : int * int = Graphics.current_point() ;;

let draw_poly_line(t : (int * int) array) : unit = Graphics.draw_poly_line t ;;

let draw_circle(x, y, r : int * int * int) : unit = Graphics.draw_circle x y r ;;

let draw_ellipse(x, y, dx, dy : int * int * int * int) : unit = 
          Graphics.draw_ellipse x y dx dy 
;;

let draw_rect(x, y, dx, dy : int * int * int * int) : unit = 
  if Sys.os_type = "Unix" then  
    Graphics.draw_rect x y (dx- 1) (dy - 1)
  else
    Graphics.draw_rect x (y+1) (dx-1) (dy-1)
;;

let fill_rect(x, y, dx, dy : int * int * int * int) : unit = 
  if Sys.os_type = "Unix" then  
    Graphics.fill_rect x y (dx- 1) (dy - 1)
  else
    Graphics.fill_rect x y dx dy
;;

let fill_poly(t : (int * int) array) : unit = Graphics.fill_poly t ;;

let fill_circle(x, y, r : int * int * int) : unit = Graphics.fill_circle x y r ;;


let fill_ellipse(x, y, dx, dy : int * int * int * int) : unit = 
          Graphics.fill_ellipse x y dx dy 
;;

let set_line_width(e : int) : unit = Graphics.set_line_width e ;;

let draw_string(s : string) : unit = Graphics.draw_string s ;;

let set_text_size(n : int) : unit = 
  let s = "-*-courier-medium-r-*-*-"^string_of_int(n)^"-*"
  in Graphics.set_font s ;;



type t_color = Graphics.color ;;

let black : t_color = Graphics.black ;;
let blue : t_color = Graphics.blue ;;
let red : t_color = Graphics.red ;;
let green : t_color = Graphics.green ;;
let white : t_color = Graphics.white ;;
let yellow : t_color = Graphics.yellow ;;
let cyan : t_color = Graphics.cyan ;;
let magenta : t_color = Graphics.magenta ;;
let grey : t_color = 128 * 256 * 256 + 128 * 256 + 128 ;;

let color_of_rgb(r, g, b : int * int * int) : t_color =
  let valid(x : int) : bool = ((0 <= x) && x <= 255) in
    if not(valid(r)) ||  not(valid(g)) || not(valid(b))
    then failwith("erreur color_of_rgb : valeurs invalides")
    else Graphics.rgb r g b
;;



let set_color(color : t_color) : unit = Graphics.set_color color ;;

(* ------------------------ *)
(*   controle evenements    *)
(* ------------------------ *)


let key_pressed() : bool =
  Graphics.key_pressed()
;;

let read_key() : char =
  Graphics.read_key()
;;

let mouse_pos() : int * int =
  Graphics.mouse_pos()
;;

let button_down() : bool = 
  Graphics.button_down()
;;



                   
