#use "APinter.ml";;

(*Ce module me sert a eviter le delais d'une seconde impose par OCAML*)

module Unix = UnixLabels ;;

(*<-------------------------------------------------------->*)


(*On rentre ensuite ce temps dans une varaivle pour le reutiliser plus simplement*)

let time(sec : float) = ignore (Unix.select [] [] [] sec);;
(*<-------------------------------------------------------->*)


(*On commence par definir les types que l'on va utiliser*)

type t_dir = HAUT | BAS | DROITE | GAUCHE ;;

(*<-------------------------------------------------------->*)


(*On definit maintenant les fonctions necessaires*)

let board : int array = [|25; 25|];;
let dir : t_dir ref = ref HAUT;;
let dead : bool ref = ref false;;
let score : int ref = ref 0;;
let snake : int array array ref = ref [|[|board.(0)/2; board.(0)/2|]; [|board.(0)/2 -1; board.(1)/2|]; [|board.(1)/2 -2; board.(1)/2|]|];;
let star_life : int ref = ref 0;;
let speed : float ref = ref 0.2;;
let dure : float ref = ref 0.;;
let color_snake : t_color ref = ref blue;;
let temps_bonus : float ref = ref 0.;;
let bonus_pris : bool ref = ref false;;
let chance_bonus : int ref = ref 10;;

(*<-------------------------------------------------------->*)


(*Fonctions de parametres du snake et de jeu*)

let erase_snake_tail() : unit = set_color(black);
       fill_rect((!snake.(arr_len(!snake)-1)).(0)*25,
                 (!snake.(arr_len(!snake)-1)).(1)*25, 25, 25)
;;

let draw_snake() : unit =
  set_color(!color_snake);
  fill_rect((!snake.(0).(0))*25,(!snake.(0).(1))*25, 25, 25);
  set_color(grey);
  fill_rect((!snake.(0).(0))*26,(!snake.(0).(1))*25, 4, 6);
  set_color(!color_snake);
  for i = 1 to (arr_len(!snake) - 1)
  do fill_rect((!snake.(i).(0))*25,(!snake.(i).(1))*25, 25, 25)
  done ;
;;

let dir_change() : unit =
  let (i, j) : int * int = mouse_pos() in
  let v : t_dir =
    if abs(i/25 - !snake.(0).(0)) < abs(j/25 - !snake.(0).(1))
    then 
      if j/25 > !snake.(0).(1)
      then HAUT
      else BAS
    else
      if i/25 < !snake.(0).(0)
      then DROITE
      else GAUCHE
  in
    if (!dir = HAUT && v = BAS) || (!dir = BAS && v = HAUT)
    then if i/25 < !snake.(0).(0) then dir := DROITE else dir := GAUCHE
    else 
      if (!dir = DROITE && v = GAUCHE) || (!dir = GAUCHE && v = DROITE)
      then if j/25 > !snake.(0).(1) then dir := HAUT else dir := BAS
      else dir := v
;;

let move() : unit =
  if (!dir = GAUCHE) then (erase_snake_tail();
                          for i = (arr_len(!snake) -1) downto 1
                          do (!snake.(i).(0) <- !snake.(i-1).(0);
                              !snake.(i).(1) <- !snake.(i-1).(1))
                          done ;
                          !snake.(0).(0) <- !snake.(0).(0) + 1 )
  else (if (!dir = DROITE) then (erase_snake_tail();
                          for i = (arr_len(!snake) -1) downto 1
                          do (!snake.(i).(0) <- !snake.(i-1).(0);
                              !snake.(i).(1) <- !snake.(i-1).(1))
                          done ;
                           !snake.(0).(0) <- !snake.(0).(0) - 1)
  else (if (!dir = HAUT) then (erase_snake_tail();
                          for i = (arr_len(!snake) -1) downto 1
                          do (!snake.(i).(0) <- !snake.(i-1).(0);
                              !snake.(i).(1) <- !snake.(i-1).(1))
                          done ;
                         !snake.(0).(1) <- !snake.(0).(1) + 1)
  else (erase_snake_tail();
        for i = (arr_len(!snake) -1) downto 1
        do (!snake.(i).(0) <- !snake.(i-1).(0);
            !snake.(i).(1) <- !snake.(i-1).(1))
        done ;
       !snake.(0).(1) <- !snake.(0).(1) - 1
       )));
  draw_snake()
;;

(*<-------------------------------------------------------->*)


(*Fonction qui verifie que le snake ne soit pas mort*)

let death_check() : unit =
  if (!snake.(0).(0) > board.(0) || !snake.(0).(0) = 0 || !snake.(0).(1) > board.(1) || !snake.(0).(1) = 0)
  then (dead := true)
;;

let tail_check() : unit = for i = 1 to (arr_len(!snake) -1)
                          do
                            if (!snake.(i).(0) = !snake.(0).(0) && !snake.(i).(1) = !snake.(0).(1))
                            then dead:=true
                          done ;
;;

(*<-------------------------------------------------------->*)


(*Fonction qui trace la partie noire et les contours de la zones de jeu*)

let board_print() : unit =
  (fill_rect(0, 0, 25*board.(0)+200, 25*board.(1)+200);
                     set_color(white);
                     fill_rect(0,0,board.(0)*25 +(2*25),25);
                     fill_rect(0,0,25,board.(1)*25 +(2*25));
  fill_rect(0,board.(1)*25 +25,board.(0)*25 +(2*25),25);
  fill_rect(board.(0)*25 +25,0,25,board.(1)*25 +(2*25))
  )
;;

(*<-------------------------------------------------------->*)


(*Fonctions pour les pommes*)

let rec spawn_apple() : int array =
  let temp : int array ref = ref[|rand_int(1, board.(0) -1);rand_int(1, board.(1) -1)|] in
  for i = 0 to (arr_len(!snake) -1)
  do (if !temp = !snake.(i)
      then (temp := [|0;0|]))
  done ;
  if !temp = [|0;0|]
  then spawn_apple()
  else !temp
;;
let apples : int matrix = [|spawn_apple(); spawn_apple(); spawn_apple()|];;
let apple_f() : unit =
  for i = 0 to 2
  do if !snake.(0) = apples.(i)
    then (apples.(i) <- spawn_apple();
          set_color(green);
          fill_rect((apples.(i).(0))*25, (apples.(i).(1))*25, 25, 25);
          score := !score + 100;
          Graphics.sound 440 10;
          snake := Array.append !snake [|[|0; 0|]|])
  done ;
;;

let apple_int() : unit =
  (
    set_color(green);
    fill_rect((apples.(0).(0))*25, (apples.(0).(1))*25, 25, 25);
    fill_rect((apples.(1).(0))*25, (apples.(1).(1))*25, 25, 25);
    fill_rect((apples.(2).(0))*25, (apples.(2).(1))*25, 25, 25)
  )
;;

(*<-------------------------------------------------------->*)


(*Ma fonction étoiles *)

let rec spawn_star() : int array =
  let temp_star : int array ref = ref[|rand_int(1, board.(0) -1);rand_int(1, board.(1) -1)|] in
  for i = 0 to (arr_len(!snake) -1)
  do (if !temp_star = !snake.(i)
      then (temp_star := [|0;0|]))
  done ;
  if !temp_star = [|0;0|]
  then spawn_star()
  else !temp_star
;;
let stars : int matrix = [|spawn_star(); spawn_star(); spawn_star()|];;
let star() : unit =
  if !snake.(0) = stars.(0)
    then (score := !score + 1000;
          Graphics.sound 440 10;
          Graphics.sound 300 10;
          Graphics.sound 440 10;
          speed := 0.1;
          temps_bonus := 0.;
          bonus_pris := true;
          chance_bonus := !chance_bonus + 10;
          star_life := 1;
          color_snake := (yellow);
          snake := Array.append !snake [|[|0; 0|]|])
;;

let star_int() : unit =
  (
    set_color(yellow);
    fill_rect((stars.(0).(0))*25, (stars.(0).(1))*25, 25, 25);
  )
;;

let check_bonus() : unit =
  if !temps_bonus > 10.
  then
    (
      color_snake := (blue);
      speed := 0.2;
      star_life := 0;
      bonus_pris := false;
    )
;;

(*<-------------------------------------------------------->*)


(*Ma fonction perdu*)

let lose() : unit =
  Graphics.sound 440 250;
  set_color(red);
  fill_rect(0, 0, 25*board.(0)+200, 25*board.(1)+200);
  time(1.);
  close_graph()
;;

(*<-------------------------------------------------------->*)


(*Fonction qui rassemble l'ensemble du jeu *)

let snake_game() : unit =
  dead := false;
  open_graph(25* board.(0) +100, 25* board.(1) +100);
  set_color(black);
  board_print();
  apple_int();
  set_color(black);
  draw_string("             Un jeu Lillo Gavois, retrouvez-moi sur lillo.best");
  set_color(!color_snake);
  while !dead = false
  do time(!speed);
     dir_change();
     move();
     star();
     apple_f();
     death_check();
     tail_check();
     dure := !dure +. !speed;
     temps_bonus := !temps_bonus +. 0.1;
     check_bonus();
     if !score > 100 && !star_life = 0 && rand_int(1,(!chance_bonus)) = 2
     then
       (
         star_int();
         star_life := 1
       );
     if !bonus_pris = true
     then
       (
         let score_string : string = "Score : "^string_of_int(!score)^" / Temps restant bonus : "^string_of_float(10. -. !temps_bonus) in
         Graphics.set_window_title score_string
       )
     else
       (
         let score_string : string = "Score : "^string_of_int(!score) in
         Graphics.set_window_title score_string
       )
  done ;
  print_string("Bravo ! Votre score est de : ");
  print_int(!score);
  print_newline();
  print_string("Vous êtes resté(e) en vie : ");
  print_float(!dure);
  print_string(" secondes.");
  print_newline();
  lose()
;;

(*<-------------------------------------------------------->*)

snake_game();;

