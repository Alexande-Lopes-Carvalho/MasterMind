type event = Button_down | Button_up | Key_pressed | Mouse_motion | Poll;;

type status =
  { mouse_x : int;
    mouse_y : int;
    button : bool;
    keypressed : bool;
    key : char};;

#load "graphics.cma";;
open Graphics;;


Graphics.open_graph " 1000x1000";;
(*                       l x h
 Graphics.open_graph "";; <=> taille prédef
 Graphics.close_graph();; *)


Graphics.set_window_title " Test fenetre 1";;

(*
(* Drawing *)

set_line_width 1;; - largeur lignes

let gris = Graphics.rgb 50 110 200;;
Graphics.set_color Graphics.red;
Graphics.draw_rect 5 5 500 200;
(* x y longueur largeur *)


Graphics.set_color cyan;
Graphics.draw_rect 50 100 200 200;;

Graphics.rgb 190 50 100;;
Graphics.draw_rect 100 100 200 200;;
(* Graphics.fill_rect 0 0 90 110;;  centre x centre y largeur hauteur  *)

Graphics.lineto 100 160;; (*trace un trait de la position actuelle vers osition en paramètre*)

Graphics.draw_circle 500 500 50;;
Graphics.fill_circle 500 500 30;; (* centre x y et rayon r*)

Graphics.draw_circle 400 450 50;;
Graphics.fill_circle 400 450 30;;

Graphics.draw_ellipse 50 50 25 30  ;;(* centre x y rx ry *)
Graphics.fill_ellipse 50 50 10 15;;

Graphics.draw_arc 160 160 80 110 50 360;;
(* Graphics.draw_arc x y rx ry a b;; *)


  Graphics.size_x();;
  Graphics.size_y();;

Graphics.current_point();; (* position actuelle*)

Graphics.moveto 50 50;;
(* Graphics.plot a b;;  - dessine 1 pt
Graphics.lineto a b;;   - dessine 1 ligne   *)

Graphics.current_point();;


Graphics.set_color black; *)
let gris = Graphics.rgb 191 191 191;;
let rouge = Graphics.rgb 250 0 0;;
let brown = Graphics.rgb 222 184 135;;

set_line_width 1;;


set_color rouge;
Graphics.draw_rect 10 10 65 30;;
Graphics.moveto 20 20;;
Graphics.draw_string "Quitter";;



set_color gris;
Graphics.draw_rect 900 10 65 30;;
Graphics.moveto 910 20;;
Graphics.draw_string "Options";;

set_color gris;
Graphics.draw_rect 8 8 980 980;;

set_line_width 2;;
set_color brown;
Graphics.draw_rect 40 45 920 940;;



Graphics.moveto 400 400;;
set_color black;;

Graphics.draw_string "Test ecriture";;




(* Graphics.set_text_size 30;;  Graphics.text_size " get size";;*)


(* Interactions user

Graphics.read_key();; (* attend que l'utilisateur appuie sur une touche du clavier pour continuer l'exécution du programme.
et rend le nom de la touche pressée. *)
Graphics.key_pressed();; (* -> boolean*)
Graphics.button_down();; (* bouton gauche pressé = true ou false *)
Graphics.mouse_pos();;

current_point;;

Graphics.Button_down;;  (* un bouton de la souris est pressé *)
Graphics.Button_up;; (* un bouton de la souris est relâché *)
Graphics.Key_pressed;; (* une touche du clavier est pressée *)
Graphics.Mouse_motion;; (* la souris est déplacée *)

Vous pouvez demander à Caml d'attendre qu'une liste d'évènements se produise en utilisant
Graphics.wait_next_event suivi d'une liste d'évènements. L'exécution du programme est suspendue
jusqu'à ce qu'un des évènements de la liste se produise. Caml rend ensuite une liste d'informations :
les coordonnées de la souris, deux booléens indiquant si un bouton de la souris a été pressé et si une
touche du clavier a été pressée, si c'est le cas la touche est indiquée.
*)
let clic() =
  let att = wait_next_event [Button_down] in
   let abs = att.mouse_x and ord = att.mouse_y in abs,ord;;

let quit_game () = let posx = fst(mouse_pos()) and posy = snd(mouse_pos()) in
if ( posx > 10 && posx < 75 )&&( posy > 10 && posy < 40 ) then close_graph() else print_string "NF";;
(* pos quit 10 10 65 30 *)



print_string "here1" ;;

read_key();; (* -> char *)

key_pressed();; (* T / F *)

current_x;;


print_string "here2" ;;
button_down();;


mouse_pos ();; (* <=> Graphics.mouse_pos();; donne x y  de la souris *)

clic();;

fst( mouse_pos() );;
snd( mouse_pos() );;

clic();;



quit_game();;

(*
let quit_game_0 () =
if  ( fst(mouse_pos() ) > 10 && snd(mouse_pos() ) > 10 ) then close_graph() else print_string "NF";;
quit_game_0();;
*)

clic();;


clic();;
clic();;
clic();;
clic();;
clic();;

quit_game();;