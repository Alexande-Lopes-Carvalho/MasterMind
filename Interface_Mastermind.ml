(* http://www.france-ioi.org/algo/course.php?idChapter=551&idCourse=1811 *)
type event = Button_down | Button_up | Key_pressed | Mouse_motion | Poll;;

type status =
  { mouse_x : int;
    mouse_y : int;
    button : bool;
    keypressed : bool;
    key : char};;

#load "graphics.cma";;


open Graphics;;



#directory "+camlimages";;
#load "graphics.cma";;
#load "camlimages.cma";;

Graphics.open_graph " 1000x1000+400";;
(*                       l x h
 Graphi
 cs.open_graph "";; <=> taille prédef
 Graphics.close_graph();; *)

let eot() = close_graph();;

Graphics.set_window_title " Test fenetre 1";;

let gris = rgb 191 191 191;;
let rouge = rgb 250 0 0;;
let brown = rgb 222 184 135;;
let violet = rgb 255 0 255;;
set_line_width 1;;


set_color rouge;
draw_rect 10 10 65 30;;
moveto 20 20;;
draw_string "Quitter";;



set_color gris;
draw_rect 900 10 65 30;;
moveto 910 20;;
set_color black;
draw_string "Options";;


set_color gris;
draw_rect 8 8 980 980;;

set_line_width 2;;
set_color brown;
draw_rect 40 45 920 940;;



moveto 400 400;;
set_color black;;

draw_string "Test ecriture";;


(* Drawing *)

set_line_width 1;; (*- largeur lignes*)


(* x y longueur largeur *)

(*
set_color cyan;
draw_rect 50 100 200 200;;

Graphics.rgb 190 50 100;;
Graphics.draw_rect 100 100 200 200;;
(* Graphics.fill_rect 0 0 90 110;;  centre x centre y largeur hauteur  *)

lineto 100 160;; (*trace un trait de la position actuelle vers osition en paramètre*)

draw_circle 500 500 50;;
fill_circle 500 500 30;; (* centre x y et rayon r*)

draw_circle 400 450 50;;
fill_circle 400 450 30;;

draw_ellipse 450 250 25 30  ;;(* centre x y rx ry *)
fill_ellipse 450 250 10 15;;

Graphics.draw_arc 160 160 80 110 50 360;;
(* Graphics.draw_arc x y rx ry a b;; *)

set_color red;;
fill_rect 500 500 200 200;;

set_color cyan;;
fill_rect 640 600 50 50;;

set_color cyan;;
fill_rect 540 600 50 50;;

set_color brown;;
fill_rect 550 610 25 25;;

set_color brown;;
fill_rect 650 610 25 25;;

set_color green;;
fill_rect 600 500 125 20;;
*)
  size_x();;
  size_y();;

current_point();; (* position actuelle*)

moveto 50 50;;
(* Graphics.plot a b;;  - dessine 1 pt
Graphics.lineto a b;;   - dessine 1 ligne   *)

Graphics.current_point();;

moveto 150 80;;
draw_string "Combinaison";;

set_color black;;
let gris = rgb 191 191 191;;
let rouge = rgb 250 0 0;;
let brown = rgb 222 184 135;;


set_line_width 1;;


set_color rouge;
draw_rect 10 10 65 30;;
moveto 20 20;;
draw_string "Quitter";;


set_color gris;
draw_rect 900 10 65 30;;
moveto 910 20;;
draw_string "Options";;

set_color gris;
draw_rect 8 8 980 980;;

set_line_width 2;;
set_color brown;
draw_rect 40 45 920 940;;


let clicV0() =
  let att = wait_next_event [Button_down] in
   let abs = att.mouse_x and ord = att.mouse_y in abs,ord;;

   let clic() =
     let att = wait_next_event [Button_down] in
      let abs = att.mouse_x and ord = att.mouse_y in Printf.printf "abs = %d ; ord = %d\n" abs ord;;


let quit_game () = let posx = fst(mouse_pos()) and posy = snd(mouse_pos()) in
if ( posx > 10 && posx < 75 )&&( posy > 10 && posy < 40 ) then close_graph() else print_string "NF";;
(* pos quit 10 10 65 30 *)

let option_clic () = let posx = fst(mouse_pos()) and posy = snd(mouse_pos()) in
if ( posx > 900 && posx < 965 )&&( posy > 10 && posy < 40 ) then  print_string "Option_clicked" else print_string "NF2";;

set_color black;;
fill_circle 350 90 20;;
fill_circle 450 90 20;;
fill_circle 550 90 20;;
fill_circle 650 90 20;;

set_color green;;
draw_rect 200 125 600 820;;


print_string "here 0" ;;

set_color yellow;;
let draw_4_circle x y r espx espy =
draw_circle x y r ;
draw_circle (x+espx) y r ;
draw_circle (x+(2*espx)) y r ;
draw_circle (x+3*espx) y r ; ;;


set_color blue;;

let rec draw_table_circle dx dy r endx endy espx espy = match (dx,dy) with
|(x,y) when x<endx && y<endy -> draw_4_circle dx dy r espx espy ; draw_table_circle x (y+espy) r endx endy espx espy
|(x,y) when y=dy -> 0;;

draw_table_circle 350 180 20 665 940 100 80;; (* dessine les 10 lignes représentant les 10 essai du joueur *)

let clic_draw_circle() = let pos = wait_next_event [Button_down] in
let posx = fst(mouse_pos()) and posy = snd(mouse_pos()) in (fill_circle posx posy 20);;


let get_char () = read_key();;
get_char();;



set_color blue;;
clic_draw_circle();;
clic_draw_circle();;

let colorer x = set_color x;;

print_string " test color";

fst(mouse_pos());;
snd(mouse_pos());;

let rec choose_color_v0 () =
 print_string " Choisissez la couleur : r rouge v vert b bleu n noir j jaune ";
 let c = read_key(); and posx = fst(mouse_pos()) and posy = snd(mouse_pos())
  in match c with
   |'n' -> colorer black ; fill_circle posx posy 20
   |'v' -> colorer green ; fill_circle posx posy 20
   |'b' -> colorer blue ; fill_circle posx posy 20
   |'r' -> colorer red ; fill_circle posx posy 20
   |'j' -> colorer yellow ; fill_circle posx posy 20
   |_ -> choose_color_v0(); (* si choix incorrect on rapelle la fonction jusqu'a avoir une entrée correspondante aux choix possibles*)

;;

choose_color_v0();;



let rec choose_color_v1 i =
 print_string " Choisissez la couleur : r rouge v vert b bleu n noir j jaune ";
 let c = read_key(); and posx = fst(mouse_pos()) and posy = snd(mouse_pos())
  in match (c,i) with
   |(y,x) when x=4 -> ()
   |('n',x) -> colorer black ; fill_circle posx posy 20 ; choose_color_v1 (x+1)
   |('v',x) -> colorer green ; fill_circle posx posy 20 ; choose_color_v1 (x+1)
   |('b',x) -> colorer blue ; fill_circle posx posy 20 ; choose_color_v1 (x+1)
   |('r',x) -> colorer red ; fill_circle posx posy 20 ; choose_color_v1 (x+1)
   |('j',x) -> colorer yellow ; fill_circle posx posy 20 ; choose_color_v1 (x+1)
   |(_,x) -> choose_color_v1 x; (* si choix incorrect on rapelle la fonction jusqu'a avoir une entrée correspondante aux choix possibles*)

;;


choose_color_v1 0;;

let rec fill_grille i =

 print_string " Choisissez la couleur : r rouge v vert b bleu n noir j jaune ";
 let c = read_key(); and posx = fst(mouse_pos()) and posy = snd(mouse_pos())
  in match (c,i) with
   |(y,x) when x=4 -> ()
   |('n',x) -> colorer black ; fill_circle posx posy 20 ; fill_grille (x+1)
   |('v',x) -> colorer green ; fill_circle posx posy 20 ; fill_grille (x+1)
   |('b',x) -> colorer blue ; fill_circle posx posy 20 ; fill_grille (x+1)
   |('r',x) -> colorer red ; fill_circle posx posy 20 ; fill_grille (x+1)
   |('j',x) -> colorer yellow ; fill_circle posx posy 20 ; fill_grille (x+1)
   |(_,x) -> fill_grille x;

;;

colorer violet;;

let rec fill_grille_aux () = let posx = fst(mouse_pos()) and posy = snd(mouse_pos()) in
if ( posx > 330 && posx < 370 )&&( posy > 160 && posy < 200 ) then (fill_circle 350 180 20) else fill_grille_aux();;

fill_grille_aux();;

colorer green;;

let rec rempli_cercle() = let pos = wait_next_event [Button_down] in
let posx = fst(mouse_pos()) and posy = snd(mouse_pos()) in
if ( posx > 430 && posx < 470 )&&( posy > 240 && posy < 280 ) then (fill_circle 450 180 20) else rempli_cercle();;
rempli_cercle();;


colorer black;;


(*
let choose_color () =
 print_string " Choisissez la couleur : r rouge v vert b bleu n noir j jaune ";
 let c = read_key();
  in match c with
   |b -> set_color blue ;
   |n -> set_color black ;
   |r -> set_color red ;

let posx = fst(mouse_pos()) and posy = snd(mouse_pos()) in (fill_circle posx posy 20)
;;

choose_color();;
*)
print_endline " end test color";;


let rec clic_to_draw y = match y with
|x when x<0 -> 0
|x when x < 5 -> clic_draw_circle(); clic_to_draw(x+1)
|_-> 1;;

clic_to_draw 1;;


(*
let rec draw_invade dx dy r endx endy espx espy i = match i with
|0 -> draw_table_circle dx dy r endx endy espx espy
|x when x<40 -> draw_table_circle dx dy r endx endy espx espy ; i+1 ; draw_invade (dx+i*2) (dy+i*2) r endx endy (espx+5) (espy+5) (x+1)
;;

set_color green;;
draw_invade 200 125 20 670 900 100 100 0;; *)


(*
let rec menu() =
print_string "p pour une figure pleine \n v pour le contour seulement  \n 0 pour quitter "

match read_key() with
|'p' ->
|'v' ->

print_string "r pour rectangle \n c pour cercle  \n 0 pour quitter "
match read_key() with
|'r' ->
|'c' ->

;;

menu();;

*)

(*
let scan_int () = Scanf.scanf " %d" (fun x ->x);;

let cercle () =
   let x = scan_int() in
   let y = scan_int() in
   let r = scan_int() in (fill_circle x y r) ;;

cercle ();;

let rect () =
   let x = scan_int() in
   let y = scan_int() in
   let l = scan_int() in
   let k = scan_int() in (fill_rect x y l k) ;;
rect ();;

*)

print_string "here1" ;;
read_key();; (* -> char *)

key_pressed();; (* T / F *)

current_x;; (* position actuelle *)

print_string "here2" ;;
button_down();;

mouse_pos ();; (* <=> Graphics.mouse_pos();; donne x y  de la souris *)

clic();;
option_clic();;

print_string "hereb" ;;

fst( mouse_pos() );;
snd( mouse_pos() );;

clic();;

quit_game();;

clic();;

print_string "here3" ;;

clic();;

clic();;

print_string "clique" ;;

clic();;

quit_game();;


(*


(* charge une image quelconque (.jpg,.png... comme supporté par
   camlimages) vers une matrice de triplets (r,g,b) d'entiers :
   (int*int*int)*array*array *)
 let load_rgb_matrix name =
   let img = Images.load name [] in
  let gimg = Graphic_image.array_of_image img in
  let rgb color =
    let quot n = n mod 256, n / 256 in
    let b, rg = quot color in
    let g, r = quot rg in
    r, g, b
in  Array.map (Array.map rgb) gimg;;

  (* transforme une matrice de triplets (r,g,b) en une "image graphics"
     de type Graphics.image *)
 let to_graphics rgb_matrix =
  Graphics.make_image
    (Array.map
       ( Array.map (fun r g b -> Graphics.rgb r g b) )
       rgb_matrix);;


let map_matrix f matrix = Array.map (Array.map f) matrix;;

let invert_colors = map_matrix
  (fun r g b -> (255-r, 255-g, 255-b) ) ;;


let () =
  (* charge l'image donnée en argument : "./test truc.png" *)
  let "./index.jpeg" test = load_rgb_matrix Sys.argv.(1) in
  (* dessine l'image une première fois *)
  draw_image (to_graphics test) 0 0;
  ignore (read_key ());
  (* dessine l'image avec les couleurs inversées *)
  draw_image (to_graphics (invert_colors test)) 0 0;
  ignore (read_key ());

;;

*)
