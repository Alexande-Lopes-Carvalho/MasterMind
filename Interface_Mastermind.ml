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

Graphics.open_graph " 1000x1000+700";;
(*                       l x h
 open_graph "";; <=> taille prédef
 Graphics.close_graph();; *)

let eot() = close_graph();;

Graphics.set_window_title " Test fenetre 1";;

(* variables globales *)
let global_x_1 = 350 ;;
let global_y_1 = 180 ;;
let global_r =20 ;;
let global_esp_x = 100 ;; (* espace en x entre les centres de 2 cercles  *)
let global_esp_y = 80 ;; (* espace en y entre les centres de 2 cercles *)


let gris = rgb 191 191 191;;
let rouge = rgb 250 0 0;;
let brown = rgb 222 184 135;;
let violet = rgb 255 0 255;;
set_line_width 1;;

Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";

set_color blue;
moveto 150 80;;
draw_string "Combinaison";;

moveto  70 400;;
draw_string "b - bleu ";;
moveto  70 370;;
draw_string "c - cyan ";;
moveto  70 340;;
draw_string "j - jaune  ";;
moveto  70 310;;
draw_string "n - noir  ";;
moveto  70 280;;
draw_string "p - violet ";;
moveto  70 250;;
draw_string "r - rouge ";;
moveto  70 220;;
draw_string "v - vert ";;

set_color black;;
let gris = rgb 191 191 191;;
let rouge = rgb 250 0 0;;
let brown = rgb 222 184 135;;

set_line_width 1;;

set_color rouge;
draw_rect 10 10 100 30;;
moveto 20 15;;
draw_string "Quitter";;

set_color gris;
draw_rect 885 10 100 30;;
moveto 895 15;;
draw_string "Options";;

set_color gris;
draw_rect 8 8 980 980;;

set_line_width 2;;
set_color brown;
draw_rect 40 45 920 940;;


let quit_game () = let posx = fst(mouse_pos()) and posy = snd(mouse_pos()) in
if ( posx > 10 && posx < 110 )&&( posy > 10 && posy < 40 ) then close_graph() else print_string "NF";;

let option_clic () = let posx = fst(mouse_pos()) and posy = snd(mouse_pos()) in
if ( posx > 885 && posx < 985 )&&( posy > 10 && posy < 40 ) then  print_string "Option_clicked" else print_string "NF2";;

set_color black;;
fill_circle 350 90 20;;
fill_circle 450 90 20;;
fill_circle 550 90 20;;
fill_circle 650 90 20;;

set_color green;;
draw_rect 200 125 600 820;;


let draw_4_circle x y r espx =
draw_circle x y r ;
draw_circle (x+espx) y r ;
draw_circle (x+(2*espx)) y r ;
draw_circle (x+3*espx) y r ; ;;

let rec draw_table_circle dx dy r endx endy espx espy = match (dx,dy) with
|(x,y) when x<endx && y<endy -> draw_4_circle dx dy r espx ; draw_table_circle x (y+espy) r endx endy espx espy ;
|(x,y) when y=dy -> 0;;

set_color black;;
draw_table_circle global_x_1 global_y_1 global_r 665 940 global_esp_x global_esp_y;; (* dessine les 10 lignes représentant les 10 essais du joueur *)

let clic_draw_circle() = let pos = wait_next_event [Button_down] in
let posx = fst(mouse_pos()) and posy = snd(mouse_pos()) in (fill_circle posx posy 20);;

let clic() =
  let att = wait_next_event [Button_down] in
   let abs = att.mouse_x and ord = att.mouse_y in abs,ord;;

   let clicV() =
     let att = wait_next_event [Button_down] in
      let abs = att.mouse_x and ord = att.mouse_y in Printf.printf "abs = %d ; ord = %d\n" abs ord;;

let colorer x = set_color x;;

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
   |(_,x) -> choose_color_v1 x;
;;

(* choose_color_v1 0;; *)

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

let rec fill_grille_aux () = let posx = fst(mouse_pos()) and posy = snd(mouse_pos()) in
if ( posx > 330 && posx < 370 )&&( posy > 160 && posy < 200 ) then (fill_circle 350 180 20) else fill_grille_aux();;

let clicA() =
  let att = wait_next_event [Button_down] in
   let abs = att.mouse_x and ord = att.mouse_y in (abs,ord);;

let rec rempli_cercle() = let (posx,posy) = (clicA()) in
if ( posx > 430 && posx < 470 )&&( posy > 160 && posy < 200 ) then (fill_circle 450 180 20) else rempli_cercle();;


let rec rempli_un_cercle() =
let (posx,posy) = (clicA()) in
match (posx,posy) with
|(a,b) when ( posx > 330 && posx < 370 )&&( posy > 160 && posy < 200 ) ->  (fill_circle 350 180 20)
|(a,b) when ( posx > 430 && posx < 470 )&&( posy > 160 && posy < 200 ) ->  (fill_circle 450 180 20)
|(a,b) when ( posx > 530 && posx < 570 )&&( posy > 160 && posy < 200 ) ->  (fill_circle 550 180 20)
|(a,b) when ( posx > 630 && posx < 670 )&&( posy > 160 && posy < 200 ) ->  (fill_circle 650 180 20)
|_ -> rempli_un_cercle() ;;


let distance a b x y r = ( ( (x-a)*(x-a) + (y-b)*(y-b) ) <= r*r );; (* renvoi true si un point est sur le cercle ou à l'intérieur *)

let rec rempli_un_cercle_L2() = (* on monte y de l'espace global entre les centres car on passe à la ligne au dessus *)
let (posx,posy) = (clicA()) in
match (posx,posy) with
|(a,b) when ( distance (global_x_1)                  (global_y_1 + global_esp_y) posx posy global_r ) -> (fill_circle global_x_1                    (global_y_1 + global_esp_y) global_r)
|(a,b) when ( distance (global_x_1 + 1*global_esp_x) (global_y_1 + global_esp_y) posx posy global_r ) -> (fill_circle (global_x_1 + 1*global_esp_x) (global_y_1 + global_esp_y) global_r)
|(a,b) when ( distance (global_x_1 + 2*global_esp_x) (global_y_1 + global_esp_y) posx posy global_r ) -> (fill_circle (global_x_1 + 2*global_esp_x) (global_y_1 + global_esp_y) global_r)
|(a,b) when ( distance (global_x_1 + 3*global_esp_x) (global_y_1 + global_esp_y) posx posy global_r ) -> (fill_circle (global_x_1 + 3*global_esp_x) (global_y_1 + global_esp_y) global_r)
|_ -> rempli_un_cercle_L2() ;;


let rec rempli_un_cercle_L3() =
let (posx,posy) = (clicA()) in
match (posx,posy) with
|(a,b) when ( distance (global_x_1)                  (global_y_1 + 2*global_esp_y) posx posy global_r ) -> (fill_circle global_x_1                    (global_y_1 + 2*global_esp_y) global_r)
|(a,b) when ( distance (global_x_1 + 1*global_esp_x) (global_y_1 + 2*global_esp_y) posx posy global_r ) -> (fill_circle (global_x_1 + 1*global_esp_x) (global_y_1 + 2*global_esp_y) global_r)
|(a,b) when ( distance (global_x_1 + 2*global_esp_x) (global_y_1 + 2*global_esp_y) posx posy global_r ) -> (fill_circle (global_x_1 + 2*global_esp_x) (global_y_1 + 2*global_esp_y) global_r)
|(a,b) when ( distance (global_x_1 + 3*global_esp_x) (global_y_1 + 2*global_esp_y) posx posy global_r ) -> (fill_circle (global_x_1 + 3*global_esp_x) (global_y_1 + 2*global_esp_y) global_r)
|_ -> rempli_un_cercle_L3() ;;


(*
let rec clique_ligne l = if l>9 then print_string " erreur le plateau de jeu n'est pas plus grand " else
let pos = wait_next_event [Button_down] in (* ligne 0 à 9 car si l=0 on a juste global_y_1 qui corrspond à la 1ère ligne *)
let posx = fst(mouse_pos()) and posy = snd(mouse_pos()) in
match (posx,posy) with
|(a,b) when ( distance (global_x_1)                  (global_y_1 + l*global_esp_y) posx posy global_r ) -> color_ligne() ; (fill_circle global_x_1                    (global_y_1 + l*global_esp_y) global_r)
|(a,b) when ( distance (global_x_1 + 1*global_esp_x) (global_y_1 + l*global_esp_y) posx posy global_r ) -> color_ligne()  ; (fill_circle (global_x_1 + 1*global_esp_x) (global_y_1 + l*global_esp_y) global_r)
|(a,b) when ( distance (global_x_1 + 2*global_esp_x) (global_y_1 + l*global_esp_y) posx posy global_r ) -> color_ligne() ; (fill_circle (global_x_1 + 2*global_esp_x) (global_y_1 + l*global_esp_y) global_r)
|(a,b) when ( distance (global_x_1 + 3*global_esp_x) (global_y_1 + l*global_esp_y) posx posy global_r ) -> color_ligne() ; (fill_circle (global_x_1 + 3*global_esp_x) (global_y_1 + l*global_esp_y) global_r)
|_ -> clique_ligne l ;;

clique_ligne 0 ;;
clique_ligne 1 ;;
clique_ligne 2 ;;
clique_ligne 3 ;;
clique_ligne 4 ;;
clique_ligne 5 ;;
clique_ligne 6 ;;
clique_ligne 7 ;;
clique_ligne 8 ;;
clique_ligne 9 ;; *)

let rec color_ligne_0() =

 let c = read_key();
  in match c with
   |('b') -> colorer blue ;
   |('c') -> colorer cyan ;
   |('j') -> colorer yellow ;
   |('n') -> colorer black ;
   |('p') -> colorer violet ;
   |('r') -> colorer red ;
   |('v') -> colorer green ;
   |_ -> color_ligne_0();
;;

let tab_vide= [| " " ; " " ; " " ; " " |] ;;
let code    = [| " " ; " " ; " " ; " " |] ;;
let essai_0 = [| " " ; " " ; " " ; " " |] ;;
let essai_1 = [| " " ; " " ; " " ; " " |] ;;
let essai_2 = [| " " ; " " ; " " ; " " |] ;;
let essai_3 = [| " " ; " " ; " " ; " " |] ;;
let essai_4 = [| " " ; " " ; " " ; " " |] ;;
let essai_5 = [| " " ; " " ; " " ; " " |] ;;
let essai_6 = [| " " ; " " ; " " ; " " |] ;;
let essai_7 = [| " " ; " " ; " " ; " " |] ;;
let essai_8 = [| " " ; " " ; " " ; " " |] ;;
let essai_9 = [| " " ; " " ; " " ; " " |] ;;

let rec color_ligne tab p =
 let c = read_key(); and essai = [| " " ; " " ; " " ; " " |]
  in match c with
   |('b') -> colorer blue ;   tab.(p) <- "b"
   |('c') -> colorer cyan ;   tab.(p) <- "c"
   |('j') -> colorer yellow ; tab.(p) <- "j"
   |('n') -> colorer black ;  tab.(p) <- "n"
   |('p') -> colorer violet ; tab.(p) <- "p"
   |('r') -> colorer red ;    tab.(p) <- "r"
   |('v') -> colorer green ;  tab.(p) <- "v"
   |_ -> color_ligne_try p;
;;

(* let rec comparer x y tab_vide i = if i = 4 then tab_vide else (* i = 0 à 3 ; on aura on tableau de t et f selon le cas où le joueur a choisi la bonne couleur ou non *)
match (x.(i)) with
|a when a = y.(i) -> tab_vide.(i) = "t" ; comparer x y tab_vide (i+1)
|a when a <> y.(i) -> tab_vide.(i) =  "f" ; comparer x y tab_vide (i+1)
|_-> failwith ("erreur comparer")
;;
*)

let rec comparer x y tab i = if i = 4 then tab else (* i = 0 à 3 ; on aura on tableau de t et f selon le cas où le joueur a choisi la bonne couleur ou non *)
match (x.(i)) with
|a when a = y.(i) -> tab.(i) = "t" ; comparer x y tab (i+1)
|a when a <> y.(i) -> tab.(i) =  "f" ; comparer x y tab (i+1)
|_-> failwith ("erreur comparer")
;; (* renvoi vide au lieu de combi t / f *)

let rec code_secret l i =
if i>=4 then () else
if l>9 then print_string " erreur le plateau de jeu n'est pas plus grand_1" else
let (posx,posy) = (clicA()) in (* ligne 0 à 9 car si l=0 on a juste global_y_1 qui corrspond à la 1ère ligne *)
match (posx,posy,i) with
|(a,b,j) when ( distance (global_x_1)                  ((global_y_1-90) + l*global_esp_y) a b global_r ) -> color_ligne code 0 ; (fill_circle global_x_1                    (90) global_r) ; code_secret l (j+1)
|(a,b,j) when ( distance (global_x_1 + 1*global_esp_x) ((global_y_1-90) + l*global_esp_y) a b global_r ) -> color_ligne code 1 ; (fill_circle (global_x_1 + 1*global_esp_x) (90) global_r) ; code_secret l (j+1)
|(a,b,j) when ( distance (global_x_1 + 2*global_esp_x) ((global_y_1-90) + l*global_esp_y) a b global_r ) -> color_ligne code 2 ; (fill_circle (global_x_1 + 2*global_esp_x) (90) global_r) ; code_secret l (j+1)
|(a,b,j) when ( distance (global_x_1 + 3*global_esp_x) ((global_y_1-90) + l*global_esp_y) a b global_r ) -> color_ligne code 3 ; (fill_circle (global_x_1 + 3*global_esp_x) (90) global_r) ; code_secret l (j+1)
|_ -> code_secret l i;;

code_secret 0 0;;
code ;;

let rec clique_ligne_2 l i tabl =
if i>=4 then () else
 if l>9 then print_string " erreur le plateau de jeu n'est pas plus grand_2 " else
let (posx,posy) = (clicA()) in (* associe posx et posy aux 2 int de clicA *)
match (posx,posy,i) with
|(a,b,j) when ( distance (global_x_1)                  (global_y_1 + l*global_esp_y) a b global_r )=true -> color_ligne tabl 0 ; (fill_circle global_x_1                    (global_y_1 + l*global_esp_y) global_r) ; clique_ligne_2 l (j+1) tabl
|(a,b,j) when ( distance (global_x_1 + 1*global_esp_x) (global_y_1 + l*global_esp_y) a b global_r )=true -> color_ligne tabl 1 ; (fill_circle (global_x_1 + 1*global_esp_x) (global_y_1 + l*global_esp_y) global_r) ; clique_ligne_2 l (j+1) tabl
|(a,b,j) when ( distance (global_x_1 + 2*global_esp_x) (global_y_1 + l*global_esp_y) a b global_r )=true -> color_ligne tabl 2 ; (fill_circle (global_x_1 + 2*global_esp_x) (global_y_1 + l*global_esp_y) global_r) ; clique_ligne_2 l (j+1) tabl
|(a,b,j) when ( distance (global_x_1 + 3*global_esp_x) (global_y_1 + l*global_esp_y) a b global_r )=true -> color_ligne tabl 3 ; (fill_circle (global_x_1 + 3*global_esp_x) (global_y_1 + l*global_esp_y) global_r) ; clique_ligne_2 l (j+1) tabl
|_ -> clique_ligne_2 l i tabl;;                                                        (* Les cases du tableau sont remplies selon le cercle ou clique le joueur, car il ne remplira pas forcément les cases dans l'ordre *)


clique_ligne_2 0 0 essai_0;; (* les 10 essais du joueur *)
essai_0 ;;

comparer code essai_0 tab_vide 0;;

clique_ligne_2 1 0 essai_1;;
essai_1 ;;
clique_ligne_2 2 0 essai_2;;
clique_ligne_2 3 0 essai_3;;
clique_ligne_2 4 0 essai_4;;
clique_ligne_2 5 0 essai_5;;
clique_ligne_2 6 0 essai_6;;
clique_ligne_2 7 0 essai_7;;
clique_ligne_2 8 0 essai_8;;
clique_ligne_2 9 0 essai_9;;


moveto 100 400;;
draw_string " - Test menu p ";;

print_string " Que voulez vous faire ? 1/ interface de jeu \n 2 phase de test 9/ Quitter ";;
let rec menu_principal()=
 let c = read_key();
  in match c with
   (* |'l' -> match read_key(); with
      |'1' -> rempli_un_cercle_V2();
      |'2' -> rempli_un_cercle_L3();
      |'3' -> rempli_un_cercle_L3();
      |'4' -> menu_principal(); *)
   |'a' -> choose_color_v12 0 ;
   |'z' -> ();
   |'e' -> eot();
   |_ -> menu_principal(); (* !! marche pas !!*)
;;


menu_principal();;

choose_color_v0();;

colorer violet;;

fill_grille_aux();;

colorer green;;

rempli_cercle();;

rempli_un_cercle();;

colorer red;;

rempli_un_cercle();;
rempli_un_cercle();;
rempli_un_cercle();;
rempli_un_cercle();;


colorer blue;;
rempli_un_cercle_L2();;
colorer white;;
rempli_un_cercle_L2();;
colorer red;;
rempli_un_cercle_L2();;
colorer violet;;
rempli_un_cercle_L2();;

colorer blue;;
rempli_un_cercle_L3();;
colorer white;;
rempli_un_cercle_L3();;
colorer red;;
rempli_un_cercle_L3();;
colorer violet;;
rempli_un_cercle_L3();;

choose_color_v12 0;;

colorer black;;

rempli_cercle_vrec 0;;

(*
à recoder si utile
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
*)


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
rect ();;    *)

print_string "here1" ;;
read_key();; (* -> char *)

key_pressed();; (* T / F *)

current_x;; (* position actuelle *)

print_string "here2" ;;
button_down();;

mouse_pos ();; (* <=> Graphics.mouse_pos();; donne x y  de la souris *)

clic();;
option_clic();;

print_string "here3" ;;

fst( mouse_pos() );;
snd( mouse_pos() );;

clic();;

quit_game();;

clic();;

print_string "here4" ;;

clic();;

clic();;

print_string "clique" ;;

clic();;

quit_game();;