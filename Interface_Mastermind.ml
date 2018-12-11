
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
 Graphics.open_graph "";; <=> taille prédef
 Graphics.close_graph();; *)


Graphics.set_window_title " Test fenetre 1";;

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
set_color black;
Graphics.draw_string "Options";;


set_color gris;
Graphics.draw_rect 8 8 980 980;;

set_line_width 2;;
set_color brown;
Graphics.draw_rect 40 45 920 940;;



Graphics.moveto 400 400;;
set_color black;;

Graphics.draw_string "Test ecriture";;



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




print_string "here1" ;;
read_key();; (* -> char *)

key_pressed();; (* T / F *)

current_x;;


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
