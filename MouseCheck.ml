#load "graphics.cma";;

#load "unix.cma";;

let width = ref 200;;
let height = ref 200;;

let startMillisCount = Unix.gettimeofday();;
let millis () = (Unix.gettimeofday()-.startMillisCount)*.1000.;;

let rec wait start =
  (*print_endline ((string_of_float (Unix.gettimeofday())) ^ " " ^ string_of_float(start));*)
  if millis()-.start < waitValue then wait start
;;


let size x y =
      Graphics.open_graph (" " ^ (string_of_int x)^"x"^(string_of_int y));
      Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
	  Graphics.auto_synchronize false;
;;

let color r g b = Graphics.rgb r g b;;

let fill co = Graphics.set_color (co);;


let background co =
  fill co;
  Graphics.fill_rect 0 0 !width !height
;;

let line x1 y1 x2 y2 =
  Graphics.moveto (x1) (y1);
  Graphics.lineto (x2) (y2);
;;


let exit () = Graphics.close_graph ();;