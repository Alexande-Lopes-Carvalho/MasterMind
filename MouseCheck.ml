#load "graphics.cma";;
let width = ref 200;;
let height = ref 200;;

let size x y =
      Graphics.open_graph (" " ^ (string_of_int x)^"x"^(string_of_int y));
      Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
	  Graphics.auto_synchronize false;
;;

let exit () = Graphics.close_graph ();;