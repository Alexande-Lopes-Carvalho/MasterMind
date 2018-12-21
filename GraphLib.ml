#load "graphics.cma";;
#load "unix.cma";;
type ocImage = {image : Graphics.image; width : int; height : int};;

module Graph :
  sig
    val pxIN : int ref
    val width : int ref
    val height : int ref
    val frameRate : float
    val millis : unit -> float
    val translateX : int ref
    val translateY : int ref

    val wait : float -> unit

    val setup : int -> int -> int -> unit

    val nameWindow : string

    val frameCount : int ref
    val mouseX : int ref
    val mouseY : int ref
    val startFrame : float ref
    val mousePos : bool ref
    val mousePressed : bool ref
    val mouseReleased : bool ref

    val beginDraw : unit -> unit
    val endDraw : unit -> unit
    val draw : (unit -> int) list -> int -> unit

    val mouseProc : unit -> unit

    val size : int -> int -> unit
    val exit : unit -> unit

    val translate : int -> int -> unit

    val color : int -> int -> int -> Graphics.color

    val fill : Graphics.color -> unit

    val rect : int -> int -> int -> int -> unit

    val background : Graphics.color -> unit

    val ellipse : int -> int -> int -> int -> unit

    val set : int -> int -> unit

    val line : int -> int -> int -> int -> unit

    val strokeWeight : int -> unit

    val text : string -> int -> int -> unit

    val wait : float -> unit

    val getImage : string -> Graphics.color array array

    val loadImage : string -> ocImage

    val loadImagePx : string -> ocImage

    val image : ocImage -> int -> int -> unit

  end =
  struct

    let pxIN = ref 1;;
    let width = ref 1(*pxIN*200*);;
    let height = ref 1(*pxIN*150*);;
    let nameWindow = "MasterMind";;
    let frameRate = 60.;; (* fps *)
    let translateX = ref 0;;
    let translateY = ref 0;;

    let startMillisCount = Unix.gettimeofday();;
    let millis () = (Unix.gettimeofday()-.startMillisCount)*.1000.;;
    let waitValue = 1000./.frameRate;;

    let frameCount = ref 0;;
    let mouseX = ref 0;;
    let mouseY = ref 0;;
    let startFrame = ref 0.;;
    let mousePos = ref false;;
    let mousePressed = ref false;;
    let mouseReleased = ref false;;

    let rec wait start =
      (*print_endline ((string_of_float (Unix.gettimeofday())) ^ " " ^ string_of_float(start));*)
      if millis()-.start < waitValue then wait start
    ;;

    let mouseProc () =
      let bool = ref false in
      if Graphics.button_down () <> !mousePos then
        bool := true;
      if !bool then mousePos := not (!mousePos);
      mousePressed := (!bool && !mousePos);
      mouseReleased := (!bool && not(!mousePos))
    ;;

    let beginDraw () =
      let mouseCoord = Graphics.mouse_pos () in
      mouseX := fst (mouseCoord);
      mouseY := !height-1 - snd (mouseCoord);
      frameCount := !frameCount+1;
      mouseProc ();
      startFrame := millis()
    ;;

    let endDraw () =
      Graphics.synchronize ();
      wait (!startFrame);
    ;;

    let exit () = Graphics.close_graph ();;

    let rec draw proc choosed =
      beginDraw();
      let i = (List.nth proc choosed) () in
      endDraw();
      if i >= 0 then draw proc i
      else exit()
    ;;

    let size x y =
      Graphics.open_graph (" " ^ (string_of_int x)^"x"^(string_of_int y));
      Graphics.set_window_title nameWindow;
      Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
    ;;

    let setup w h p =
      width := w;
      height := h;
      pxIN := p;
      size !width !height;
      Graphics.auto_synchronize false;
    ;;

    let translate x y =
      translateX := x;
      translateY := y;
    ;;

    let color r g b = Graphics.rgb r g b;;

    let fill co = Graphics.set_color (co);;

    let rect x y w h =
      Graphics.fill_rect (x+ !translateX) (!height-1 - y - h - !translateY) w h
    ;;

    let background co =
      fill co;
      Graphics.fill_rect 0 0 !width !height
    ;;

    let ellipse x y w h =
      Graphics.fill_ellipse (x+w/2+ !translateX) (!height-1-y-h/2- !translateY) (w/2) (h/2)
    ;;

    let set x y =
      Graphics.plot (x+ !translateX) (!height-1-y- !translateY)
    ;;

    let line x1 y1 x2 y2 =
      Graphics.moveto (x1+ !translateX) (!height-1-y1- !translateY);
      Graphics.lineto (x2+ !translateX) (!height-1-y2- !translateY);
    ;;

    let strokeWeight x = Graphics.set_line_width x;;

    let text txt x y =
      Graphics.moveto (x+ !translateX) ((!height)-1 -y-(snd(Graphics.text_size ""))- !translateY);
      Graphics.draw_string txt;
    ;;

    let input_line_option ch =
      try Some(input_line ch) with
      | _ -> None
    ;;

    let rec lireImagelignes_rec ch l =
      let k = input_line_option ch in
      match k with
      | Some(s) -> lireImagelignes_rec ch (l ^ s ^ "\n")
      | None -> close_in ch; l
    ;;

    let lireImagelignes data =
      lireImagelignes_rec data ""
    ;;

    let rec charlist_of_string_rec s i l =
      if i >= 0 then charlist_of_string_rec s (i-1) (String.get s i::l)
      else l
    ;;

    let charlist_of_string s =
      charlist_of_string_rec s ((String.length s)-1) []
    ;;

    let getImageWidthHeight x =
      let out = fst (List.fold_left (fun acc c -> let data = fst acc in
                                            let mode = snd acc in
                                            if mode then
                                              let out = (String.make 1 c) in (((fst data), (snd data) ^ out), mode)
                                            else
                                              let out = (String.make 1 c) in
                                              if String.compare out "|" = 0 then (data, true)
                                              else (((fst data)^out, snd data), mode)
                                    ) (("",""), false) (charlist_of_string x)) in (int_of_string (fst out), int_of_string (snd out))
    ;;

    let stringArrayInt a =
      (Array.fold_left (fun acc c -> acc ^ (string_of_int c)^" ") "[|" a) ^ "|]"
    ;;

    let stringArrayArrayInt a =
      (Array.fold_left (fun acc c -> acc ^ (stringArrayInt c)^ "\n ") "[|" a) ^ "|]"
    ;;

    let rec makeImage s i a =
      (*print_endline (string_of_int i);*)
      if i = Array.length (a)*Array.length (Array.get a 0) then a
      else
        match s with
        | alpha::r::g::b::l -> (*print_endline ((string_of_int i) ^ " " ^ string_of_int(int_of_char r) ^ " " ^ string_of_int(int_of_char g) ^ " " ^ string_of_int(int_of_char b) ^ " " ^ string_of_int(int_of_char alpha))(* ^ " \n" ^ (stringArrayArrayInt a))*);*)
                        Array.set (Array.get a (i/(Array.length (Array.get a 0)))) (i mod (Array.length (Array.get a 0))) (if (int_of_char alpha <> 0) then (Graphics.rgb (int_of_char r) (int_of_char g) (int_of_char b))
                                                                                                                           else Graphics.transp);
                        makeImage l (i+1) a
        | _ -> (*print_endline (stringArrayArrayInt a );*) a
    ;;

    let getImage path =
      let data = open_in (path) in
      let dimension = getImageWidthHeight (input_line data) in print_endline ("" ^ string_of_int(fst dimension) ^ " " ^ string_of_int(snd dimension));
      let pixels = Array.init (snd dimension) (fun x -> Array.make (fst dimension) 0) in
        (makeImage (charlist_of_string (lireImagelignes data)) 0 pixels)
    ;;

    let loadImage path =
      let im = getImage path in
      {image = Graphics.make_image (im); width = Array.length (Array.get im 0); height = Array.length im}
    ;;

    let rec setAr ar i c nb =
      (Array.set ar i c);
      if nb = 1 then ar else setAr ar (i+1) c (nb-1)
    ;;

    let resize im pxIN =
      fst (Array.fold_left (fun acc c -> let i = snd acc in
                                         let xligne = fst( Array.fold_left (fun acc_ c_ -> let i = snd acc_ in
                                                                                            (setAr (fst acc_) (i*pxIN) c_ pxIN, i+1)
                                                                             ) ((Array.make ((Array.length (Array.get im 0))*pxIN) 0), 0) c) in
                                         (setAr (fst acc) (i*pxIN) xligne pxIN, i+1)
                             ) ((Array.init ((Array.length im)*pxIN) (fun x -> Array.make ((Array.length (Array.get im 0))*pxIN) 0)), 0) im)
    ;;

    let loadImagePx path =
      let im = resize (getImage path) !pxIN in
      {image = Graphics.make_image im; width = Array.length (Array.get im 0); height = Array.length im}
    ;;

    let image im x y =
      Graphics.draw_image im.image (x+ !translateX) (!height-y-im.height- !translateY)
    ;;
  end;;