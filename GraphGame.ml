#use "GraphLib.ml";;
#use "Code.ml";;

(* OBLIGATOIRE :
Code.nombre_pions = 4
Code.couleurs_possibles = 6 ou moins
*)
let secretCode = ref (Code.makeCode "abcd");;

let makeCodeMode = 0;;
let makeAnswerMode = 1;;
let gameMode = ref makeCodeMode;; (* 0 : on peut crée notre code
                          1 : on peut faire notre reponse *)
let answerIA = ref true;;
let nombre_pions = 4;; (*pour prevenir le cas Code.nombre_pions <> 4*)

let round f = floor (f +. 0.5);;
let roundInt f = int_of_float (floor (f +. 0.5));;
let floatc x = float_of_int x;;
let intc x = int_of_float x;;

let pxIN = 4;;
Graph.setup (200*pxIN) (150*pxIN) pxIN;;

let tokenCodeStartX = 57;;
let tokenCodeStepX = 12;;
let tokenCodeStartY = 133;;
let tokenCodeStepY = 14;;

type image = {image : ocImage; x : float ref; y : float ref};;
type imageObj = {imageo : ocImage; xo : float ref; yo : float ref; objx : float ref; objy : float ref; k : float};; (* Obj pour objectif *)
type imageCode = {color : Code.t; listAnswer : imageObj list};;

let loadImagePx path _x _y = {image = Graph.loadImagePx path; x = ref (_x*.(floatc(!Graph.pxIN))); y = ref (_y*.(floatc(!Graph.pxIN)))};;
let loadImageObjPx path _x _y _objx _objy _k = {imageo = Graph.loadImagePx path; xo = ref (_x*.(floatc(!Graph.pxIN))); yo = ref (_y*.(floatc(!Graph.pxIN))); objx = ref (_objx*.(floatc(!Graph.pxIN))); objy = ref (_objy*.(floatc(!Graph.pxIN))); k = _k};;

let image im = Graph.image (im.image) (roundInt !(im.x)) (roundInt !(im.y));;
let imageObj im =
im.xo := !(im.xo)+. im.k*.(!(im.objx)-. !(im.xo));
im.yo := !(im.yo)+. im.k*.(!(im.objy)-. !(im.yo));
(Graph.image (im.imageo) (roundInt !(im.xo)) (roundInt !(im.yo)));;

let back = loadImagePx ("./data/Back.im") 0. 0.;;
let board = loadImagePx ("./data/Board.im") 46. 0.;;

let answerWellplaced = Graph.loadImagePx "./data/TokenWellPlaced.im";;
let answerMisplaced = Graph.loadImagePx "./data/TokenMisplaced.im";;

let codeArrayList = ref [];;
let codeArray = Array.make nombre_pions None;;
(*let codeAnswer = ;;*)
(*codeArrayList := {color = (Code.makeCode "abcd"); listAnswer = [{imageo = answerWellplaced; xo = ref (floatc(50* !Graph.pxIN));  yo = ref (floatc(134* !Graph.pxIN)); objx = ref (floatc(5* !Graph.pxIN)); objy = ref (floatc(134* !Graph.pxIN)); k = 0.1}]}:: !codeArrayList;;
*)
let codeSelectedX = 54.;;
let codeSelectedY = 131.;;
let codeSelectedStep = 14.;;

let codeSelectedImage =  loadImageObjPx ("./data/CodeSelector.im") codeSelectedX codeSelectedY codeSelectedX codeSelectedY 0.1;;

let tokenList =
  let rec makeList l i =
    if i = -1 then l
    else makeList ((Graph.loadImagePx ("./data/"^(string_of_int i)^".im"))::l) (i-1)
  in makeList [] ((List.length Code.couleurs_possibles)-1)
;;
let tokenStartX = 113;;
let tokenStartY = 131;;
let tokenStep = 12;;

let codeSelected = ref 0;;
let tokenSelected = ref (-1);;

let drawTokenList () =
  List.fold_left (fun acc c -> let i = acc in
                               if i <> !tokenSelected then (Graph.image c (tokenStartX* !Graph.pxIN) ((tokenStartY-i*tokenStep)* !Graph.pxIN));
                               i+1
                          ) 0 tokenList;
  if !tokenSelected <> -1 then
  (let c = (List.nth tokenList !tokenSelected) in
  Graph.image c (!Graph.mouseX-roundInt((float_of_int (c.width))/.2.)) (!Graph.mouseY-roundInt((float_of_int (c.height))/.2.)))
;;

let tokenSelectedHitBox i c =
  (*print_endline (string_of_int i);*)
  let x = tokenStartX* !Graph.pxIN in
  let y = ((tokenStartY-i*tokenStep))* !Graph.pxIN in
  let mx = !Graph.mouseX in let my = !Graph.mouseY in (*Graph.rect (x+1* !Graph.pxIN) (y+(1* !Graph.pxIN)) (7* !Graph.pxIN) (7* !Graph.pxIN);*)
  if (mx >= x+1* !Graph.pxIN && mx <= x+8* !Graph.pxIN && my >= y+(1* !Graph.pxIN) && my <= y+(8* !Graph.pxIN))
  || (mx >= x+3* !Graph.pxIN && mx <= x+6* !Graph.pxIN && my >= y && my <= y+(9* !Graph.pxIN))
  || (mx >= x && mx <= x+(9* !Graph.pxIN) && my >= y+(3* !Graph.pxIN) && my <= y+(6* !Graph.pxIN))
  then tokenSelected := i;
;;

let tokenListSelectedHitBox () =
  tokenSelected := -1;
  List.fold_left (fun acc c -> tokenSelectedHitBox acc c; acc+1
                  ) 0 tokenList;
;;

let updateGameMode x =
  gameMode := x;
  if !gameMode = makeCodeMode then
    (

      );
  if !gameMode = makeAnswerMode then
    (
      if !answerIA then
        (

          )
      else ()
      );
;;

let updateCodeSelectedImage () =
  codeSelectedImage.objy := (codeSelectedY -. floatc (!codeSelected) *. codeSelectedStep) *. floatc (!Graph.pxIN);
;;

let updateCodeSelected () =
  codeSelected := !codeSelected+1;
  updateCodeSelectedImage()
;;

let drawCodeArrayList () =
  List.fold_left (fun accy c -> List.fold_left (fun accx c_ -> let color = match c_ with
                                                                          | Code.Color(x) -> x
                                                                          | _ -> -1 in
                                                              Graph.image (List.nth tokenList color) (((tokenCodeStartX+accx*tokenCodeStepX+1)* !Graph.pxIN)) (((tokenCodeStartY-accy*tokenCodeStepY+1)* !Graph.pxIN));
                                                              accx+1
                                               ) 0 c.color;
                                List.fold_left (fun acc_ c_ -> imageObj (c_); acc_
                                                ) 0 c.listAnswer;
                                  accy+1
                  ) 0 (!codeArrayList)
;;

let drawCodeArray () =
  Array.fold_left (fun acc c -> (match c with
                                 | Some(Code.Color(x)) -> Graph.image (List.nth tokenList x)
                                                          (((tokenCodeStartX+acc*tokenCodeStepX+1)* !Graph.pxIN))
                                                          (((tokenCodeStartY-(! codeSelected)*tokenCodeStepY+1)* !Graph.pxIN))
                                 | None -> ());
                                 acc+1
                  ) 0 codeArray
;;

let tokenCodeHitBox x y =
  let x = (tokenCodeStartX+x*tokenCodeStepX)* !Graph.pxIN in
  let y = (tokenCodeStartY-(!codeSelected)*tokenCodeStepY)* !Graph.pxIN in
  let mx = !Graph.mouseX in
  let my = !Graph.mouseY in (*Graph.line (x+(1)* !Graph.pxIN) (y+(1)* !Graph.pxIN) (x+(tokenCodeStepX-2)* !Graph.pxIN) (y+(tokenCodeStepX-2)* !Graph.pxIN);
  Graph.line (x+(3)* !Graph.pxIN ) (y) (x+(8)* !Graph.pxIN) (y+(11)* !Graph.pxIN);
  Graph.line (x) (y+(3)* !Graph.pxIN) (x+11* !Graph.pxIN) (y+(8)* !Graph.pxIN);*)
  (mx >= x+(1)* !Graph.pxIN && mx <= x+(tokenCodeStepX-2)* !Graph.pxIN && my >= y+(1)* !Graph.pxIN && my <= y+(tokenCodeStepX-2)* !Graph.pxIN) ||
  (mx >= x+(3)* !Graph.pxIN && mx <= x+(8)* !Graph.pxIN && my >= y && my <= y+(11)* !Graph.pxIN) ||
  (mx >= x && mx <= x+11* !Graph.pxIN && my >= y+(3)* !Graph.pxIN && my <= y+(8)* !Graph.pxIN)
;;

let codeSelection () =
  if !gameMode = makeCodeMode && !tokenSelected <> -1  then
    (let newTokenSelected = fst(
    Array.fold_left (fun acc c -> let i = snd acc in
                                  match tokenCodeHitBox i !codeSelected with
                                    | true -> Array.set codeArray i (Some(Code.Color( !tokenSelected))); (-1, i+1)
                                    | false -> (fst acc, i+1)
                      ) (!tokenSelected,0) codeArray) in
    if newTokenSelected = -1 then
      (tokenSelected := newTokenSelected;
        if Array.fold_left (fun acc c -> acc && c <> None) true codeArray then
        updateGameMode makeAnswerMode;
        );
    );
;;

let screenGame () =
  let choiceScr = ref 0 in
  Graph.background (Graph.color 0 0 0);
  image back;
  image board;
  drawCodeArrayList ();
  drawCodeArray ();
  imageObj codeSelectedImage;
  drawTokenList ();
  Graph.fill(Graph.color 255 255 255);
  Graph.text (string_of_int !Graph.frameCount) 0 0;
  Graph.text (string_of_int !codeSelected) 0 40;
  Graph.text (string_of_int(!Graph.mouseX) ^ " " ^ string_of_int(!Graph.mouseY)) 0 80;
  Graph.text (string_of_int(fst (Graphics.mouse_pos ())) ^ " " ^ string_of_int(snd (Graphics.mouse_pos ()))) 0 120;
  (*Graph.rect (!Graph.mouseX -20) (!Graph.mouseY-20) (20) (20);*)
  if Graphics.key_pressed () then
    (match Graphics.read_key () with
    | 'a' -> updateCodeSelected()
    | _ -> ()
    )
  ;
  if !Graph.mousePressed then
    (
    codeSelection();
    tokenListSelectedHitBox ();
    if !Graph.mouseX >= (!Graph.width-20) && !Graph.mouseX <= !Graph.width && !Graph.mouseY >= (0) && !Graph.mouseY <= 20 then (choiceScr := -1);
    )
  ;
  !choiceScr
;;

let screenList = [screenGame];;

Graph.draw screenList 0;;

(*
Screen Format :

(fun () ->
      Graph.background (Graph.color 255 255 255);
      if Graphics.key_pressed () then
        (match Graphics.read_key () with
        | _ -> ()
        )
      ;
      if Graphics.button_down () then
        ()
      ;
)

*)
(*
let screen1 () =
  let choiceScr = ref 1 in
  Graph.background (Graph.color 0 0 0);
  if Graphics.key_pressed () then
    (match Graphics.read_key () with
    | _ -> ()
    )
  ;
  Graph.fill(Graph.color 255 255 255);
  Graph.rect (Graph.width-20) (Graph.height-20) (20) (20);
  if !Graph.mousePressed then
    if !Graph.mouseX >= (Graph.width-20) && !Graph.mouseX <= Graph.width && !Graph.mouseY >= (Graph.height-20) && !Graph.mouseY <= Graph.height then choiceScr := -1;
  !choiceScr
;;

let screen0 () =
  let choiceScr = ref 0 in
  Graph.background (Graph.color 255 255 255);
  Graph.fill (Graph.color 0 0 0);
  Graph.text (string_of_int !Graph.frameCount) 20 (Graph.height-40);
  Graph.image imN 0 90;
  Graph.image im (int_of_float(((float_of_int(int_of_float(Graph.millis()) mod 6000)/.6000.)*.float_of_int(Graph.width-im.width)))) 0;
  if Graphics.key_pressed () then
    (match Graphics.read_key () with
    | x -> key := x;
    | _ -> ()
    )
  ;
  Graph.rect (Graph.width-20) (Graph.height-20) (20) (20);
  Graph.text (String.make 1 !key) 20 (Graph.height-70);
  if !Graph.mousePressed then
    if !Graph.mouseX >= (Graph.width-20) && !Graph.mouseX <= Graph.width && !Graph.mouseY >= (Graph.height-20) && !Graph.mouseY <= Graph.height then choiceScr := 1;
  !choiceScr
;;*)