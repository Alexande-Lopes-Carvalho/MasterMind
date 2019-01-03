#use "GraphLib.ml";;
#use "Code.ml";;
#use "IA.ml";;

let initStartBool = ref true;;
(* OBLIGATOIRE :
Code.nombre_pions = 4
Code.couleurs_possibles = 6 ou moins
*)

(*
  typePlayerBreaker = -1 : Human
  typePlayerBreaker = 0 : IA naif
  typePlayerBreaker = 1 : IA Knuth

  typePlayerMaker = -1  : Human
  typePlayerMaker = 0  : IA assist
*)

let maxvalue a b = if a>b then a else b;;
let minvalue a b = if a>b then b else a;;

let gameLeft = ref 4;;

type player = {name : string ref; typePlayerBreaker : int ref; typePlayerMaker : int ref; score : int ref};;

let player = Array.make 2 {name = ref ""; typePlayerBreaker = ref 0; typePlayerMaker = ref 0; score = ref 0};;
Array.set player 0 ({name = ref "Player1"; typePlayerBreaker = ref (-1); typePlayerMaker = ref (-1); score = (Array.get player 0).score});;
Array.set player 1 ({name = ref "Player2"; typePlayerBreaker = ref (0); typePlayerMaker = ref (0); score = (Array.get player 1).score});;

let getTypePlayerBreaker i =
  !((Array.get player i).typePlayerBreaker)
;;

let getTypePlayerMaker i =
  !((Array.get player i).typePlayerMaker)
;;

let secretCode = ref [](*(Code.makeCode "feba")*);;
let codeBreaker = ref 0;;
let codeMaker = ref ((!codeBreaker+1) mod 2);;

let codeBreakerWon = ref false;;
let makeSecretCodeMode = 0;;
let makeCodeMode = 1;;
let makeAnswerMode = 2;;
let makeEndMode = 3;;
let gameMode = ref (-1);; (* 0 : on cree le code secret
                                     1 : on peut proposer un code
                                     2 : on peut faire notre reponse
                                     3 : fin du round*)
let listEssaiPropose = ref [];;
let listEssaiPossible = ref Code.tous;;

let badAnswer = ref false;;
let nombre_pions = 4;; (*pour prevenir le cas Code.nombre_pions <> 4*)

let round f = floor (f +. 0.5);;
let roundInt f = int_of_float (floor (f +. 0.5));;
let floatc x = float_of_int x;;
let intc x = int_of_float x;;

Random.init (roundInt(Unix.gettimeofday()));;
print_endline ((string_of_int(roundInt(Unix.gettimeofday()))) ^ " INIT RANDOM");;

let pxIN = 4;;
let txtPxIN = 4;;
Graph.setup (200*pxIN) (150*pxIN) pxIN;;

let tokenCodeStartX = 57;;
let tokenCodeStepX = 12;;
let tokenCodeStartY = 133;;
let tokenCodeStepY = 14;;

type image = {image : ocImage; x : float ref; y : float ref};;
type imageObj = {imageo : ocImage; xo : float ref; yo : float ref; objx : float ref; objy : float ref; k : float};; (* Obj pour objectif *)
type imageTxtMessageObj = {txt : string list ref; xt : float; yt : float; img : imageObj};;
type imageAnswer = {id : int; imAns : imageObj};;
type imageCode = {color : Code.t; listAnswer : imageAnswer list};;
let answerWellplacedID = 0;;
let answerMisplacedID = 1;;

let loadImagePx path _x _y = {image = Graph.loadImagePx path; x = ref (_x*.(floatc(!Graph.pxIN))); y = ref (_y*.(floatc(!Graph.pxIN)))};;
let loadImageObjPx path _x _y _objx _objy _k = {imageo = Graph.loadImagePx path; xo = ref (_x*.(floatc(!Graph.pxIN))); yo = ref (_y*.(floatc(!Graph.pxIN))); objx = ref (_objx*.(floatc(!Graph.pxIN))); objy = ref (_objy*.(floatc(!Graph.pxIN))); k = _k};;

let image im = Graph.image (im.image) (roundInt !(im.x)) (roundInt !(im.y));;
let imageObj im =
im.xo := !(im.xo)+. im.k*.(!(im.objx)-. !(im.xo));
im.yo := !(im.yo)+. im.k*.(!(im.objy)-. !(im.yo));
(Graph.image (im.imageo) (roundInt !(im.xo)) (roundInt !(im.yo)));;
let imageTxtMessageObj im =
imageObj im.img;
Graph.textList (!(im.txt)) (roundInt (!(im.img.xo)+.im.xt)) (roundInt (!(im.img.yo)+.im.yt));;

let messageEndImage = {txt = ref (""::[]); xt = floatc (3*txtPxIN); yt = floatc (2*txtPxIN); img = {imageo = Graph.loadImagePxSet "./data/Message.im" txtPxIN; xo = ref (floatc(!Graph.width)); yo = ref (floatc(40* !Graph.pxIN)); objx = ref (floatc(!Graph.width)); objy = ref (floatc(40* !Graph.pxIN)); k = 0.1}};; (* image ayant du texte on tjrs une taille fixe *)

let back = loadImagePx ("./data/Back.im") 0. 0.;;
let secretCodeBoard = loadImageObjPx ("./data/TokenStart.im") (floatc (!Graph.width)) 0. (floatc (!Graph.width)) 0. 0.1;;
let board = loadImagePx ("./data/Board.im") 46. 0.;;

let secretCodeMake = Array.make nombre_pions None;;

let answerTokenY = 134;;
let answerTokenStepY = 14;;
let answerTokenOutputX = 50;;
let answerTokenOutputEndX = 5;;
let answerToken = Graph.loadImagePx "./data/TokenAnswer.im";;
let answerTokenObj = {imageo = answerToken; xo = ref (floatc(!Graph.width)); yo = ref 0. ; objx = ref (floatc(!Graph.width)); objy = ref 0.; k = 0.1};;
let answerWellplaced = Graph.loadImagePx "./data/TokenWellPlaced.im";;
let answerMisplaced = Graph.loadImagePx "./data/TokenMisplaced.im";;

let codeArrayList = ref [];;
let codeArray = Array.make nombre_pions None;;
let codeAnswer = Array.make nombre_pions None;;
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

let codeSelected = ref (-1);;
let codeSelectedMax = 9;;
let tokenSelected = ref (-1);;

let blankCodeArray a =
  (Array.fold_left (fun acc c ->let i = acc in Array.set a i None; i+1) 0 a)
;;

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
  if !gameMode = makeCodeMode || !gameMode = makeSecretCodeMode then (
    tokenSelected := -1;
    List.fold_left (fun acc c -> tokenSelectedHitBox acc c; acc+1
                    ) 0 tokenList;
                  ())
;;

let updateCodeSelectedImage () =
  codeSelectedImage.objy := (codeSelectedY -. floatc (!codeSelected) *. codeSelectedStep) *. floatc (!Graph.pxIN);
;;

let updateCodeSelected () =
  codeSelected := !codeSelected+1;
  updateCodeSelectedImage()
;;

let initMakeCodeMode () =
  if !codeSelected <> 0 then
    (answerTokenObj.objx := floatc(!Graph.width)
    )
;;

let initMakeSecretCodeMode () =
  secretCodeBoard.xo := floatc (53* !Graph.pxIN);
  secretCodeBoard.yo := 0.;
  secretCodeBoard.objx := floatc (53* !Graph.pxIN);
  secretCodeBoard.objy := 0.;
;;

let drawCodeArray ar xco yco =
  Array.fold_left (fun acc c -> (match c with
                                 | Some(Code.Color(x)) -> Graph.image (List.nth tokenList x)
                                                          (xco+((acc*tokenCodeStepX)* !Graph.pxIN))
                                                          (yco)
                                 | None -> ());
                                 acc+1
                  ) 0 ar
;;

let tokenCodeHitBox x y =
  let mx = !Graph.mouseX in
  let my = !Graph.mouseY in (*Graph.line (x+(1)* !Graph.pxIN) (y+(1)* !Graph.pxIN) (x+(tokenCodeStepX-2)* !Graph.pxIN) (y+(tokenCodeStepX-2)* !Graph.pxIN);
  Graph.line (x+(3)* !Graph.pxIN ) (y) (x+(8)* !Graph.pxIN) (y+(11)* !Graph.pxIN);
  Graph.line (x) (y+(3)* !Graph.pxIN) (x+11* !Graph.pxIN) (y+(8)* !Graph.pxIN);*)
  (mx >= x+(1)* !Graph.pxIN && mx <= x+(tokenCodeStepX-2)* !Graph.pxIN && my >= y+(1)* !Graph.pxIN && my <= y+(tokenCodeStepX-2)* !Graph.pxIN) ||
  (mx >= x+(3)* !Graph.pxIN && mx <= x+(8)* !Graph.pxIN && my >= y && my <= y+(11)* !Graph.pxIN) ||
  (mx >= x && mx <= x+11* !Graph.pxIN && my >= y+(3)* !Graph.pxIN && my <= y+(8)* !Graph.pxIN)
;;


let drawSecretCodeBoard () =
  imageObj secretCodeBoard;
  drawCodeArray secretCodeMake (( (roundInt (!(secretCodeBoard.xo)) ) +(5)* !Graph.pxIN)) ((roundInt (!(secretCodeBoard.yo)) )+ 73* !Graph.pxIN)(*( ( ( (roundInt (!(secretCodeBoard.xo)) ) +58+1)* !Graph.pxIN) ) ((((roundInt(!(secretCodeBoard.yo)))+73* !Graph.pxIN)))*);
;;

let initMakeAnswerMode () =
 answerTokenObj.xo := floatc(!Graph.width);
 answerTokenObj.yo := floatc ((answerTokenY-answerTokenStepY* !codeSelected)* !Graph.pxIN);
 answerTokenObj.objx := floatc (!Graph.width-(66)* ! Graph.pxIN);
 answerTokenObj.objy := floatc ((answerTokenY-answerTokenStepY* !codeSelected)* !Graph.pxIN);
;;

let initMakeEndMode () =
  secretCodeBoard.xo := floatc (53* !Graph.pxIN);
  secretCodeBoard.objx := floatc (53* !Graph.pxIN);
  messageEndImage.img.objx := floatc( !Graph.width - messageEndImage.img.imageo.width);
  secretCodeBoard.objy := 0.;
  answerTokenObj.objx := floatc(!Graph.width);
  let wonID = if !codeBreakerWon || !badAnswer then !codeBreaker else !codeMaker in
  let oldscore = !((Array.get player (wonID)).score) in (*print_endline (string_of_int oldscore);*)
  Array.set player (wonID) ({name = (Array.get player (wonID)).name; typePlayerBreaker = (Array.get player (wonID)).typePlayerBreaker; typePlayerMaker = (Array.get player (wonID)).typePlayerMaker; score = ref (1+ oldscore)});
  let score0 = (!((Array.get player (0)).score)) in let score1 = (!((Array.get player (1)).score)) in
  messageEndImage.txt := (!((Array.get player (wonID)).name) ^ " has won") ::
                         (if !badAnswer then (!((Array.get player ((wonID+1) mod 2)).name) ^ " mistake were made") else "") ::
                         ("") ::
                         ("") ::
                         (!((Array.get player (0)).name) ^ " : " ^ string_of_int(!((Array.get player (0)).score))) ::
                         (!((Array.get player (1)).name) ^ " : " ^ string_of_int(!((Array.get player (1)).score))) ::
                         (if !gameLeft=0 then
                           (if (score0 = score1) then
                                ("- Equality !")
                            else (
                      "- "^  (if (score0 > score1) then
                                (!((Array.get player (0)).name))
                              else
                                (!((Array.get player (1)).name)))
                                 ^ " has won the Game"))
                          else "")
                           ::
                         ("Press key to continue ...")::[];
;;

(*
let newCode () = (*
 codeArray*)

;;*)

let addCodeAnswer x =
  let im = (if x = answerWellplacedID then answerWellplaced else answerMisplaced) in
  Array.fold_left (fun acc c ->(*print_endline "rec";*) if not (fst (fst(acc))) then
                                  let i = snd acc in
                                  match c with
                                  | None ->  Array.set codeAnswer i (Some({id = x; imAns = {imageo = im;
                                                                     xo = ref (floatc(answerTokenOutputX* !Graph.pxIN + snd (fst acc)));
                                                                     yo = ref (floatc ((answerTokenY-answerTokenStepY* !codeSelected + 10)* !Graph.pxIN-(im.height)));
                                                                     objx = ref (floatc(answerTokenOutputEndX* !Graph.pxIN + snd (fst acc)));
                                                                     objy = ref (floatc ((answerTokenY-answerTokenStepY* !codeSelected + 10)* !Graph.pxIN-(im.height)));
                                                                     k = 0.1}})); ((true, 0), i)
                                  | Some(x) -> ((false, snd (fst acc)+x.imAns.imageo.width+ 1* !Graph.pxIN), i+1)
                                else acc
                   ) ((false, 0), 0) codeAnswer; ()
;;

let rec makeListIAanswer x l =
  match x with
  | (0, 0) -> l
  | (0, v) -> makeListIAanswer (0, v-1) (l@[answerMisplacedID])
  | (v, k) -> makeListIAanswer (v-1, k) (l@[answerWellplacedID])
;;

(*
let endAnswerIA () =
  let colorList = fst (Array.fold_left (fun acc c -> let i = snd acc in
                                                      match c with
                                                      | Some(x) -> Array.set codeArray i None; ((fst acc)@[x], i+1)
                                                      | _ -> print_endline "ERROR AT endAnswer () | codeArray have None"; acc
                                         ) ([], 0) codeArray) in
  let answer = match Code.reponse colorList (!secretCode) with
                | Some(x) -> x
                | _ -> print_endline "ERROR AT endAnswerIA | Code.reponse return None";(-1, -1)
  in
  List.fold_left (fun acc c -> addCodeAnswer c;) () (makeListIAanswer answer []);
  if fst answer = Code.nombre_pions then ((* updateGameMode *)) (*MESSAGE DE FIN ENDROUND*)
  else if !codeSelected = codeSelectedMax then ((* updateGameMode *)) (*MESSAGE DE FIN ENDROUND*)
  else
  (codeArrayList :=  !codeArrayList@[{color = colorList;
              listAnswer = fst (Array.fold_left (fun acc c -> let i = snd acc in
                                                              match c with
                                                              | Some(x) -> Array.set codeAnswer i None; ((fst acc)@[x], i+1)
                                                              | _ -> acc
                                                  ) ([], 0) codeAnswer)}];
  updateGameMode makeCodeMode)
;;*)

let putSecretCode () =
  (List.fold_left (fun acc c -> Array.set secretCodeMake acc (Some(c)); acc+1) (0) !secretCode)
;;

let rec updateGameMode x =
  gameMode := x;
  if !gameMode = makeSecretCodeMode then
   (
     if getTypePlayerBreaker (!codeMaker) >= 0 then
      (
        secretCode := List.nth Code.tous (Random.int (List.length Code.tous));
        putSecretCode ();
        updateGameMode makeCodeMode;
      )
     else
      (
      initMakeSecretCodeMode ()
      )
    )
  else if !gameMode = makeCodeMode then
    ( updateCodeSelected ();
      if !codeSelected = 0 then
      (
        secretCodeBoard.objy := floatc(-secretCodeBoard.imageo.height);
        );
      if getTypePlayerBreaker (!codeBreaker) >= 0 then
      (
        let output = IA.choix (getTypePlayerBreaker (!codeBreaker)) !listEssaiPropose !listEssaiPossible in
        (List.fold_left (fun acc c -> Array.set codeArray acc (Some(c)); acc+1) 0 output);
        updateGameMode makeAnswerMode
      )
      else initMakeCodeMode ()
      )
  else if !gameMode = makeAnswerMode then
    (
      if getTypePlayerMaker (!codeMaker) >= 0 then
        (
          let colorList = Array.fold_left (fun acc c -> match c with
                                                        | Some(x) -> acc@[x]
                                                        | _ -> print_endline "ERROR AT endAnswer () | codeArray have None"; acc
                                                 ) [] codeArray in
          let answer = match Code.reponse colorList (!secretCode) with
                        | Some(x) -> x
                        | _ -> print_endline "ERROR AT endAnswerIA | Code.reponse return None";(-1, -1)
          in
          List.fold_left (fun acc c -> addCodeAnswer c;) () (makeListIAanswer answer []);
          if fst answer = Code.nombre_pions then
            (codeBreakerWon := true;
             updateGameMode makeEndMode;(* updateGameMode *)) (*MESSAGE DE FIN ENDROUND*)
          else if !codeSelected = codeSelectedMax then
           (codeBreakerWon := false;
            updateGameMode makeEndMode;(* updateGameMode *)) (*MESSAGE DE FIN ENDROUND*)
          else
          (if getTypePlayerBreaker (!codeBreaker) >= 0 then
            (
            listEssaiPossible := IA.filtre (getTypePlayerBreaker (!codeBreaker)) (colorList, Some(answer)) !listEssaiPossible;
            listEssaiPropose := !listEssaiPropose@[colorList]
            );
           blankCodeArray codeArray;
           codeArrayList :=  !codeArrayList@[{color = colorList;
                                              listAnswer = fst (Array.fold_left (fun acc c -> let i = snd acc in
                                                                                              match c with
                                                                                              | Some(x) -> Array.set codeAnswer i None; ((fst acc)@[x], i+1)
                                                                                              | _ -> acc
                                                                                  ) ([], 0) codeAnswer)}];
          updateGameMode makeCodeMode)
        )
      else (initMakeAnswerMode ())
      )
    else if !gameMode = makeEndMode then
    (
      initMakeEndMode ()
      );
;;

let initNewGame () =
  secretCode := [];
  codeBreaker := (!codeBreaker+1) mod 2;
  codeMaker := ((!codeBreaker+1) mod 2);
  codeBreakerWon := false;
  listEssaiPropose := [];
  listEssaiPossible := Code.tous;
  badAnswer := false;
  codeArrayList := [];
  blankCodeArray secretCodeMake;
  blankCodeArray codeArray;
  blankCodeArray codeAnswer;
  codeSelected := -1;
  tokenSelected := -1;
  gameLeft := !gameLeft-1;
  secretCodeBoard.yo := floatc(-secretCodeBoard.imageo.height);
  secretCodeBoard.objy := floatc(-secretCodeBoard.imageo.height);
  messageEndImage.img.objx := floatc (!Graph.width);
  updateGameMode makeSecretCodeMode;
;;

let drawCodeArrayList () =
  List.fold_left (fun accy c -> List.fold_left (fun accx c_ -> let color = match c_ with
                                                                          | Code.Color(x) -> x
                                                                          | _ -> -1 in
                                                              Graph.image (List.nth tokenList color) (((tokenCodeStartX+accx*tokenCodeStepX+1)* !Graph.pxIN)) (((tokenCodeStartY-accy*tokenCodeStepY+1)* !Graph.pxIN));
                                                              accx+1
                                               ) 0 c.color;
                                List.fold_left (fun acc_ c_ -> imageObj (c_.imAns); acc_
                                                ) 0 c.listAnswer;
                                  accy+1
                  ) 0 (!codeArrayList)
;;

let drawCodeAnswer () =
  Array.fold_left (fun acc c -> match c with
                                  | Some(x) -> imageObj (x.imAns)
                                  | _ -> ();
                   ) () codeAnswer
;;

let compareUserAnswer ans =
  let answerIA = (match (Code.reponse (Array.fold_left (fun acc c -> match c with
                                                | Some(x) -> acc@[x]
                                                | _ -> print_endline "ERROR AT endAnswer () | codeArray have None"; acc
                                         ) [] codeArray) (!secretCode)) with
                  | Some(x) -> x
                  | _ -> (-1, -1)) in
    badAnswer := !badAnswer || ((fst ans <> fst answerIA) || (snd ans <> snd answerIA));
;;

let endAnswer () =
  let answer = (Array.fold_left (fun acc c -> match c with
                                              | Some(x) -> if x.id = answerWellplacedID then (fst acc +1, snd acc) else (fst acc, snd acc+1)
                                              | _ -> acc
                                 ) (0, 0) codeAnswer) in
  compareUserAnswer answer;
  if (fst answer) = Code.nombre_pions then
    (codeBreakerWon := true;
     updateGameMode makeEndMode(* updateGameMode *)) (*MESSAGE DE FIN ENDROUND*)
  else if !codeSelected = codeSelectedMax then
    (codeBreakerWon := false;
     updateGameMode makeEndMode(* updateGameMode *)) (*MESSAGE DE FIN ENDROUND*)
  else
    (let colorList = fst (Array.fold_left (fun acc c -> let i = snd acc in
                                                         match c with
                                                         | Some(x) -> Array.set codeArray i None; ((fst acc)@[x], i+1)
                                                         | _ -> print_endline "ERROR AT endAnswer () | codeArray have None"; acc
                                            ) ([], 0) codeArray) in
      if getTypePlayerBreaker (!codeBreaker) >= 0 then
      (
      listEssaiPossible := IA.filtre (getTypePlayerBreaker (!codeBreaker)) (colorList, Some(answer)) !listEssaiPossible;
      listEssaiPropose := !listEssaiPropose@[colorList]
      );
     codeArrayList :=  !codeArrayList@[{color = colorList;
                                                listAnswer = fst (Array.fold_left (fun acc c -> let i = snd acc in
                                                                                                match c with
                                                                                                | Some(x) -> Array.set codeAnswer i None; ((fst acc)@[x], i+1)
                                                                                                | _ -> acc
                                                    ) ([], 0) codeAnswer)}];
    updateGameMode makeCodeMode)
;;

let supCodeAnswer v =
  Array.fold_left (fun acc c -> let i = snd acc in
                                if not (fst (fst acc)) then
                                  match c with
                                   | Some(x) -> if x.id = v then ((true, snd (fst acc)), i+1)
                                                else ((false, snd (fst acc)+x.imAns.imageo.width+ 1* !Graph.pxIN), i+1)
                                   | _ -> acc
                                else
                                  match c with
                                    | Some(x) -> x.imAns.objx := floatc(answerTokenOutputEndX* !Graph.pxIN + snd (fst acc));
                                                 Array.set codeAnswer (i-1) c;
                                                 Array.set codeAnswer (i) None;
                                                ((true, snd (fst acc)+x.imAns.imageo.width+ 1* !Graph.pxIN), i+1)
                                    | _ -> Array.set codeAnswer (i-1) c; (fst acc, i+1)
                   ) ((false, 0), 0) codeAnswer; ()
;;

let codeAnswerHitBox () =
  if !gameMode= makeAnswerMode then
   (
     let mx = !Graph.mouseX in
     let my = !Graph.mouseY in
     if floatc(my) >= !(answerTokenObj.yo) && floatc(my) <= !(answerTokenObj.yo)+.floatc (10* !Graph.pxIN) then
      (
         if floatc(mx) >= !(answerTokenObj.xo) && floatc(mx) <= !(answerTokenObj.xo)+.floatc(10* !Graph.pxIN) then
          (endAnswer ()
          ); (*Validation Answer*)
         let addSup = List.fold_left (fun acc c -> let kx = (List.fold_left (fun acc_ c_ -> (*Graph.fill (if c = 0 then Graph.color 255 0 0 else Graph.color 255 255 255);
                                                                                            Graph.line (roundInt(!(answerTokenObj.xo)+.floatc((13+c*29+c_*(if c = 0 then 19 else 14))* !Graph.pxIN))) (roundInt(!(answerTokenObj.yo))) (roundInt(!(answerTokenObj.xo)+.floatc((13+c*29+c_*(if c = 0 then 19 else 14)+7)* !Graph.pxIN))) (roundInt( !(answerTokenObj.yo)+.floatc (10* !Graph.pxIN)));*)
                                                                                            if (floatc (mx) >= !(answerTokenObj.xo)+.floatc((13+c*29+c_*(if c = 0 then 19 else 14))* !Graph.pxIN) &&
                                                                                                floatc(mx) <= !(answerTokenObj.xo)+.floatc((13+c*29+c_*(if c = 0 then 19 else 14)+7)* !Graph.pxIN)) then c_
                                                                                           else acc_
                                                                             ) (-1) [0; 1]) in
                                                   if (kx <> -1) then (c, kx)
                                                   else acc
                                      ) (-1, -1) [0; 1] in (* On aurai pu utiliser une boucle mais par contrainte on utilise un fold_left *)
         if fst addSup <> -1 then (* + *)
          (
            if snd addSup = 0 then (addCodeAnswer (fst (addSup)))
            else (supCodeAnswer (fst (addSup)))
            );
      );
     );
;;

let codeSelectionHitbox ar x y =
fst(Array.fold_left (fun acc c -> let i = snd acc in
                                  match tokenCodeHitBox (x+((i*tokenCodeStepX)* !Graph.pxIN)) y with
                                    | true -> Array.set ar i (Some(Code.Color( !tokenSelected))); (-1, i+1)
                                    | false -> (fst acc, i+1)
                      ) (!tokenSelected,0) ar)
;;

let codeSelection () =
  if !tokenSelected <> -1 then
    (
    if !gameMode = makeSecretCodeMode then
      (let newTokenSelected = codeSelectionHitbox secretCodeMake ((roundInt (!(secretCodeBoard.xo)) )+(4* !Graph.pxIN)) ((roundInt (!(secretCodeBoard.yo)) )+(72* !Graph.pxIN)) in
      if newTokenSelected = -1 then
        (tokenSelected := newTokenSelected;
          if Array.fold_left (fun acc c -> acc && c <> None) true secretCodeMake then
            (secretCode := (Array.fold_left (fun acc c -> match c with
                                                            | Some(c) -> acc@[c]
                                                            | _ -> acc) [] secretCodeMake);
             updateGameMode makeCodeMode;
            );
        );
      );
    if !gameMode = makeCodeMode then
      (let newTokenSelected = codeSelectionHitbox codeArray (tokenCodeStartX* !Graph.pxIN) ((tokenCodeStartY-(!codeSelected)*tokenCodeStepY)* !Graph.pxIN) in
      if newTokenSelected = -1 then
        (tokenSelected := newTokenSelected;
          if Array.fold_left (fun acc c -> acc && c <> None) true codeArray then
          updateGameMode makeAnswerMode;
          );
      );
    );
;;

let screenGame () =
  let choiceScr = ref 1 in
  Graph.background (Graph.color 0 0 0);
  image back;
  drawCodeAnswer ();
  image board;
  drawCodeArrayList ();
  drawCodeArray codeArray ((tokenCodeStartX+1)* !Graph.pxIN) (((tokenCodeStartY-(! codeSelected)*tokenCodeStepY+1)* !Graph.pxIN));
  imageObj codeSelectedImage;
  drawSecretCodeBoard ();
  imageObj answerTokenObj;
  drawTokenList ();
  Graph.fill(Graph.color 255 255 255);
  imageTxtMessageObj messageEndImage;
  if !gameMode <> makeEndMode then
    (
      Graph.text ((!((Array.get player ((if (!gameMode = makeCodeMode) then !codeBreaker else !codeMaker))).name))^" turn") (0) (0);
    );
  (*Graph.text (string_of_int !Graph.frameCount) 0 0;
  Graph.text (string_of_int !codeSelected) 0 40;
  Graph.text (string_of_int(!Graph.mouseX) ^ " " ^ string_of_int(!Graph.mouseY)) 0 80;
  Graph.text (string_of_int(fst (Graphics.mouse_pos ())) ^ " " ^ string_of_int(snd (Graphics.mouse_pos ()))) 0 120;*)
  (*Graph.rect (!Graph.mouseX -20) (!Graph.mouseY-20) (20) (20);*)
  if Graphics.key_pressed () then
    (match Graphics.read_key () with
      | _ -> if !gameMode = makeEndMode then
              if !gameLeft <> 0 then (initNewGame ())
              else (initStartBool := true; choiceScr := 0)
    )
  ;
  if !Graph.mousePressed then
    (
    codeSelection();
    tokenListSelectedHitBox ();
    codeAnswerHitBox();
    if !Graph.mouseX >= (!Graph.width-20) && !Graph.mouseX <= !Graph.width && !Graph.mouseY >= (0) && !Graph.mouseY <= 20 then (choiceScr := -1);
    )
  ;
  !choiceScr
;;

(* Partie screenStart _______________________*)

let startTxtPxIN = 4;;
let maxLengthName = 9;;
let defaultName = "Player";;

let nbRoundSelected = ref 2;;
let playerSelectedText = ref (-1);;
let playerSelectedMode = ref (-1);;

let colorOnIcon = Graph.color 52 163 243;;
let colorOnIconSel = Graph.color 243 27 27;;

let isOnImageObj im =
  !Graph.mouseX >= roundInt(!(im.xo)) &&
  !Graph.mouseX <= roundInt(!(im.xo))+im.imageo.width &&
  !Graph.mouseY >= roundInt(!(im.yo)) &&
  !Graph.mouseY <= roundInt(!(im.yo))+im.imageo.height
;;

let imageStartPlayIcon = {imageo = Graph.loadImagePxSet "./data/StartPlayIcon.im" startTxtPxIN; xo = ref 0.; yo = ref 0.; objx = ref 0.; objy = ref 0.; k = 0.1};;
let imageStartBackIcon = {imageo = Graph.loadImagePxSet "./data/StartBackIcon.im" startTxtPxIN; xo = ref 0.; yo = ref 0.; objx = ref 0.; objy = ref 0.; k = 0.1};;
let imageStartRoundIm = {imageo = Graph.loadImagePxSet "./data/StartRoundSelection.im" startTxtPxIN; xo = ref 0.; yo = ref 0.; objx = ref 0.; objy = ref 0.; k = 0.1};;

let startRoundHitBox x y =
  !Graph.mouseX >= x && !Graph.mouseX <= x+imageStartRoundIm.imageo.width &&
  !Graph.mouseY >= y && !Graph.mouseY <= y+startTxtPxIN*8
;;

let imageStartRoundHitBox () =
  if startRoundHitBox (roundInt !(imageStartRoundIm.xo)) (roundInt !(imageStartRoundIm.yo)) then
   (nbRoundSelected := minvalue (!nbRoundSelected+2) 98)
  else if startRoundHitBox (roundInt !(imageStartRoundIm.xo)) (roundInt !(imageStartRoundIm.yo)+17*startTxtPxIN) then
   (nbRoundSelected := maxvalue (!nbRoundSelected-2) 2);
;;

let imageStartRound () =
  imageObj imageStartRoundIm;
  let txt = (string_of_int(!nbRoundSelected)) in
  let dim = Graphics.text_size txt in
  Graph.fill (Graph.color 255 255 255);
  Graph.text txt ((roundInt !(imageStartRoundIm.xo)) + imageStartRoundIm.imageo.width/2-(fst dim)/2) ((roundInt !(imageStartRoundIm.yo)) + imageStartRoundIm.imageo.height/2-(snd dim)/2);
;;

type imagePlayerIcon = {playerData : player; imgPlayerIcon : image}

let imagePlayerModeIconIm = Graph.loadImagePxSet "./data/StartPlayerModeIcon.im" startTxtPxIN;;
let playerModeList = "Human"::"Human a"::"IA easy"::"IA hard"::[];;

let imagePlayerIconIm = Graph.loadImagePxSet "./data/StartPlayerIcon.im" startTxtPxIN;;
let arrayPlayerIcon = Array.init 2 (fun c -> {
  playerData = {name = ref ("Player "^string_of_int c); typePlayerBreaker = ref (-1); typePlayerMaker = ref (-1); score = ref 0
    };
  imgPlayerIcon =
    {image = imagePlayerIconIm;
     x = ref (floatc ((!(Graph.width))/2 - imagePlayerIconIm.width/2));
     y = ref (floatc ((!(Graph.height))/2 + (if c = 0 then -8*startTxtPxIN-imagePlayerIconIm.height/2 else 8*startTxtPxIN)))}});;


let isOnImagePlayerModeIcon x y =
  !Graph.mouseX >= x &&
  !Graph.mouseX <= x+42*startTxtPxIN &&
  !Graph.mouseY >= y &&
  !Graph.mouseY <= y+imagePlayerModeIconIm.height
;;

let identifyPlayerMode c =
  let a = !(c.typePlayerBreaker) in
  let b = !(c.typePlayerMaker) in
  if a = -1 && b = -1 then 0
  else if a = -1 && b = 0 then 1
  else if a = 0 && b = 0 then 2
  else if a = 1 && b = 0 then 3
  else (-1)
;;

let imagePlayerModeIcon xc yc c b s =
  Graph.image imagePlayerModeIconIm xc yc;
  if (b || s) then
    (
      Graph.fill (if b then colorOnIcon else colorOnIconSel);
      Graph.rect (xc+31*startTxtPxIN) (yc+1*startTxtPxIN) (1*startTxtPxIN) (7*startTxtPxIN);
      );
  Graph.fill (Graph.color 255 255 255);
  let dim = Graphics.text_size c in
  Graph.text c (xc+imagePlayerModeIconIm.width/2-(fst dim)/2) (yc+imagePlayerModeIconIm.height/2-(snd dim)/2);
;;

let drawImageListPlayerModeIcon () =
  if !playerSelectedMode <> -1 then
    (
    let mode = identifyPlayerMode ((Array.get arrayPlayerIcon (!playerSelectedMode)).playerData) in
    let y = ((roundInt(!((Array.get arrayPlayerIcon (!playerSelectedMode)).imgPlayerIcon.y))) + (Array.get arrayPlayerIcon (!playerSelectedMode)).imgPlayerIcon.image.height) in
    let xc = (roundInt(!((Array.get arrayPlayerIcon (!playerSelectedMode)).imgPlayerIcon.x))+ (Array.get arrayPlayerIcon (!playerSelectedMode)).imgPlayerIcon.image.width)-imagePlayerModeIconIm.width in
    (List.fold_left (fun acc c -> let i = snd acc in
                                  let h = fst acc in
                                  let yc = (y+i*(imagePlayerModeIconIm.height-startTxtPxIN)) in
                                  let b = isOnImagePlayerModeIcon xc yc in
                                  imagePlayerModeIcon xc yc c (b && (not h)) (mode = i);
                                  (h || b, i+1)
                     ) (false, 0) playerModeList); ()
    );
;;

let setPlayer i a b =
  (Array.get arrayPlayerIcon i).playerData.typePlayerBreaker := a;
  (Array.get arrayPlayerIcon i).playerData.typePlayerMaker := b;
;;

let isOnTextImageIcon im =
  !Graph.mouseX >= roundInt(!(im.imgPlayerIcon.x)) &&
  !Graph.mouseX <= roundInt(!(im.imgPlayerIcon.x))+42*startTxtPxIN &&
  !Graph.mouseY >= roundInt(!(im.imgPlayerIcon.y)) &&
  !Graph.mouseY <= roundInt(!(im.imgPlayerIcon.y))+im.imgPlayerIcon.image.height
;;

let isOnModeImageIcon im =
  !Graph.mouseX > roundInt(!(im.imgPlayerIcon.x))+42*startTxtPxIN &&
  !Graph.mouseX <= roundInt(!(im.imgPlayerIcon.x))+im.imgPlayerIcon.image.width &&
  !Graph.mouseY >= roundInt(!(im.imgPlayerIcon.y)) &&
  !Graph.mouseY <= roundInt(!(im.imgPlayerIcon.y))+im.imgPlayerIcon.image.height
;;

let imagePlayerIcon im =
  image (im.imgPlayerIcon);
  if ((!playerSelectedMode) = -1 && isOnTextImageIcon im) then
    (Graph.fill (colorOnIcon);
     Graph.rect (roundInt(!(im.imgPlayerIcon.x))+1*startTxtPxIN) (roundInt(!(im.imgPlayerIcon.y))+1*startTxtPxIN) (1*startTxtPxIN) (startTxtPxIN*7);
     Graph.rect (roundInt(!(im.imgPlayerIcon.x))+40*startTxtPxIN) (roundInt(!(im.imgPlayerIcon.y))+1*startTxtPxIN) (1*startTxtPxIN) (startTxtPxIN*7);
      );
  Graph.fill (Graph.color 255 255 255);
  let dim = Graphics.text_size (!(im.playerData.name)) in
  Graph.text (!(im.playerData.name))
  (roundInt(!(im.imgPlayerIcon.x))+21*startTxtPxIN-((fst (dim))/2))
  (roundInt(!(im.imgPlayerIcon.y))+im.imgPlayerIcon.image.height/2-((snd (dim))/2));
;;

type imageIcon = {text : string ref; imgIcon : image};;

let iconIDPlay = 0;;
let iconIDQuit = 1;;
let imageIconSelection = Graph.loadImagePxSet "./data/StartIconSelection.im" startTxtPxIN;;
let iconList =
{text = ref "play"; imgIcon =
  {image = imageIconSelection; x = ref 0.; y = ref 0.}}::
{text = ref "quit"; imgIcon =
  {image = imageIconSelection; x = ref 0.; y = ref 0.}}
::[];;

let isOnImageIcon im =
  !Graph.mouseX >= roundInt(!(im.imgIcon.x)) &&
  !Graph.mouseX <= roundInt(!(im.imgIcon.x))+im.imgIcon.image.width &&
  !Graph.mouseY >= roundInt(!(im.imgIcon.y)) &&
  !Graph.mouseY <= roundInt(!(im.imgIcon.y))+im.imgIcon.image.height
;;

let imageIcon im =
  image (im.imgIcon);
  if (isOnImageIcon im) then
    (Graph.fill (colorOnIcon);
     Graph.rect (roundInt(!(im.imgIcon.x))+startTxtPxIN*1) (roundInt(!(im.imgIcon.y))+startTxtPxIN*1) (startTxtPxIN*1) (startTxtPxIN*7);
     Graph.rect (roundInt(!(im.imgIcon.x))+startTxtPxIN*40) (roundInt(!(im.imgIcon.y))+startTxtPxIN*1) (startTxtPxIN*1) (startTxtPxIN*7)
      );
  Graph.fill (Graph.color 255 255 255);
  let dim = Graphics.text_size (!(im.text)) in
  Graph.text (!(im.text))
  (roundInt(!(im.imgIcon.x))+im.imgIcon.image.width/2-((fst (dim))/2))
  (roundInt(!(im.imgIcon.y))+im.imgIcon.image.height/2-((snd (dim))/2));
;;

let startScreenMenu = 0;;
let startScreenPlay = 1;;
let startScreenMode = ref 0;;
let startBack = loadImagePx "./data/StartBack.im" 0. 0.;;
let startBackLayer = loadImagePx "./data/StartBackLayer.im" 0. 0.;;
let logo = {imageo = Graph.loadImagePxSet "./data/Logo.im" 3; xo = ref 0.; yo = ref 0.; objx = ref 0.; objy = ref 0.; k = 0.1};;

let updateStartMode x =
  startScreenMode := x;
  if !startScreenMode = startScreenMenu then
   (
      imageStartPlayIcon.objx := floatc (171*startTxtPxIN);
      imageStartBackIcon.objx := floatc (29*startTxtPxIN - imageStartBackIcon.imageo.width);
      imageStartRoundIm.objx := floatc (171*startTxtPxIN);
      logo.objy := floatc (14* !Graph.pxIN)
    )
  else if !startScreenMode = startScreenPlay then
    (
      imageStartPlayIcon.objx := floatc (153*startTxtPxIN);
      imageStartBackIcon.objx := floatc (38*startTxtPxIN);
      imageStartRoundIm.objx := floatc (153*startTxtPxIN);
      logo.objy := floatc (-logo.imageo.height)
    );
;;

let initStartScreen () =
  logo.xo := floatc (!Graph.width/2-logo.imageo.width/2);
  logo.objx := !(logo.xo);
  logo.yo := floatc (-logo.imageo.height);
  (List.fold_left (fun acc c -> c.imgIcon.x := floatc (!Graph.width/2-c.imgIcon.image.width/2); c.imgIcon.y := floatc (!Graph.height/2+acc*(c.imgIcon.image.height+startTxtPxIN*7)); acc+1) 0 iconList);
  updateStartMode startScreenMenu;
  nbRoundSelected := 2;
  playerSelectedText := -1;
  playerSelectedMode := -1;
  imageStartPlayIcon.xo := floatc (171*startTxtPxIN);
  imageStartPlayIcon.yo := floatc (138*startTxtPxIN);
  imageStartPlayIcon.objx := !(imageStartPlayIcon.xo);
  imageStartPlayIcon.objy := !(imageStartPlayIcon.yo);
  imageStartBackIcon.xo := floatc (29*startTxtPxIN - imageStartBackIcon.imageo.width);
  imageStartBackIcon.yo := floatc (138*startTxtPxIN);
  imageStartBackIcon.objx := !(imageStartBackIcon.xo);
  imageStartBackIcon.objy := !(imageStartBackIcon.yo);
  imageStartRoundIm.xo := floatc (171*startTxtPxIN);
  imageStartRoundIm.yo := floatc (106*startTxtPxIN);
  imageStartRoundIm.objx := !(imageStartPlayIcon.xo);
  imageStartRoundIm.objy := !(imageStartRoundIm.yo);
  (Array.fold_left (fun acc c -> Array.set arrayPlayerIcon acc
    ({playerData = {name = ref (defaultName^" "^string_of_int (acc+1));
                    typePlayerBreaker = ref (-1);
                    typePlayerMaker = ref (-1);
                    score = ref 0
                    };
      imgPlayerIcon = {image = imagePlayerIconIm;
                       x = ref (floatc ((!(Graph.width))/2 - imagePlayerIconIm.width/2));
                       y = ref (floatc ((!(Graph.height))/2 + (if acc = 0 then -8*startTxtPxIN-imagePlayerIconIm.height/2 else 8*startTxtPxIN)))}
                       });
                      acc+1) 0 arrayPlayerIcon);
;;

let drawImageIcon () =
 (List.fold_left (fun acc c -> imageIcon c; acc) () iconList)
;;

let drawImagePlayerIcon () =
  (Array.fold_left (fun acc c -> imagePlayerIcon (c);
                                 ()
                    ) () arrayPlayerIcon);
;;

let imageIconHitBox () =
  fst (List.fold_left (fun acc c -> if isOnImageIcon c then (snd acc, snd acc +1)
                                    else (fst acc, snd acc +1)
                 ) (-1,0) iconList)
;;

let nameKeyPressed c =
  if !playerSelectedText <> -1 then (
    match (int_of_char c) with
    | 8 -> (Array.get arrayPlayerIcon (!playerSelectedText)).playerData.name := String.sub (!((Array.get arrayPlayerIcon (!playerSelectedText)).playerData.name)) 0
     (maxvalue ((String.length (!((Array.get arrayPlayerIcon (!playerSelectedText)).playerData.name))) -1) 0);
    | 10 -> playerSelectedText := -1;
    | x when ((x >= 32) && (x <= 126)) && (String.length (!((Array.get arrayPlayerIcon (!playerSelectedText)).playerData.name))) < maxLengthName ->
      (Array.get arrayPlayerIcon (!playerSelectedText)).playerData.name := (!((Array.get arrayPlayerIcon (!playerSelectedText)).playerData.name)) ^ (String.make 1 c);
    | _ -> ()
    );
;;

let nameHitboxImagePlayerIcon () =
  if !playerSelectedText <> -1 && String.length (!((Array.get arrayPlayerIcon (!playerSelectedText)).playerData.name)) = 0  then
    (
      (Array.get arrayPlayerIcon (!playerSelectedText)).playerData.name :=  (defaultName^" "^string_of_int ((!playerSelectedText)+1))
      );
  playerSelectedText := if !playerSelectedMode <> -1 then -1 else (
  fst (Array.fold_left (fun acc c -> if (isOnTextImageIcon c) then
                                      (
                                        c.playerData.name := "";
                                        (snd acc, snd acc+1)
                                      )
                                     else (fst acc, snd acc+1)
                        ) (-1, 0) arrayPlayerIcon));
;;

let modeHitboxImagePlayerIcon () =
if !playerSelectedMode <> -1 then
  (
    let y = ((roundInt(!((Array.get arrayPlayerIcon (!playerSelectedMode)).imgPlayerIcon.y))) + (Array.get arrayPlayerIcon (!playerSelectedMode)).imgPlayerIcon.image.height) in
    let xc = (roundInt(!((Array.get arrayPlayerIcon (!playerSelectedMode)).imgPlayerIcon.x))+ (Array.get arrayPlayerIcon (!playerSelectedMode)).imgPlayerIcon.image.width)-imagePlayerModeIconIm.width in
    let output = fst
    (List.fold_left (fun acc c -> let i = snd acc in
                                 let index = fst acc in
                                 let yc = (y+i*(imagePlayerModeIconIm.height-startTxtPxIN)) in
                                 if (index = (-1)) && (isOnImagePlayerModeIcon xc yc) then
                                   (i, i+1)
                                 else
                                   (index, i+1)
                    ) (-1, 0) playerModeList) in
    match output with
    | 0 -> setPlayer (!playerSelectedMode) (-1) (-1);      playerSelectedMode := -2;
    | 1 -> setPlayer (!playerSelectedMode) (-1) 0;      playerSelectedMode := -2;
    | 2 -> setPlayer (!playerSelectedMode) 0 0;      playerSelectedMode := -2;
    | 3 -> setPlayer (!playerSelectedMode) 1 0;      playerSelectedMode := -2;
    | _ -> ()
  );
  if !playerSelectedMode = -2 then (playerSelectedMode := -1)
  else
    (
  playerSelectedMode :=
  fst (Array.fold_left (fun acc c -> if (isOnModeImageIcon c) then
                                      (
                                        (snd acc, snd acc+1)
                                      )
                                     else (fst acc, snd acc+1)
                        ) (-1, 0) arrayPlayerIcon);
                        )
;;

let launchGame () =
  gameLeft := !nbRoundSelected;
  codeBreaker := Random.int 2;
  gameLeft := !nbRoundSelected;
  Array.fold_left (fun acc c -> Array.set player acc (c.playerData);
                                acc+1
                   ) 0 arrayPlayerIcon;
  initNewGame ();
;;

let screenStart () =
  let choiceScr = ref 0 in
  if !initStartBool then
    ( initStartBool := false;
      initStartScreen (); ()
    );
  image startBack;
  imageObj imageStartPlayIcon;
  imageObj imageStartBackIcon;
  imageStartRound ();
  image startBackLayer;
  imageObj logo;
  if !startScreenMode = startScreenMenu then
    (
      drawImageIcon();
    );
  if !startScreenMode = startScreenPlay then
    (
       drawImagePlayerIcon ();
       drawImageListPlayerModeIcon ();
    );
  Graph.fill (Graph.color 255 255 255);
  (*Graph.line (!Graph.mouseX) 0 (!Graph.mouseX) (!Graph.height);
  Graph.line (0) (!Graph.mouseY) (!Graph.width) (!Graph.mouseY);*)
  (*Graph.text (string_of_int !Graph.frameCount) 0 0;
  Graph.text (string_of_int !codeSelected) 0 40;
  Graph.text (string_of_int(!Graph.mouseX) ^ " " ^ string_of_int(!Graph.mouseY)) 0 80;
  Graph.text (string_of_int(!playerSelectedText)) 0 120;
  Graph.text (string_of_int(!playerSelectedMode)) 0 160;
  Graph.text (string_of_int(!((Array.get arrayPlayerIcon 0).playerData.typePlayerBreaker)) ^ " " ^ string_of_int(!((Array.get arrayPlayerIcon 0).playerData.typePlayerMaker))) 0 200;
  Graph.text (string_of_int(!((Array.get arrayPlayerIcon 1).playerData.typePlayerBreaker)) ^ " " ^ string_of_int(!((Array.get arrayPlayerIcon 1).playerData.typePlayerMaker))) 0 220;
  *)if Graphics.key_pressed () then
    (
    let c = Graphics.read_key () in
    if !startScreenMode = startScreenPlay then
      (
      nameKeyPressed c
      );
    );
  if !Graph.mousePressed then
    (
      if !startScreenMode = startScreenMenu then
        (
          let i = imageIconHitBox () in
          if i = iconIDPlay then (updateStartMode startScreenPlay);
          if i = iconIDQuit then ( (choiceScr := -1) );
          )
      else if !startScreenMode = startScreenPlay then
        (
          if (!playerSelectedMode) = -1 && (!playerSelectedText) = -1 then
            (
              imageStartRoundHitBox ();
              if isOnImageObj imageStartPlayIcon then (launchGame (); choiceScr := 1);
              if isOnImageObj imageStartBackIcon then (updateStartMode startScreenMenu);
            );
          nameHitboxImagePlayerIcon ();
          modeHitboxImagePlayerIcon ();
          );
    if !Graph.mouseX >= (!Graph.width-20) && !Graph.mouseX <= !Graph.width && !Graph.mouseY >= (0) && !Graph.mouseY <= 20 then (choiceScr := -1);
    );
  !choiceScr
;;

let screenList = [screenStart; screenGame];;

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