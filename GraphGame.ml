#use "GraphLib.ml";;
#use "Code.ml";;
#use "IA.ml";;

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
  let my = !Graph.mouseY in Graph.line (x+(1)* !Graph.pxIN) (y+(1)* !Graph.pxIN) (x+(tokenCodeStepX-2)* !Graph.pxIN) (y+(tokenCodeStepX-2)* !Graph.pxIN);
  Graph.line (x+(3)* !Graph.pxIN ) (y) (x+(8)* !Graph.pxIN) (y+(11)* !Graph.pxIN);
  Graph.line (x) (y+(3)* !Graph.pxIN) (x+11* !Graph.pxIN) (y+(8)* !Graph.pxIN);
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
  let oldscore = !((Array.get player (wonID)).score) in print_endline (string_of_int oldscore);
  Array.set player (wonID) ({name = (Array.get player (wonID)).name; typePlayerBreaker = (Array.get player (wonID)).typePlayerBreaker; typePlayerMaker = (Array.get player (wonID)).typePlayerMaker; score = ref (1+ oldscore)});
  messageEndImage.txt := (!((Array.get player (wonID)).name) ^ " has won") ::
                         (if !badAnswer then (!((Array.get player ((wonID+1) mod 2)).name) ^ " hasn't answered correctly") else "") ::
                         ("") ::
                         ("") ::
                         (!((Array.get player (0)).name) ^ " : " ^ string_of_int(!((Array.get player (0)).score))) ::
                         (!((Array.get player (1)).name) ^ " : " ^ string_of_int(!((Array.get player (1)).score))) ::
                         ("") ::
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
     if getTypePlayerMaker (!codeMaker) >= 0 then
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
  Graph.text (string_of_int !Graph.frameCount) 0 0;
  Graph.text (string_of_int !codeSelected) 0 40;
  Graph.text (string_of_int(!Graph.mouseX) ^ " " ^ string_of_int(!Graph.mouseY)) 0 80;
  Graph.text (string_of_int(fst (Graphics.mouse_pos ())) ^ " " ^ string_of_int(snd (Graphics.mouse_pos ()))) 0 120;
  (*Graph.rect (!Graph.mouseX -20) (!Graph.mouseY-20) (20) (20);*)
  if Graphics.key_pressed () then
    (match Graphics.read_key () with
      | _ -> if !gameMode = makeEndMode then
              if !gameLeft <> 0 then (initNewGame ())
              else (choiceScr := -1)
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

let screenStart () =
  let choiceScr = ref 0 in

  !choiceScr
;;

initNewGame ();;

let screenList = [screenStart; screenGame];;

Graph.draw screenList 1;;

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