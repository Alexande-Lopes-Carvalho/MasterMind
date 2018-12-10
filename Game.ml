#use "Code.ml";;
#use "IA.ml";;
module Game :
  sig

    val nameIA : string
    (* mastermind ne contient pas les arguement demandé par le pdf car c'est la procedure elle meme qui demandera a l'utilisateur les argument*)
    val mastermind : unit -> unit

    val printRoundState : string -> unit

    (*val newLineGameState : Code.t -> (int * int) option -> string *)

    val upString : string

    val roundPlayerGuessIA : string -> int -> bool (* true si player won *)
    val roundIAGuessPlayer : int -> int -> string -> int -> bool (* true si IA won *)
    val roundPlayerGuessPlayer : int -> string -> string -> int -> bool (* true si PlayerA won *)

    val outputPlayerFun : string -> Code.t -> Code.t -> ((int * int) option)* bool

    (*val roundPlayerGuessPlayer : string -> string -> int -> bool*)
    (*val round_PlayerGuess : unit -> unit*)
    (*val round_IAGuess : unit -> unit*)
  end =
  struct
    let nameIA = "IA";;

    let upString =
      let rec build x s motif =
        if x > 0 then build (x-1) (s^motif) motif
        else s in "Token Possible : " ^ (List.fold_left (fun acc c -> acc ^ (if (String.length acc = 0)then "" else " | ") ^ (Code.string_of_pion c)
                                                        ) "" Code.couleurs_possibles) ^ "\nWell Placed : " ^ Code.wellplacedMotif ^ "\nMisplaced   : " ^ Code.misplacedMotif ^ "\n" ^ (build (Code.nombre_pions) "" "_")
    ;;

    let printRoundState l =
      Sys.command "clear";
      print_endline l;
    ;;

    let actRoundStateCode code =
      "\n" ^ (Code.string_of_code (code))
    ;;

    let actRoundStateAnswer rep =
      " " ^ (Code.string_of_reponse (rep))
    ;;

    let userfiltrage x l = (* l'utilisateur na pas besoin de filtrage il retourne donc la list*)
      l
    ;;

    let rec userChooseCode name =
      Sys.command "clear";
      print_endline (name ^ " please choose the secret code : ");
      match Code.code_of_string (read_line ()) with
      | None -> userChooseCode name
      | Some(x) -> x
    ;;

    let userChooseCode_Round name listEssaiPropose listEssaiPossible =
      print_endline (name ^ " please enter a Code");
      Code.code_of_string (read_line ())
    ;;

    let outputIAFun x c = (Code.reponse x c, true);;

    let outputPlayerFun player x c =
      let rec out () = print_endline ("please " ^ player ^ " enter the answer according to your code");
        let s = read_line () in
        let res = Code.reponse_of_string s in
        if res = None then out() else res
      in let outRep = out() in (outRep, (Code.reponse x c) = outRep);;

    let iaChooseCodeMakeFun x = IA.choix x;;

    let returnRound roundStateln x =printRoundState roundStateln; x;;

    (* chooseCode : function qui cherchera le code *)
    (* roundState : string *)
    (* tryLeft : essai restant*)
    (* codeSearched : le code recherché *)
    (* outputFun : function qui donnera la reponse du code vis a vis du code recherché *)
    (* filtrage  : (Code.t * (int * int) option) -> Code.t list -> Code.t list *)
    (* listEssaiPropose : ... *)
    (* listEssaiPossible : ...*)
    (* correctAnswer : la fonction pour calculer l'output a toujours repondu correctement *)

    let rec refreshRound chooseCode roundStateln tryLeft codeSearched outputFun filtrage listEssaiPropose listEssaiPossible correctAnswer =
    printRoundState roundStateln;
    match chooseCode listEssaiPropose listEssaiPossible with
    | None -> refreshRound chooseCode roundStateln tryLeft codeSearched outputFun filtrage listEssaiPropose listEssaiPossible correctAnswer(* Invalid code | si l'utilisateur se trompe on refesh l'affichage d'ou la necessité de la rep de type option *)
    | Some(x) -> let newroundStateln = roundStateln ^ actRoundStateCode x in printRoundState newroundStateln;
      let output = outputFun x codeSearched(* a changer par fct car pvp Code.reponse x codeSearched*) in
        match fst output with
        | None -> refreshRound chooseCode roundStateln tryLeft codeSearched outputFun filtrage listEssaiPropose listEssaiPossible correctAnswer(* Invalid code *)
        | Some(c) -> if fst c = Code.nombre_pions (*&& Code.compare x codeSearched = 0*) then returnRound (newroundStateln ^ (actRoundStateAnswer (fst(output)))) (true, correctAnswer && snd (output))
                     else if tryLeft = 1 then returnRound (newroundStateln ^ (actRoundStateAnswer (fst(output)))) (false, correctAnswer && snd (output))
                     else refreshRound chooseCode (newroundStateln ^ (actRoundStateAnswer (fst(output)))) (tryLeft-1) codeSearched outputFun filtrage (listEssaiPropose@[x]) (filtrage (x, fst output) listEssaiPossible) (correctAnswer && snd (output))
    ;;
    
    (* return true si playerA a gagné*)
    let endRound result playerA playerB code =
      print_endline ("The code was " ^ (Code.string_of_code code) ^ "\n" ^ (if not(snd result) then (playerB^" haven't answered correctly \n") else "") ^ (if (not(fst result) && (snd result)) then playerB else playerA) ^ " won the round \npress Enter to continue ...");
      read_line();
      not (not(fst result) && (snd result))
    ;;

    (* return true si playerA a gagné *)
    (* playerA : joueur | playerB : IA *)
    let roundPlayerGuessIA playerA nbTentative =
      let playerB = nameIA in
      let code = List.nth (Code.tous) (Random.int (List.length Code.tous)) in
      let result = refreshRound (userChooseCode_Round playerA) upString nbTentative code outputIAFun userfiltrage [] Code.tous true in
      endRound result playerA playerB code
    ;;

    (* return true si playerA a gagné *)
    (* playerA : IA | playerB : joueur*)
    let roundIAGuessPlayer chIA chRep playerB nbTentative =
      let playerA = nameIA in
      let code = userChooseCode playerB in
      let result = refreshRound (fun a b -> Some(IA.choix chIA a b)) upString nbTentative code (if chRep = 1 then outputPlayerFun playerB else outputIAFun) (IA.filtre chIA) [] Code.tous true in
      endRound result playerA playerB code
    ;;

    (* return true si playerA a gagné *)
    let roundPlayerGuessPlayer chRep playerA playerB nbTentative =
      let code = userChooseCode playerB in
      let result = refreshRound (userChooseCode_Round playerA) upString nbTentative code (if chRep = 1 then outputPlayerFun playerB else outputIAFun) userfiltrage [] Code.tous true in
      endRound result playerA playerB code
    ;;

    let rec askIntBorne message a b =
      Sys.command "clear";
      print_endline message;
      let k = read_int () in
      if k >= a && k <= b then k else askIntBorne message a b 
    ;;

    let rec askIntSup message a =
      Sys.command "clear";
      print_endline message;
      let k = read_int () in
      if k >= a then k else askIntSup message a 
    ;;

    let rec askString message l =
      print_endline message;
      let res = read_line () in 
      let i = fst (List.fold_left (fun acc c -> let i = snd acc in
                                           if String.compare res c = 0 then (i, i+1)
                                           else (fst acc, i+1)
                                   ) (-1, 0) l) in
      if i = -1 then askString message l
      else i
    ;; 

    let askPlayerInfo message = 
      print_endline (message ^ " Please write your name");
      let name = read_line () in
      (name, askString (name ^ " do you want an IA assist ?\nyes / no") ["yes"; "no"]) (* pour que l'IA calculer les reponse ou que ce soit lui qui reponde*)
    ;;

    let endGame res playerA playerB = 
      Sys.command "clear";
      print_endline ( (if (fst res <> snd res) then ((if (fst res > snd res) then playerA else playerB) ^ " won the game") else "Equality") ^ "\n" ^ playerA ^ " won " ^ (string_of_int (fst res)) ^ " round \n" ^ playerB ^ " won " ^ (string_of_int(snd res)) ^ " round\npress Enter to continue ...");
    ;; 

    (* funA return true si PlayerA won 
       funB return true si PlayerB won*)
    let rec game funA funB nbTentative latch roundLeft result = 
      if roundLeft = 0 then result 
      else if latch then let res = funA nbTentative in game funA funB nbTentative (not latch) (roundLeft-1) (fst result + (if res then 1 else 0), snd result + (if res then 0 else 1))
      else let res = funB nbTentative in game funA funB nbTentative (not latch) (roundLeft-1) (fst result + (if res then 0 else 1), snd result + (if res then 1 else 0))
    ;;

    let rec mastermind () =
      let gameMode = askIntBorne ("MasterMind \nGame mode : \n0 : Player Vs IA\n1 : Player Vs Player\nplease Choose a Game mode") 0 1 in
      let nbGame = let v = askIntSup ("MasterMind \nHow much Game would you like ?") 1 in (v+v mod 2) in
      let nbTry = askIntSup ("MasterMind \nHow much try per Game would you like ?") 1 in
      let playerA = askPlayerInfo "Player 1" in
      if gameMode = 0 then (* Player Vs IA *) 
        let chIA = askIntBorne ("Please choose an IA\n0 : Algo Naif\n1 : Algo Knuth") 0 1 in
        let result = game (roundPlayerGuessIA (fst playerA)) (roundIAGuessPlayer (chIA) (snd playerA) (fst playerA)) nbTry (Random.bool ()) nbGame (0, 0) in
        endGame result (fst playerA) nameIA;
        let relaunchGame = askString ("Would you like to play another game?\nyes / no") ["yes"; "no"] in 
        if relaunchGame = 0 then mastermind ();
      else if gameMode = 1 then 
        let playerB = askPlayerInfo "Player 2" in 
        let result = game (roundPlayerGuessPlayer (snd playerB) (fst playerA) (fst playerB)) (roundPlayerGuessPlayer (snd playerA) (fst playerB) (fst playerA)) nbTry (Random.bool ()) nbGame (0, 0) in
        endGame result (fst playerA) (fst playerB);
        let relaunchGame = askString ("Would you like to play another game?\nyes / no") ["yes"; "no"] in 
        if relaunchGame = 0 then mastermind ();
    ;;
end;;

(*Game.roundPlayerGuessPlayer "you" "me" 10;;*)
Game.mastermind ();;
(*
read_line ();;
Game.roundPlayerGuessPlayer 1 "A" "B" 10;;
read_line ();;
Game.roundIAGuessPlayer 1 0 "you" 10;;
read_line ();;
Game.roundPlayerGuessIA "you" 10;;*)

(*
Game.round_PlayerGuess ();;
Game.printGameState ["abcd //.."; "aaaa /..."];;*)
      (*
    let round_PlayerGuess () =
      let codeToGuess = ref (List.nth Code.tous (Random.int (List.length Code.tous))) in
      let isFinished = ref true in
      let listString = ref [] in
      while !isFinished do
        printGameState !listString;
        listString := listString@[read_line ()];
        if List.length !listString > 4 then
          isFinished := false;
      done
      print_endline (Code.string_of_code );;*)

(*
match chooseCode listEssaiPropose listEssaiPossible with
| None -> refresh chooseCode roundStateln tryLeft codeSearched outputFun listEssaiPropose listEssaiPossible(* Invalid code *)
| Some(x) -> let output = outputFun x codeSearched(* a changer par fct car pvp Code.reponse x codeSearched*) in
    match output with
    | None -> refresh chooseCode roundStateln tryLeft codeSearched outputFun listEssaiPropose listEssaiPossible(* Invalid code *)
    | Some(c) -> if fst c = Code.nombre_pions && Code.compare x codeSearched = 0 then (true, (roundStateln ^ (newLineGameState x output)))
                 else if tryLeft = 1 then (false, (roundStateln ^ (newLineGameState x output)))
                 else refresh chooseCode (roundStateln ^ (newLineGameState x output)) (tryLeft-1) codeSearched outputFun listEssaiPropose listEssaiPossible
*)
