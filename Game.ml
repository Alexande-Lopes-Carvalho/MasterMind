#use "Code.ml";;
#use "IA.ml";;
module Game :
  sig

    (*val mastermind : string -> int -> int -> bool -> unit*)

    val printRoundState : string -> unit

    val newLineGameState : Code.t -> (int * int) option -> string (**)

    val upString : string

    val roundPlayerGuessIA : string -> string -> int -> bool
    val roundIAGuessPlayer : int -> string -> string -> int -> bool
    (*val roundPlayerGuessPlayer : string -> string -> int -> bool*)
    (*val round_PlayerGuess : unit -> unit*)
    (*val round_IAGuess : unit -> unit*)
  end =
  struct

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

    let newLineGameState code rep =
      "\n" ^ (Code.string_of_code (code)) ^ " " ^ (Code.string_of_reponse (rep))
    ;;

    let userfiltrage x l = (* l'utilisateur na pas besoin de filtrage il retourne donc la list*)
      l
    ;;

    let rec userChooseCode () =
      Sys.command "clear";
      print_endline "Choose the secret code : ";
      match Code.code_of_string (read_line ()) with
      | None -> userChooseCode ()
      | Some(x) -> x
    ;;

    let userChooseCode_Round listEssaiPropose listEssaiPossible =
      Code.code_of_string (read_line ())
    ;;

    let outputIAFun = Code.reponse;;

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

    let rec refreshRound chooseCode roundStateln tryLeft codeSearched outputFun filtrage listEssaiPropose listEssaiPossible =
    printRoundState roundStateln;
    match chooseCode listEssaiPropose listEssaiPossible with
    | None -> refreshRound chooseCode roundStateln tryLeft codeSearched outputFun filtrage listEssaiPropose listEssaiPossible(* Invalid code | si l'utilisateur se trompe on refesh l'affichage d'ou la necessité de la rep de type option *)
    | Some(x) -> let output = outputFun x codeSearched(* a changer par fct car pvp Code.reponse x codeSearched*) in
        match output with
        | None -> refreshRound chooseCode roundStateln tryLeft codeSearched outputFun filtrage listEssaiPropose listEssaiPossible(* Invalid code *)
        | Some(c) -> if fst c = Code.nombre_pions && Code.compare x codeSearched = 0 then returnRound (roundStateln ^ (newLineGameState x output)) true
                     else if tryLeft = 1 then returnRound (roundStateln ^ (newLineGameState x output)) false
                     else refreshRound chooseCode (roundStateln ^ (newLineGameState x output)) (tryLeft-1) codeSearched outputFun filtrage (listEssaiPropose@[x]) (filtrage (x, output) listEssaiPossible)
    ;;

    let printEndRound result playerA playerB code =
      print_endline ("The code was " ^ (Code.string_of_code code));
      print_endline ((if (result) then playerA else playerB) ^ " won the round ");
    ;;

    let roundPlayerGuessIA playerA playerB nbTentative =
      let code = List.nth (Code.tous) (Random.int (List.length Code.tous)) in
      let result = refreshRound userChooseCode_Round upString nbTentative code outputIAFun userfiltrage [] Code.tous in
      printEndRound result playerA playerB code; result
    ;;

    let roundIAGuessPlayer chIA playerA playerB nbTentative =
      let code = userChooseCode () in 
      let result = refreshRound (fun a b -> Some(IA.choix chIA a b)) upString nbTentative code outputIAFun (IA.filtre chIA) [] Code.tous in
      printEndRound result playerA playerB code; result
    ;;

    (*
    let roundIAGuessPlayer playerA playerB nbTentative =
    ;;*)

    (*
    let roundPlayerGuessPlayer playerA playerB nbTentative =
    ;;*)

    (*
    let rec makeGame_rec playerA playerB nbRound =

    ;;*)

    (*
    let gamePlayerGuessIA =

    ;;*)

    (*
    let gameIAGuessPlayer =

    ;;*)

    (*
    let mastermind name maxtry nbround assist = (* assist = true : reponse calculé par IA | assist = false*)

    ;;*)
end;;

(*Game.roundPlayerGuessPlayer "you" "me" 10;;*)
Game.roundIAGuessPlayer 0 "IA" "you" 100;;
Game.roundPlayerGuessIA "you" "IA" 10;;

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
