#use "Code.ml";;
module Game :
  sig

    (*val mastermind : string -> int -> int -> bool -> unit*)
    
    val printGameState : string -> unit

    val upString : string
    (*val round_PlayerGuess : unit -> unit*)
    (*val round_IAGuess : unit -> unit*)
  end =
  struct
    let upString = 
      let rec build x s motif = 
        if x > 0 then build (x-1) (s^motif) motif
        else s in build (Code.nombre_pions) "" "_"
    ;;

    let printGameState l =
      print_endline (upString ^ l);
    ;;

    let newLineGameState code rep = 
      "\n" ^ (Code.string_of_code (code)) ^ " " ^ (Code.string_of_reponse (rep))
    ;;

    let rec refresh chooseCode gameStateln tryLeft codeSearched = (* chooseCode : procedure qui cherchera le code *) (* gameState : string *) (* tryLeft : essai restant*) (* codeSearched : le code recherché *)
      printGameState gameStateln;
      match chooseCode () with 
      | None -> refresh chooseCode gameStateln tryLeft codeSearched (* Invalid code *)
      | Some(x) -> let output = (* a changer par fct car pvp*)Code.reponse x codeSearched in 
          match output with
          | None -> refresh chooseCode gameStateln tryLeft codeSearched (* Invalid code *)
          | Some(c) -> if fst c = Code.nombre_pions && Code.compare x codeSearched = 0 then true
                       else if tryLeft = 1 then false 
                       else refresh chooseCode (gameStateln ^ (newLineGameState x output)) (tryLeft-1) codeSearched
    ;;

    (*
    let gamePlayerGuessIA = 
      let code = List.nth (Code.tous) (Random.int (List.length Code.tous)) in code

    ;;*)
    
    (*
    let gameIAGuessPlayer = 
      
    ;;*)

    (*let mastermind name maxtry nbround assist = (* assist = true : reponse calculé par IA | assist = false*)

    ;;*)
end;;

Game.round_PlayerGuess ();;
Game.printGameState ["abcd //.."; "aaaa /..."];;
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