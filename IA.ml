(** Algorithmes de recherche de code *)
module IA :
  sig
  (** Nombre d ' algorithmes developpes *)
  val nombre_methodes : int

  val preCalculatedValueForKnuth : Code.t list
  (** Choisit un code a proposer
    * @param methode 0 pour l ' algorithme naif,
    * 1 pour l ' algorithme de KNUTH
    * ... et ainsi de suite
    *@param essais la liste des codes deja proposes
    * @param possibles la liste des codes possibles
    * @return le prochain code a essayer
    *)

  val choix : int -> Code.t list -> Code.t list -> Code.t

  (** filtre les codes possibles
    * @param methode 0 pour l ' algorithme naif,
    * 1 pour l ' algorithme de KNUTH
    * ... et ainsi de suite
    * @param (code, rep) le code essaye et la reponse correspondante
    * @param possibles la liste de courante de codes possibles
    * @param la nouvelle liste de codes possibles
    *)
    val filtre : int -> (Code.t * (int * int) option) -> Code.t list -> Code.t list

    end =
    struct
    let preCalculatedValueForKnuth = [[Code.Color(0); Code.Color(1)]; [Code.Color(0); Code.Color(1); Code.Color(2)]; [Code.Color(0);Code.Color(0);Code.Color(1);Code.Color(1)]];;
    let nombre_methodes = 2;; (* de 0 a 1*)

    let algoNaif listEssaiPossible =
      match listEssaiPossible with
      | x::l -> x
      | _ -> failwith "ERROR AT algoNaif | listEssaiPossible Empty" (* L'utilisateur c'est tromper dans lors d'une reponse *)
    ;;

    let max a b =
      if a > b then a else b
    ;;

    let min a b =
      if a < b then a else b
    ;;

    let algoKnuth listEssaiPropose listEssaiPossible =
      if List.length listEssaiPropose = 0 && Code.nombre_pions-2 < List.length preCalculatedValueForKnuth then List.nth preCalculatedValueForKnuth (Code.nombre_pions-2) else
      if List.length listEssaiPossible = 1 then List.nth listEssaiPossible 0 else
      let reponseScore =
        let rec makeNtab n l = if n > 0 then makeNtab (n-1) (0::l) else l
        in makeNtab (List.length Code.toutes_reponses) []
      in (*printList reponseScore;*)
      let listPoidCandidat = List.fold_left (fun acc c -> acc@[List.fold_left (fun maxValue x -> max maxValue x) 0 (List.fold_left (fun acc_ c_ -> match (Code.reponse c_ c) with
                                                                                                                                                    | Some(x) -> fst (List.fold_left (fun acc__ c__ -> let i = snd acc__ in
                                                                                                                                                                                                       if ((List.nth Code.toutes_reponses i) = x) then ((fst acc__)@[c__+1], i+1)
                                                                                                                                                                                                       else ((fst acc__)@[c__], i+1)
                                                                                                                                                                                    ) ([],0) acc_)
                                                                                                                                                    | _-> failwith "ERROR AT algoKnuth | listWithoutPropose and listEssaiPossible don't have code of the same length"
                                                                                                                                    ) reponseScore listEssaiPossible)]
                                              ) [] Code.tous in (*print_endline (string_of_int (List.length listPoidCandidat));*)
      List.nth Code.tous (snd (fst (List.fold_left (fun acc c -> let i = snd acc in
                                                                  if i = 0 then ((c, i), i+1) else
                                                                  let value = fst (fst acc) in
                                                                  if c < value then ((c, i),i+1) else (fst acc, i+1)
                                                    ) ((-1, -1), 0) listPoidCandidat)))
    ;;

    let choix ch listEssaiPropose listEssaiPossible =
      match ch with
      | 0 -> algoNaif listEssaiPossible
      | 1 -> algoKnuth listEssaiPropose listEssaiPossible
      | _ -> algoNaif listEssaiPossible
    ;;

    let eraseCodeWithoutN l code n = (* Efface toute les combinaison n'ayant pas n couleur dans code *)
      List.fold_left (fun acc c -> if List.length (List.fold_left (fun acc_ c_ -> let indexAlreadyTaken = acc_ in
                                                                                  let ind = fst (List.fold_left (fun acc__ c__ -> let i = snd acc__ in
                                                                                                                                  let v = fst acc__ in
                                                                                                                                  if v = -1 && c__ = c_ && List.for_all (fun x -> i<>x) indexAlreadyTaken then (i, i+1)
                                                                                                                                  else (v, i+1)
                                                                                                                ) (-1, 0) code) in
                                                                                  if ind <> -1 then acc_@[ind]
                                                                                  else acc_
                                                                       ) [] c) = n then acc@[c]
                                   else acc
                      ) [] l
    ;;

    let filtreNaif reponse listEssaiPossible =
      (*Code.printListCode listEssaiPossible;*)
      match listEssaiPossible with
      | code::l -> match (snd reponse) with
                  | Some(x) -> eraseCodeWithoutN l (fst reponse) (fst x + snd x)
                  | None -> listEssaiPossible
      | _ -> listEssaiPossible
    ;;

    let filtreKnuth reponse listEssaiPossible =
        List.filter (fun x -> (Code.reponse x (fst(reponse))) = (snd reponse)
                     ) listEssaiPossible
    ;;

    let filtre ch reponse listEssaiPossible =
      match ch with
      | 0 -> filtreNaif reponse listEssaiPossible
      | 1 -> filtreKnuth reponse listEssaiPossible
      | _ -> filtreNaif reponse listEssaiPossible
    ;;
end;;


(*
let rec funct code prop l = Code.printListCode l;
  let o = IA.choix 1 prop l in print_endline (Code.string_of_code o);
  let rep = Code.reponse o code in
  if o = code then "end" else funct code (prop@[o]) (IA.filtre 1 (o, rep) l)
;;

funct (Code.makeCode "bafe") [] Code.tous;;*)

(*
print_endline (Code.string_of_code (IA.choix 1 Code.tous (Code.tous)));;
Code.printListCode (IA.filtre 1 (Code.makeCode "aab", Code.reponse (Code.makeCode "aab") (Code.makeCode "bac")) (Code.tous));;
print_endline (Code.string_of_code (IA.choix 1 [] (IA.filtre 1 (Code.makeCode "aab", Code.reponse (Code.makeCode "aab") (Code.makeCode "bac")) (Code.tous))));;*)
