(** Algorithmes de recherche de code *)
module IA :
  sig
  (** Nombre d ' algorithmes developpes *)
  val nombre_methodes : int

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
    let nombre_methodes = 2;; (* de 0 a 1*)

    let algoNaif listEssaiPossible =
      match listEssaiPossible with
      | x::l -> x
      | _ -> failwith "ERROR AT algoNaif | listEssaiPossible Empty"
    ;;
    
    let choix ch listEssaiPropose listEssaiPossible =
      match ch with
      | 0 -> algoNaif listEssaiPossible
      | _ -> algoNaif listEssaiPossible
    ;;

    let eraseUnrelatedStrict l code = (* Supprime les code n'ayant pas strictment les meme couleurs *)
       List.fold_left (fun acc c -> if List.length ((List.fold_left (fun acc_ c_ -> (*printList acc_;*) fst (List.fold_left (fun acc__ c__ -> let i = snd acc__ in
                                                                                                                                              if i then ((fst acc__)@[c__], i) 
                                                                                                                                              else if c__ <> c_ then ((fst acc__)@[c__], i) 
                                                                                                                                              else (fst acc__, true)
                                                                                                                             ) ([], false) acc_)
                                                                     ) c code)) = 0 then acc@[c] else acc  
                        ) [] l
    ;;

    let eraseRelatedStrict l code = (* Supprime les code ayant strictment les meme couleurs *)
      List.fold_left (fun acc c -> if List.length ((List.fold_left (fun acc_ c_ -> (*printList acc_;*) fst (List.fold_left (fun acc__ c__ -> let i = snd acc__ in
                                                                                                                                             if i then ((fst acc__)@[c__], i) 
                                                                                                                                             else if c__ <> c_ then ((fst acc__)@[c__], i) 
                                                                                                                                             else (fst acc__, true)
                                                                                                                            ) ([], false) acc_)
                                                                     ) c code)) <> 0 then acc@[c] else acc  
                      ) [] l
    ;;

    let eraseUnrelated l code = (* Supprime les code ayant aucune couleur dans le code *)
    List.fold_left (fun acc c -> if (List.fold_left (fun acc_ c_ -> acc_ || List.fold_left (fun acc__ c__ -> acc__ || c__ = c_ 
                                                                                            ) false c 
                                                     ) false code) then acc@[c]
                                 else acc
                    ) [] l
    ;;

    let eraseRelated l code = (* Supprime les code ayant une couleur dans le code *)
      List.fold_left (fun acc c -> if not(List.fold_left (fun acc_ c_ -> acc_ || List.fold_left (fun acc__ c__ -> acc__ || c__ = c_ 
                                                                                              ) false c 
                                                       ) false code) then acc@[c]
                                   else acc
                      ) [] l
      ;;

    let filtreNaif reponse listEssaiPossible =
      Code.printListCode listEssaiPossible;
      match listEssaiPossible with 
      | code::l -> match (snd reponse) with 
                  | Some(x) -> if (fst x + snd x = Code.nombre_pions) then eraseUnrelatedStrict l (fst reponse)
                               else if (fst x + snd x <> 0) then eraseRelatedStrict (eraseUnrelated l (fst reponse)) (fst reponse)
                               else eraseRelated l (fst reponse)
                  | None -> listEssaiPossible
      | _ -> listEssaiPossible
    ;;

    let filtre ch reponse listEssaiPossible =
      match ch with
      | 0 -> filtreNaif reponse listEssaiPossible
      | _ -> filtreNaif reponse listEssaiPossible
    ;;
end;;
