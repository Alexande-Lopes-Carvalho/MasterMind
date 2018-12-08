(** Module de definition d ' un code dans le jeu Mastermind *)
module Code :
  sig
    (** Le type d ' un pion *)
    type pion = Color of int
    (** Le type d ' un code *)
    type t = pion list
    (** Nombre de pions par code *)
    val nombre_pions : int
    (* motif pour affichage console de pion bien placé *)

    val reponseBonneMotif  : string
    (* motif pour affichage console de pion mal placé *)

    val reponseMauvaiseMotif : string
    (** Liste des couleurs possibles *)

    val couleurs_possibles : pion list

    (** Compare deux codes
      * @param code1 premier code a comparer
      * @param code2 second code a comparer
      * @return 0 si les deux codes sont identiques,
                un entier positif si [code1] est strictement plus grand que [code2]
                un entier negatif si [code1] est strictement plus petit que [code2]
      *)

    val compare : t -> t -> int

    (** Conversion pion vers chaine de caracteres (pour affichage)
      * @param a pion a convertir
      * @return la representation en chaine de caracteres de [code]
      *)

    val string_of_pion : pion -> string

    (** Conversion code vers chaine de caracteres (pour affichage)
      * @param code code a convertir
      * @return la representation en chaine de caracteres de [code]
      *)

    val string_of_code : t -> string

    (** Conversion chaine de caracteres vers code (pour saisie)
      * @param string chaine de caractere saisie
      * @return le code correspondant a la saisie si la conversion est possible
                [None] si la conversion n ' est pas possible
      *)

    val code_of_string : string -> t option

    (** Conversion de reponse vers string pour affichage
      * @param reponse
      * @return sa representation en chaine de caractere
      *)

    val string_of_reponse : (int * int) option -> string

    (* a supp pour debug *)
    val makeCode : string -> t

    (** La liste de tous les codes permis *)
    val tous : t list

    (** La liste de toutes les reponses possibles *)
    val toutes_reponses : (int * int) list ;;


    (** Calcule la reponse d ' un code par rapport au code cache
      * @param code le code propose
      * @param vrai_code le code cache
      * @return un couple (nombre de pions bien places, nombre de pions mal places)
        [None] si la reponse ne peut etre calculee
      *)
    val reponse : t -> t -> (int * int) option

    end =
    struct
      let min a b = if a > b then b else a;;
      let max a b = if a > b then a else b;;

      type pion = Color of int;;
      type t = pion list;;
      let pionToken = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'];;
      let pionColor = ["[0;31m"; "[0;32m"; "[0;33m"; "[0;34m"; "[0;35m"; "[0;36m"; "[0;37m"; "[1;31m"; "[1;32m"; "[1;33m"; "[1;34m"; "[1;35m"; "[1;36m"; "[1;37m"];;
      let defaultColor = "[0m";;
      let reponseBonneMotif = "/";;
      let reponseMauvaiseMotif = ".";;

      let rec makeArray n l =
        if n > 0 then makeArray (n-1) ((Color(n-1))::l)
        else l
      ;;

      let makeArray n =
        makeArray n []
      ;;

      let makecolorList x = makeArray (min (max x 0) (List.length pionColor));;
      let makeNbPions x = max x 1;;

      let nombre_pions = makeNbPions 4;;
      let couleurs_possibles = makecolorList 6;;

      let extractColor a = match a with | Color(x) -> x;; 

      let rec compare a b = 
        fst (List.fold_left (fun acc c -> let i = snd acc in
                                     ( fst acc + (( (extractColor c) - (extractColor (List.nth b i)) ) * (int_of_float ( float_of_int(List.length couleurs_possibles) ** float_of_int (nombre_pions-1-i))) ) , i+1)
                        ) (0, 0) a)
      ;;

      let fill s = "\027" ^ s;;

      let string_of_pion a =
        let v = extractColor a in
        (fill (List.nth pionColor v) (*^ " "*)) ^ (String.make 1 (List.nth pionToken v))
      ;;

      let string_of_code t =
          (List.fold_left (fun acc c -> acc ^ (string_of_pion c)) "" t) ^ (fill defaultColor)
        ;;

      let rec charlist_of_string_rec s i l =
        if i >= 0 then charlist_of_string_rec s (i-1) (String.get s i::l)
        else l
      ;;

      let charlist_of_string s =
        charlist_of_string_rec s ((String.length s)-1) []
      ;;

      let find l x = fst (List.fold_left (fun acc c -> let i = snd acc in
                                                  if c = x then (i, i+1)
                                                  else (fst acc, i+1)
                                     ) (-1, 0) l);;

      let code_of_string s =
          if (String.length s = nombre_pions) then 
            List.fold_left (fun acc c -> match acc with 
                                          | Some(x) -> let res = find pionToken c in
                                                       if res <> -1 && res < (List.length couleurs_possibles) then Some(x@[Color(res)]) else None
                                          | None -> None 
                            ) (Some([])) (charlist_of_string s)
          else None
        ;;

      let rec build x s motif =
        if x > 0 then build (x-1) (s^motif) motif
        else s
      ;;

      let string_of_reponse x =
        match x with
        | None -> ""
        | Some(y) -> (build (fst(y)) "" reponseBonneMotif) ^ (build (snd(y)) "" reponseMauvaiseMotif)
      ;;

      let makeCode s = (* Pour Debug*)
        match code_of_string s with 
        | Some(x) -> x
        | None -> [Color(0)]
      ;;

      let tous =
        let o = couleurs_possibles in
        let rec f x nb =
          if nb = 1 then List.fold_left (fun acc c -> acc@[[c]]) [] x
          else List.fold_left (fun acc c -> acc@(List.fold_left (fun acc_ c_ -> acc_@[c::c_]) [] (f x (nb-1)))) [] x
        in f o nombre_pions;;

      let toutes_reponses =
        let rec makeArray n l =
          if n > 0 then makeArray (n-1) ((n-1)::l)
          else l
        in let o = makeArray (nombre_pions+1) [] in
        List.fold_left (fun acc c -> acc@(List.fold_left (fun acc_ c_ -> if c+c_ <= nombre_pions then acc_@[(c, c_)] else acc_ 
                                                          ) [] o)
                        ) [] o
      ;;

      (*REMOVE DOWN printList
      let printList l =
        print_endline ((List.fold_left (fun acc c -> acc ^ (string_of_int c) ^ " ") " [" l) ^ "] ");
      ;;*)
      let setList l is x = fst (List.fold_left (fun acc c -> let i = snd acc in if is = i then ((fst acc)@[x] , i+1) else ((fst acc)@[c] , i+1) ) ([], 0) l);; 

      let reponse a b =
        if List.length a = List.length b then
           let checkCorrectPion = 
            fst (List.fold_left (fun acc c -> let i = snd acc in ((fst acc)@[if List.nth b i = c then i else -1] , (snd acc) +1)) ([], 0) a)
           in 
           let checkPion = 
            fst (List.fold_left (fun acc c ->  let i = (snd acc) in 
                                          if (List.nth (fst acc) i = -1) then 
                                            let res = fst (List.fold_left (fun acc_ c_ -> let i = (snd acc_) in
                                                                                          if c_ = c && not(List.exists (fun x -> x = i) (fst acc)) then (i , i+1) 
                                                                                          else (fst acc_, i+1)
                                                                            ) (-1, 0) b) in
                                            if res <> -1 then (setList (fst acc) i res, i+1)
                                            else (fst acc, i+1)
                                         else (fst acc, i+1)
                            ) (checkCorrectPion, 0) a)
           in 
           let result = 
            fst (List.fold_left (fun acc c -> let i = (snd acc) in
                                         if i = c then (( (fst (fst acc)) +1 , (snd (fst acc)) ) , i+1) 
                                         else if c <> -1 then (( (fst (fst acc))  , (snd (fst acc)) +1) , i+1)
                                         else  (fst acc , i+1)
                            ) ((0, 0), 0) checkPion)
           in Some(result)
        else None
      ;;

      (*
      let reponse a b =
        if List.length a = List.length b then
           let rec findExceptIndex c l li i =
            match l with
            | [] -> None
            | x::k -> if x = c && (List.fold_left (fun acc c -> c<>i &&acc) true li) then Some(i) else findExceptIndex c k li (i+1)
           in
           let value = List.length (List.fold_left (fun acc c -> (*printList acc;*)match findExceptIndex c b acc 0 with
                                                                  | None -> acc
                                                                  | Some(x) -> x::acc) [] a) in
          Some(value, nombre_pions-value)
        else None
      ;;*)

end ;;


let optionString a =
  match a with
  | None -> ""
  | Some(x) -> "(" ^ (string_of_int (fst x)) ^ ", " ^ (string_of_int (snd x)) ^ ") " ^ (Code.string_of_reponse a)
;;

print_endline (Code.string_of_code (Code.makeCode "abcd"));;
print_endline (Code.string_of_code (Code.makeCode "abc"));;
print_endline (Code.string_of_code (Code.makeCode "abhh"));;
print_endline (Code.string_of_code (Code.makeCode "dcba"));;

print_endline (optionString (Code.reponse (Code.makeCode "abcd") (Code.makeCode "afff")));;
print_endline (optionString (Code.reponse (Code.makeCode "abcd") (Code.makeCode "afff")));;
print_endline (optionString (Code.reponse (Code.makeCode "abcd") (Code.makeCode "abcf")));;
print_endline (optionString (Code.reponse (Code.makeCode "abcd") (Code.makeCode "abcd")));;
print_endline (optionString (Code.reponse (Code.makeCode "ffaf") (Code.makeCode "ffaf")));;
print_endline (optionString (Code.reponse (Code.makeCode "ffaf") (Code.makeCode "cffa")));;
print_endline (optionString (Code.reponse (Code.makeCode "abcd") (Code.makeCode "bcda")));;
print_endline (optionString (Code.reponse (Code.makeCode "aabb") (Code.makeCode "baab")));;

print_endline ("Compare " ^ (string_of_int (Code.compare (Code.makeCode"abcd") (Code.makeCode"abcd") )));;
print_endline ("Compare " ^ (string_of_int (Code.compare (Code.makeCode"abaf") (Code.makeCode"abba") )));;
print_endline ("Compare " ^ (string_of_int (Code.compare (Code.makeCode"bbaf") (Code.makeCode"abaf") )));;
print_endline ("Compare " ^ (string_of_int (Code.compare (Code.makeCode"afff") (Code.makeCode"baaa") )));;
print_endline ("Compare " ^ (string_of_int (Code.compare (Code.makeCode"aaba") (Code.makeCode"aaaa") )));;

(*
let reponse a b =
    if (List.length a = List.length b) then
    let o = let rec makeRep a b i =
      match (a , b) with
      | ([], []) -> i
      | (x::l, y::k) -> makeRep l k ((if x = y then 1 else 0) + i)
      in makeRep a b 0 in
    Some((o, nombre_pions-o))
  else None
;;
*)

(*
match (find c pionToken) with
                                        | Some(x) -> Some(accx @ [Color(x)])
                                        | None -> None
*)

(*
let tous =
let o = makeArray (List.length couleurs_possibles) in
let rec f x nb =
  if nb = 1 then List.fold_left (fun acc c -> [c]::acc) [] x
  else List.fold_left (fun acc c -> acc@(List.fold_left (fun acc_ c_ -> (c::c_)::acc_) [] (f x (nb-1)))) [] x
in f o nombre_pions;;
*)
  (*List.fold_left (fun acc c -> if nb = 0 then c::acc
                                else (List.fold_left (fun acc_ c_ -> (c::c_)::acc_) [] (f x (nb-1)))::acc  ) [] x*)

                                      (*let rec find_rec s l i =
        match l with
        | [] -> None
        | x:: k -> if x=s then Some(i) else find_rec s k (i+1)
        ;;

      let find s l =
        find_rec s l 0
      ;;

      let rec charlist_of_string_rec s i l =
        if i >= 0 then charlist_of_string_rec s (i-1) (String.get s i::l)
        else l
      ;;

      let charlist_of_string s =
        charlist_of_string_rec s ((String.length s)-1) []
      ;;

      let code_of_string s =
        if String.length s = nombre_pions then
          List.fold_left
          (fun acc c -> match acc with
                        | Some(accx) -> match (find c pionToken) with
                                     | Some(x) -> Some(accx @ [Color(x)])
                                     | None -> None
                        | None -> None) (Some([])) (charlist_of_string s)
        else None
      ;;*)