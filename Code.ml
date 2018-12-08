(** Module de definition d ' un code dans le jeu Mastermind *)
module Code :
  sig
    (** Le type d ' un pion *)
    type pion = Color of int
    (** Le type d ' un code *)
    type t = pion list
    (** Nombre de pions par code *)
    val nombre_pions : int
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

      let rec makecolorListrec x l r = 
        if x = 0 then r else 
        match l with 
          | [] -> print_endline "Not Enough Color";r (* cas impossible a cause du min max lors de l'appel de la fonction*)
          | c::k -> makecolorListrec (x-1) k (r@[c]) 
      ;;

      let makecolorList x = makecolorListrec (min (max x 0) (List.length pionColor)) pionColor [];;

      let nombre_pions = 4;;
      let couleurs_possibles = makecolorList 6;;
      let defaultColor = "[1m";;

      let rec compare a b = match (a, b)with
        | ([], []) -> 0
        | (Color(x):: l, Color(y) :: k) -> if (x = y) then compare l k
                                           else (x-y)
        | _ -> failwith "ERROR AT compare | a, b HAVE DIFFERENT SIZE";;
      
      let pionValue a = 
        match a with 
        | Color(x) -> x
      ;;

      let fill s = "\027" ^ s;;
      
      let string_of_pion a = 
        let v = pionValue a in 
        (fill (List.nth couleurs_possibles v) (*^ " "*)) ^ (String.make 1 (List.nth pionToken v))
      ;;
      
      let string_of_code t = 
          (List.fold_left (fun acc c -> acc ^ (string_of_pion c)) "" t) ^ (fill defaultColor)
        ;;

      let rec find_rec s l i =
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
      ;;

      let rec makeArray n l =
        if n > 0 then makeArray (n-1) ((n-1)::l)
        else l  
      ;;

      let makeArray n = 
        makeArray n []
      ;;

      let tous = 
        let o = makeArray (List.length couleurs_possibles) in
        let rec f x nb = 
          if nb = 1 then List.fold_left (fun acc c -> acc@[[c]]) [] x
          else List.fold_left (fun acc c -> acc@(List.fold_left (fun acc_ c_ -> acc_@[c::c_]) [] (f x (nb-1)))) [] x
        in f o nombre_pions;;
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
end ;;

(*
match (find c pionToken) with 
                                        | Some(x) -> Some(accx @ [Color(x)])
                                        | None -> None
*)