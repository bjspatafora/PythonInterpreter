(* File: a09q04.ml *)

#use "lexer.mml.ml";;

exception BadToks;;
exception UnmatchedParen;;
exception NotImplemented;;

type expr = Equ of (expr * expr)
          | Neq of (expr * expr)
          | Geq of (expr * expr)
          | Gt of (expr * expr)
          | Leq of (expr * expr)
          | Lt of (expr * expr)
          | Or of (expr * expr)
          | And of (expr * expr)
          | Not of expr
          | Plus of (expr * expr)
          | Minus of (expr * expr)
          | Mult of (expr * expr)
          | Div of (expr * expr)
          | Mod of (expr * expr)
          | Exp of (expr * expr)
          | Neg of expr
          | Paren of expr
          | Int of int
          | Bool of bool
          | Float of float
;;

let vartbl : (string, expr) Hashtbl.t = Hashtbl.create 10;;

let rec bool1 toks =
  let rec bool1loop toks acc =
    (* let () = print_string "Bool1: " in
       let () = print_tokens toks in
       let () = print_newline () in *)
    match toks with
    | Or_tok::rt -> let rtree, rtoks = bool2 rt in
      bool1loop rtoks (Or (acc, rtree))
    | _ -> acc, toks
  in
  let ltree, rtoks = bool2 toks in
  bool1loop rtoks ltree
and
  bool2 toks =
  let rec bool2loop toks acc =
    (* let () = print_string "Bool2: " in
       let () = print_tokens toks in
       let () = print_newline () in *)
    match toks with
    | And_tok::rt -> let rtree, rtoks = bool3 rt in
      bool2loop rtoks (And (acc, rtree))
    | _ -> acc, toks
  in
  let ltree, rtoks = bool3 toks in
  bool2loop rtoks ltree
and
  bool3 toks =
  (* let () = print_string "Bool4: " in
       let () = print_tokens toks in
       let () = print_newline () in *)
  match toks with
  | Not_tok::rt -> let t, rtoks = bool3 rt in Not t, rtoks
  | _ -> bool4 toks
and
  bool4 toks =
  let rec bool4loop toks acc =
    (* let () = print_string "Bool4: " in
    let () = print_tokens toks in
    let () = print_newline () in *)
    match toks with
    | Is_Equ_tok::rt -> let rtree, rtoks = arith1 rt in
      bool4loop rtoks (Equ (acc, rtree))
    | Is_Neq_tok::rt -> let rtree, rtoks = arith1 rt in
      bool4loop rtoks (Neq (acc, rtree))
    | Is_Geq_tok::rt -> let rtree, rtoks = arith1 rt in
      bool4loop rtoks (Geq (acc, rtree))
    | Is_Gt_tok::rt -> let rtree, rtoks = arith1 rt in
      bool4loop rtoks (Gt (acc, rtree))
    | Is_Leq_tok::rt -> let rtree, rtoks = arith1 rt in
      bool4loop rtoks (Leq (acc, rtree))
    | Is_Lt_tok::rt -> let rtree, rtoks = arith1 rt in
      bool4loop rtoks (Lt (acc, rtree))
    | _ -> acc, toks
  in
  let ltree, rtoks = arith1 toks in
  bool4loop rtoks ltree
and
  arith1 toks =
  let rec arith1loop toks acc =
    (* let () = print_string "Arith1: " in
    let () = print_tokens toks in
    let () = print_newline () in *)
    match toks with
    | Plus_tok::rt -> let rtree, rtoks = arith2 rt in
      arith1loop rtoks (Plus (acc, rtree))
    | Minus_tok::rt -> let rtree, rtoks = arith2 rt in
      arith1loop rtoks (Minus (acc, rtree))
    | _ -> acc, toks
  in
  let ltree, rtoks = arith2 toks in
  arith1loop rtoks ltree
and
arith2 toks =
  let rec arith2loop toks acc =
    (* let () = print_string "Arith2: " in
    let () = print_tokens toks in
    let () = print_newline () in *)
    match toks with
    | Mult_tok::rt -> let rtree, rtoks = arith3 rt in
        arith2loop rtoks (Mult (acc, rtree))
    | Div_tok::rt -> let rtree, rtoks = arith3 rt in
        arith2loop rtoks (Div (acc, rtree))
    | Mod_tok::rt -> let rtree, rtoks = arith3 rt in
        arith2loop rtoks (Mod (acc, rtree))
    | _ -> acc, toks
  in
  let ltree, rtoks = arith3 toks in
  arith2loop rtoks ltree
and
arith3 toks =
  (* let () = print_string "Arith3: " in
  let () = print_tokens toks in
  let () = print_newline () in *)
  let ltree, rtoks = atom toks in
  match rtoks with
    | Exp_tok::rt -> let rtree, rtoks = arith3 rt in
        Exp (ltree, rtree), rtoks
    | _ -> ltree, rtoks
and
atom toks =
  (* let () = print_string "Atom: " in
  let () = print_tokens toks in
  let () = print_newline () in *)
  match toks with
    | (Id_tok x)::rt -> (Hashtbl.find vartbl x), rt
    | (Int_tok x)::rt -> Int x, rt
    | (Bool_tok x)::rt -> Bool x, rt
    | (Float_tok x)::rt -> Float x, rt
    | Lparen_tok::rt -> let t, rtoks = bool1 rt in
        begin match rtoks with
          | Rparen_tok::rt -> Paren t, rt
          | _ -> raise UnmatchedParen
        end
    | Neg_tok::rt -> let t, rtoks = atom rt in Neg t, rtoks
    | _ -> raise BadToks
;;

let rec print_parsetree tree ind =
  let indent = String.make (ind * 4) ' ' in
  match tree with
  | Or (lt, rt) -> let () = Printf.printf "%sOr(\n" indent in
    let () = print_parsetree lt (ind + 1) in
    let () = Printf.printf "%s,\n" (String.make ((ind + 1) * 4) ' ') in
    let () = print_parsetree rt (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | And (lt, rt) -> let () = Printf.printf "%sAnd(\n" indent in
    let () = print_parsetree lt (ind + 1) in
    let () = Printf.printf "%s,\n" (String.make ((ind + 1) * 4) ' ') in
    let () = print_parsetree rt (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Not (t) -> let () = Printf.printf "%sNot(\n" indent in
    let () = print_parsetree t (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Equ (lt, rt) -> let () = Printf.printf "%sEqu(\n" indent in
    let () = print_parsetree lt (ind + 1) in
    let () = Printf.printf "%s,\n" (String.make ((ind + 1) * 4) ' ') in
    let () = print_parsetree rt (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Neq (lt, rt) -> let () = Printf.printf "%sNeq(\n" indent in
    let () = print_parsetree lt (ind + 1) in
    let () = Printf.printf "%s,\n" (String.make ((ind + 1) * 4) ' ') in
    let () = print_parsetree rt (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Geq (lt, rt) -> let () = Printf.printf "%sGeq(\n" indent in
    let () = print_parsetree lt (ind + 1) in
    let () = Printf.printf "%s,\n" (String.make ((ind + 1) * 4) ' ') in
    let () = print_parsetree rt (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Gt (lt, rt) -> let () = Printf.printf "%sGt(\n" indent in
    let () = print_parsetree lt (ind + 1) in
    let () = Printf.printf "%s,\n" (String.make ((ind + 1) * 4) ' ') in
    let () = print_parsetree rt (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Leq (lt, rt) -> let () = Printf.printf "%sLeq(\n" indent in
    let () = print_parsetree lt (ind + 1) in
    let () = Printf.printf "%s,\n" (String.make ((ind + 1) * 4) ' ') in
    let () = print_parsetree rt (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Lt (lt, rt) -> let () = Printf.printf "%sLt(\n" indent in
    let () = print_parsetree lt (ind + 1) in
    let () = Printf.printf "%s,\n" (String.make ((ind + 1) * 4) ' ') in
    let () = print_parsetree rt (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Plus (lt, rt) -> let () = Printf.printf "%sPlus(\n" indent in
    let () = print_parsetree lt (ind + 1) in
    let () = Printf.printf "%s,\n" (String.make ((ind + 1) * 4) ' ') in
    let () = print_parsetree rt (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Minus (lt, rt) -> let () = Printf.printf "%sMinus(\n" indent in
    let () = print_parsetree lt (ind + 1) in
    let () = Printf.printf "%s,\n" (String.make ((ind + 1) * 4) ' ') in
    let () = print_parsetree rt (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Mult (lt, rt) -> let () = Printf.printf "%sMult(\n" indent in
    let () = print_parsetree lt (ind + 1) in
    let () = Printf.printf "%s,\n" (String.make ((ind + 1) * 4) ' ') in
    let () = print_parsetree rt (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Div (lt, rt) -> let () = Printf.printf "%sDiv(\n" indent in
    let () = print_parsetree lt (ind + 1) in
    let () = Printf.printf "%s,\n" (String.make ((ind + 1) * 4) ' ') in
    let () = print_parsetree rt (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Mod (lt, rt) -> let () = Printf.printf "%sMod(\n" indent in
    let () = print_parsetree lt (ind + 1) in
    let () = Printf.printf "%s,\n" (String.make ((ind + 1) * 4) ' ') in
    let () = print_parsetree rt (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Exp (lt, rt) -> let () = Printf.printf "%sExp(\n" indent in
    let () = print_parsetree lt (ind + 1) in
    let () = Printf.printf "%s,\n" (String.make ((ind + 1) * 4) ' ') in
    let () = print_parsetree rt (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Neg t -> let () = Printf.printf "%sNeg(\n" indent in
    let () = print_parsetree t (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Paren t -> let () = Printf.printf "%sParen(\n" indent in
    let () = print_parsetree t (ind + 1) in
    Printf.printf "%s)\n" (String.make ((ind + 1) * 4) ' ')
  | Int t -> Printf.printf "%s%d\n" indent t
  | Bool t -> Printf.printf "%s%B\n" indent t
  | Float t -> Printf.printf "%s%f\n" indent t
;;

let rec executeTree t = match t with
  | Or (lt, rt) -> begin match executeTree lt with
      | Int x when x <> 0 -> Bool true
      | Float x when x <> 0. -> Bool true
      | Bool true -> Bool true
      | _ -> begin match executeTree rt with
          | Int x when x <> 0 -> Bool true
          | Float x when x <> 0. -> Bool true
          | Bool true -> Bool true
          | _ -> Bool false
        end
    end
  | And (lt, rt) -> begin match executeTree lt with
      | Int 0 -> Bool false
      | Float 0. -> Bool false
      | Bool false -> Bool false
      | _ -> begin match executeTree rt with
          | Int 0 -> Bool false
          | Float 0. -> Bool false
          | Bool false -> Bool false
          | _ -> Bool true
        end
    end
  | Not (t) -> begin match executeTree t with
      | Int x -> Bool (x = 0)
      | Float x -> Bool (x = 0.)
      | Bool x -> Bool (not x)
      | _ -> raise NotImplemented
    end
  | Equ (lt, rt) -> begin match (executeTree lt, executeTree rt) with
      | (Int l, Int r) -> Bool (l = r)
      | (Int l, Bool r) -> Bool (l = (Bool.to_int r))
      | (Bool l, Int r) -> Bool ((Bool.to_int l) = r)
      | (Bool l, Bool r) -> Bool (l = r)
      | (Bool l, Float r) -> Bool ((Bool.to_float l) = r)
      | (Float l, Bool r) -> Bool (l = (Bool.to_float r))
      | (Float l, Float r) -> Bool (l = r)
      | (Float l, Int r) -> Bool (l = (float_of_int r))
      | (Int l, Float r) -> Bool ((float_of_int l) = r)
      | _ -> raise NotImplemented
    end
  | Neq (lt, rt) -> begin match (executeTree lt, executeTree rt) with
      | (Int l, Int r) -> Bool (l <> r)
      | (Int l, Bool r) -> Bool (l <> (Bool.to_int r))
      | (Bool l, Int r) -> Bool ((Bool.to_int l) <> r)
      | (Bool l, Bool r) -> Bool (l <> r)
      | (Bool l, Float r) -> Bool ((Bool.to_float l) <> r)
      | (Float l, Bool r) -> Bool (l <> (Bool.to_float r))
      | (Float l, Float r) -> Bool (l <> r)
      | (Float l, Int r) -> Bool (l <> (float_of_int r))
      | (Int l, Float r) -> Bool ((float_of_int l) <> r)
      | _ -> raise NotImplemented
    end
  | Geq (lt, rt) -> begin match (executeTree lt, executeTree rt) with
      | (Int l, Int r) -> Bool (l >= r)
      | (Int l, Bool r) -> Bool (l >= (Bool.to_int r))
      | (Bool l, Int r) -> Bool ((Bool.to_int l) >= r)
      | (Bool l, Bool r) -> Bool (l >= r)
      | (Bool l, Float r) -> Bool ((Bool.to_float l) >= r)
      | (Float l, Bool r) -> Bool (l >= (Bool.to_float r))
      | (Float l, Float r) -> Bool (l >= r)
      | (Float l, Int r) -> Bool (l >= (float_of_int r))
      | (Int l, Float r) -> Bool ((float_of_int l) >= r)
      | _ -> raise NotImplemented
    end
  | Gt (lt, rt) -> begin match (executeTree lt, executeTree rt) with
      | (Int l, Int r) -> Bool (l > r)
      | (Int l, Bool r) -> Bool (l > (Bool.to_int r))
      | (Bool l, Int r) -> Bool ((Bool.to_int l) > r)
      | (Bool l, Bool r) -> Bool (l > r)
      | (Bool l, Float r) -> Bool ((Bool.to_float l) > r)
      | (Float l, Bool r) -> Bool (l > (Bool.to_float r))
      | (Float l, Float r) -> Bool (l > r)
      | (Float l, Int r) -> Bool (l > (float_of_int r))
      | (Int l, Float r) -> Bool ((float_of_int l) > r)
      | _ -> raise NotImplemented
    end
  | Leq (lt, rt) -> begin match (executeTree lt, executeTree rt) with
      | (Int l, Int r) -> Bool (l <= r)
      | (Int l, Bool r) -> Bool (l <= (Bool.to_int r))
      | (Bool l, Int r) -> Bool ((Bool.to_int l) <= r)
      | (Bool l, Bool r) -> Bool (l <= r)
      | (Bool l, Float r) -> Bool ((Bool.to_float l) <= r)
      | (Float l, Bool r) -> Bool (l <= (Bool.to_float r))
      | (Float l, Float r) -> Bool (l <= r)
      | (Float l, Int r) -> Bool (l <= (float_of_int r))
      | (Int l, Float r) -> Bool ((float_of_int l) <= r)
      | _ -> raise NotImplemented
    end
  | Lt (lt, rt) -> begin match (executeTree lt, executeTree rt) with
      | (Int l, Int r) -> Bool (l < r)
      | (Int l, Bool r) -> Bool (l < (Bool.to_int r))
      | (Bool l, Int r) -> Bool ((Bool.to_int l) < r)
      | (Bool l, Bool r) -> Bool (l < r)
      | (Bool l, Float r) -> Bool ((Bool.to_float l) < r)
      | (Float l, Bool r) -> Bool (l < (Bool.to_float r))
      | (Float l, Float r) -> Bool (l < r)
      | (Float l, Int r) -> Bool (l < (float_of_int r))
      | (Int l, Float r) -> Bool ((float_of_int l) < r)
      | _ -> raise NotImplemented
    end
  | Plus (lt, rt) -> begin match (executeTree lt, executeTree rt) with
      | (Int l, Int r) -> Int (l + r)
      | (Int l, Bool r) -> Int (l + (Bool.to_int r))
      | (Bool l, Int r) -> Int ((Bool.to_int l) + r)
      | (Bool l, Bool r) -> Int ((Bool.to_int l) + (Bool.to_int r))
      | (Bool l, Float r) -> Float ((Bool.to_float l) +. r)
      | (Float l, Bool r) -> Float (l +. (Bool.to_float r))
      | (Float l, Float r) -> Float (l +. r)
      | (Float l, Int r) -> Float (l +. (float_of_int r))
      | (Int l, Float r) -> Float ((float_of_int l) +. r)
      | _ -> raise NotImplemented
    end
  | Minus (lt, rt) -> begin match (executeTree lt, executeTree rt) with
      | (Int l, Int r) -> Int (l - r)
      | (Int l, Bool r) -> Int (l - (Bool.to_int r))
      | (Bool l, Int r) -> Int ((Bool.to_int l) - r)
      | (Bool l, Bool r) -> Int ((Bool.to_int l) - (Bool.to_int r))
      | (Bool l, Float r) -> Float ((Bool.to_float l) -. r)
      | (Float l, Bool r) -> Float (l -. (Bool.to_float r))
      | (Float l, Float r) -> Float (l -. r)
      | (Float l, Int r) -> Float (l -. (float_of_int r))
      | (Int l, Float r) -> Float ((float_of_int l) -. r)
      | _ -> raise NotImplemented
    end
  | Mult (lt, rt) -> begin match (executeTree lt, executeTree rt) with
      | (Int l, Int r) -> Int (l * r)
      | (Int l, Bool r) -> Int (l * (Bool.to_int r))
      | (Bool l, Int r) -> Int ((Bool.to_int l) * r)
      | (Bool l, Bool r) -> Int ((Bool.to_int l) * (Bool.to_int r))
      | (Bool l, Float r) -> Float ((Bool.to_float l) *. r)
      | (Float l, Bool r) -> Float (l *. (Bool.to_float r))
      | (Float l, Float r) -> Float (l *. r)
      | (Float l, Int r) -> Float (l *. (float_of_int r))
      | (Int l, Float r) -> Float ((float_of_int l) *. r)
      | _ -> raise NotImplemented
    end
  | Div (lt, rt) -> begin match (executeTree lt, executeTree rt) with
      | (Int l, Int r) -> Int (l / r)
      | (Int l, Bool r) -> Int (l / (Bool.to_int r))
      | (Bool l, Int r) -> Int ((Bool.to_int l) / r)
      | (Bool l, Bool r) -> Int ((Bool.to_int l) / (Bool.to_int r))
      | (Bool l, Float r) -> Float ((Bool.to_float l) /. r)
      | (Float l, Bool r) -> Float (l /. (Bool.to_float r))
      | (Float l, Float r) -> Float (l /. r)
      | (Float l, Int r) -> Float (l /. (float_of_int r))
      | (Int l, Float r) -> Float ((float_of_int l) /. r)
      | _ -> raise NotImplemented
    end
  | Mod (lt, rt) -> begin match (executeTree lt, executeTree rt) with
      | (Int l, Int r) -> Int (l mod r)
      | (Int l, Bool r) -> Int (l mod (Bool.to_int r))
      | (Bool l, Int r) -> Int ((Bool.to_int l) mod r)
      | (Bool l, Bool r) -> Int ((Bool.to_int l) mod (Bool.to_int r))
      | (Bool l, Float r) -> Float (Float.rem (Bool.to_float l) r)
      | (Float l, Bool r) -> Float (Float.rem l (Bool.to_float r))
      | (Float l, Float r) -> Float (Float.rem l r)
      | (Float l, Int r) -> Float (Float.rem l (float_of_int r))
      | (Int l, Float r) -> Float (Float.rem (float_of_int l) r)
      | _ -> raise NotImplemented
    end
  | Exp (lt, rt) -> begin match (executeTree lt, executeTree rt) with
      | (Int l, Int r) -> Int (int_of_float ((float_of_int l) ** (float_of_int r)))
      | (Int l, Bool r) -> Int (int_of_float ((float_of_int l) ** (Bool.to_float r)))
      | (Bool l, Int r) -> Int (int_of_float ((Bool.to_float l) ** (float_of_int r)))
      | (Bool l, Bool r) -> Int (int_of_float ((Bool.to_float l) ** (Bool.to_float r)))
      | (Bool l, Float r) -> Float ((Bool.to_float l) ** r)
      | (Float l, Bool r) -> Float (l ** (Bool.to_float r))
      | (Float l, Float r) -> Float (l ** r)
      | (Float l, Int r) -> Float (l ** (float_of_int r))
      | (Int l, Float r) -> Float ((float_of_int l) ** r)
      | _ -> raise NotImplemented
    end
  | Neg t -> begin match (executeTree t) with
      | Int v -> Int (-v)
      | Bool v -> Int (-(Bool.to_int v))
      | Float v -> Float (-.v)
      | _ -> raise NotImplemented
    end
  | Paren t -> executeTree t
  | _ -> t
;;

let rec main () =
  let () = print_string ">>> " in
  let s = read_line () in
  if s = "exit()" then ()
  else
    let toks = tokenize s in
    (* let () = print_tokens toks in *)
    match toks with
      | (Indent_tok x)::t when x = 0 -> begin match t with
          | (Id_tok v)::Equ_tok::rt -> let ptree, rt = bool1 rt in
              let () = Hashtbl.replace vartbl v (executeTree ptree)
            in
            main ()
          | [] -> main ()
          | _ -> let ptree, rt = bool1 t in
            (* let () = print_parsetree ptree 0 in *)
            let () = print_parsetree (executeTree ptree) 0 in
            main ()
          end
      | _ -> print_string "Indentation Error\n"
;;

main ();;
