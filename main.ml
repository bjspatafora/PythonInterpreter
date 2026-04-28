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
          | Id of string
          | If of (expr * (statement list) * (statement list))
          | While of (expr * (statement list))
          | Empty
and
  statement = Statement of (string * expr)
            | Expr of expr
;;

let vartbl : (string, expr) Hashtbl.t = Hashtbl.create 10;;

let rec input () =
  let () = print_string ">>> " in
  let s = read_line () in
  if s = "exit()" then raise Exit
  else tokenize s
;;

let rec print_parsetree tree ind =
  let indent = String.make (ind * 4) ' ' in
  match tree with
  | While (cond, exp) -> let () = Printf.printf "%sWhile(\n" indent in
    let () = print_parsetree cond (ind + 1) in
    let () = Printf.printf "%sdo\n" indent in
    let () = print_statements exp (ind + 1) in
    Printf.printf "%s)\n" indent
  | If (cond, exp, el) -> let () = Printf.printf "%sIf(\n" indent in
    let () = print_parsetree cond (ind + 1) in
    let () = Printf.printf "%sthen\n" indent in
    let () = print_statements exp (ind + 1) in
    let () = Printf.printf "%selse\n" indent in
    let () = print_statements el (ind + 1) in
    Printf.printf "%s)\n" indent
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
  | Id t -> Printf.printf "%s%s\n" indent t
  | _ -> ()
and
  print_statement s ind = match s with
  | Statement (v, e) -> let indent = String.make (ind * 4) ' ' in
    let () = Printf.printf "%s%s=(\n" indent v in
    let () = print_parsetree e (ind + 1) in
    Printf.printf "%s)\n" indent
  | Expr e -> print_parsetree e ind
and
  print_statements xs ind = match xs with
  | [] -> ()
  | x::xs -> let () = print_statement x ind in print_statements xs ind
;;

let rec executeTree t = match t with
  | While (cond, exp) -> begin match executeTree cond with
      | Int x when x = 0 -> Empty
      | Float x when x = 0. -> Empty
      | Bool false -> Empty
      | _ -> let () = executeStatements exp in executeTree t
    end
  | If (cond, exp, el) -> begin match executeTree cond with
      | Int x when x = 0 -> let () = executeStatements el in Empty
      | Float x when x = 0. -> let () = executeStatements el in Empty
      | Bool false -> let () = executeStatements el in Empty
      | _ -> let () = executeStatements exp in Empty
    end
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
  | Id t -> Hashtbl.find vartbl t
  | _ -> t
and
  executeStatement s = match s with
  | Statement (v, x) -> let e = executeTree x in
    (* let () = Printf.printf "%s = " v in
       let () = print_parsetree e 0 in *)
    Hashtbl.replace vartbl v e
  | Expr x -> print_parsetree (executeTree x) 0
and
  executeStatements xs = match xs with
  | [] -> ()
  | x::xs -> let () = executeStatement x in executeStatements xs
;;

let rec block toks ind : (statement list * token list * int) =
  let rec ifloop cb eb els ind : (statement list * statement list * statement list * int) =
    (* let () = Printf.printf "\tin ifloop %d\n" ind in *)
    let toks = input () in
    try
      begin match toks with
        | (Indent_tok x)::[] -> ifloop cb eb els ind
        | (Indent_tok x)::rt when x = ind -> let b, rt, endind = block rt ind in
          begin match (rt, endind, els) with
            | ([], x, 0) when x = ind -> ifloop (cb @ b) eb els ind
            | ([], x, 1) when x = ind -> ifloop cb (eb @ b) els ind
            | ([], x, _) when x < ind && x mod 4 = 0 -> cb, eb, b, x
            | (_, _, _) -> let () = print_string "Invalid Input\n" in
              (* let () = print_tokens rt in
                 let () = Printf.printf "%d, %d\n" x els in *)
              ifloop cb eb els ind
          end
        | (Indent_tok x)::Else_tok::Colon_tok::[] when x = ind - 4 -> if els = 1
          then let () = print_string "Double else\n" in raise BadToks
          else ifloop cb eb 1 ind
        | (Indent_tok x)::Else_tok::Colon_tok::rt when x = ind - 4 -> if els = 1
          then let () = print_string "Double else\n" in raise BadToks
          else let exp, rt = statement rt in
            begin match rt with
              | [] -> cb, [exp], [Expr Empty], (ind - 4)
              | _ -> let () = print_string "Invalid Input\n" in ifloop cb eb els ind
            end
        | (Indent_tok x)::rt when (x < ind && x mod 4 = 0) -> let b, rt, endind = block rt x in
          begin match rt with
            | [] -> cb, eb, b, x
            | _ -> let () = print_string "Invalid Input\n" in ifloop cb eb els ind
          end
        | _ -> let () = print_string "Indentation Error\n" in ifloop cb eb els ind
      end
    with
    | BadToks -> let () = print_string "Invalid Input\n" in ifloop cb eb els ind
    | UnmatchedParen -> let () = print_string "Invalid Input: unmatched parentheses\n" in
      ifloop cb eb els ind
  in
  let rec whileloop b ind =
    (* let () = print_string "\tIn while loop\n" in *)
    let toks = input () in
    try
      begin match toks with
        | (Indent_tok x)::[] -> whileloop b ind
        | (Indent_tok x)::rt when x = ind -> let ib, rt, endind = block rt ind in
          begin match (rt, endind) with
            | ([], x) when x = ind -> whileloop (b @ ib) ind
            | ([], x) when x < ind && x mod 4 = 0 -> b, ib, x
            | (_, _) -> let () = print_string "Invalid Input\n" in
              whileloop b ind
          end
        | (Indent_tok x)::rt when (x < ind && x mod 4 = 0) -> let ib, rt, endind = block rt x in
          begin match rt with
            | [] -> b, ib, x
            | _ -> let () = print_string "Invalid Input\n" in whileloop b ind
          end
        | _ -> let () = print_string "Indentation Error\n" in whileloop b ind
      end
    with
    | BadToks -> let () = print_string "Invalid Input\n" in whileloop b ind
    | UnmatchedParen -> let () = print_string "Invalid Input: unmatched parentheses\n" in
      whileloop b ind
  in
  (* let () = Printf.printf "\tin block %d\n" ind in *)
  match toks with
  | If_tok::rt -> let cond, rtoks = bool1 rt in
    begin match rtoks with
      | Colon_tok::[] -> let iblock, eblock, ne, endind = ifloop [] [] 0 (ind + 4) in
        [Expr (If (cond, iblock, eblock))] @ ne, [], endind
      | Colon_tok::rt -> let exp, rt = statement rt in
        begin match rt with
          | [] -> let iblock, eblock, ne, endind = ifloop [exp] [] 2 ind in
            [Expr (If (cond, iblock, eblock))] @ ne, rt, endind
          | _ -> raise BadToks
        end
      | _ -> raise BadToks
    end
  | While_tok::rt -> let cond, rtoks = bool1 rt in
    begin match rtoks with
      | Colon_tok::[] -> let b, ne, endind = whileloop [] (ind + 4) in
        [Expr (While (cond, b))] @ ne, [], endind
      | Colon_tok::rt -> let exp, rt = statement rt in
        begin match rt with
          | [] -> [Expr (While (cond, [exp]))], rt, (ind - 4)
          | _ -> raise BadToks
        end
      | _ -> raise BadToks
    end
  | _ -> let s, rt = statement toks in [s], rt, ind
and
  statement toks : (statement * token list) = match toks with
  | (Id_tok v)::Equ_tok::rt -> let exp, rt = bool1 rt in
    Statement (v, exp), rt
  | _ -> let exp, rt = bool1 toks in Expr exp, rt
and
  bool1 toks =
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
  (* let () = print_string "Bool3s: " in
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
    | (Int_tok x)::rt -> Int x, rt
    | (Id_tok x)::rt -> Id x, rt
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
    
let rec main () =
  let toks = input () in
  (* let () = print_tokens toks in *)
  match toks with
  | (Indent_tok x)::[] -> main ()
  | (Indent_tok x)::t when x = 0 -> begin try
      let b, rt, _ = block t 0 in
      begin match rt with
        | [] -> let () = executeStatements b in
          main ()
        | _ -> let () = print_string "Invalid Input\n" in main ()
      end
      with
      | BadToks -> let () = print_string "Invalid Input\n" in main ()
      | UnmatchedParen -> let () = print_string "Invalid Input: unmatched parentheses\n" in
        main ()
    end
  | _ -> let () = print_string "Indentation Error\n" in main ()
;;

main ();;
