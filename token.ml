(* File: token.ml *)

type toktype = Op_tok
             | Val_tok
;;

type token = Int_tok of int
           | Bool_tok of bool
           | Float_tok of float
           | Id_tok of string
           | Plus_tok
           | Minus_tok
           | Neg_tok
           | Mult_tok
           | Div_tok
           | Mod_tok
           | Exp_tok
           | Is_Equ_tok
           | Is_Neq_tok
           | Is_Geq_tok
           | Is_Gt_tok
           | Is_Leq_tok
           | Is_Lt_tok
           | Equ_tok
           | And_tok
           | Or_tok
           | Not_tok
           | If_tok
           | Else_tok
           | Colon_tok
           | Lparen_tok
           | Rparen_tok
           | Lcomment_tok
           | Rcomment_tok
           | Indent_tok of int
;;

exception IgnoreCase;;

let token_to_string t = match t with
  | Int_tok x -> Printf.sprintf "Int_tok %d" x
  | Bool_tok x -> Printf.sprintf "Bool_tok %b" x
  | Float_tok x -> Printf.sprintf "Float_tok %f" x
  | Id_tok x -> Printf.sprintf "Id_tok \"%s\"" x
  | Plus_tok -> "Plus_tok"
  | Minus_tok -> "Minus_tok"
  | Mult_tok -> "Mult_tok"
  | Div_tok -> "Div_tok"
  | Mod_tok -> "Mod_tok"
  | Exp_tok -> "Exp_tok"
  | Is_Equ_tok -> "Is_Equ_tok"
  | Is_Neq_tok -> "Is_Neq_tok"
  | Equ_tok -> "Equ_tok"
  | If_tok -> "If_tok"
  | Else_tok -> "Else_tok"
  | Neg_tok -> "Neg_tok"
  | Lparen_tok -> "Lparen_tok"
  | Rparen_tok -> "Rparen_tok"
  | Lcomment_tok -> "Lcomment_tok"
  | Rcomment_tok -> "Rcomment_tok"
  | Indent_tok x -> Printf.sprintf "Indent %d" x
  | _ -> ""
;;

let print_token t = print_string (token_to_string t);;

let print_tokenval t = match t with
  | Int_tok x -> print_int x
  | Bool_tok x -> Printf.printf "%b" x
  | Float_tok x -> print_float x
  | _ -> raise IgnoreCase
;;
    
let print_tokens ts =
  Printf.printf "[%s]" (String.concat ", " (List.map token_to_string ts));;
