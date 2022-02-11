include Combinator

(*Type definitions*)

module Context = Map.Make (String)

type expression =
  | EInt of int
  | EBool of bool
  | EVar of string
  | EAbstract of (string * typeval * expression)
  | EClosure of {param: string; body: expression; env: expression Context.t}
  | ENative of (expression -> expression)
  | EApply of (expression * expression)
  | EIf of (expression * expression * expression)
  | EAdd of (expression * expression)
  | ESub of (expression * expression)
  | ETimes of (expression * expression)
  | EDivide of (expression * expression)
  | EEq of (expression * expression)

and typeval =
  | TInt
  | TBool
  | TArrow of (typeval * typeval)
[@@deriving show { with_path=false }]
(*Type utils*)

let rec format_typeval = function
  | TInt -> "INT"
  | TBool -> "BOOL"
  | TArrow (e1, e2) -> (format_typeval e1) ^ " -> " ^ (format_typeval e2)

let extract_int x =
  match x with
  | EInt h -> h
  | _ -> raise (Failure "Attempted int extract on non-int")

let extract_bool x =
  match x with
  | EBool h -> h
  | _ -> raise (Failure "Attempt bool extract on non-bool")

and extract_var_symbol h =
  match h with
      | EVar h -> h
      | _ -> (raise (Failure "attempted var extraction on non-var"))

let make_abstract arg typ param =
  (EAbstract (arg, typ, param))

let make_apply ex1 ex2 =
  (EApply (ex1, ex2))

(*Language specific parsers*)

let typeintparse =
  fun state -> (pblock (listparse [(pchar "I");
                                   (pchar "N");
                                   (pchar "T")])) state
let typeboolparse =
  fun state -> (pblock (listparse [(pchar "B");
                                   (pchar "O");
                                   (pchar "O");
                                   (pchar "L")])) state

let rec arrowparse =
  fun state -> (listparse
                          [(nullparse (pchar "-"));
                           (nullparse (pchar ">"));
                           (nullparse (pchar "("));
                           (choiceparse [(arrowparse); (typeintparse); (typeboolparse)]);
                           (nullparse (pchar ","));
                           (choiceparse [(arrowparse); (typeintparse); (typeboolparse)]);
                          (nullparse (pchar ")"))]) state

let typeparse =
  fun state -> (pblock (listparse [(pchar ":");
                                   (choiceparse [
                                       (arrowparse);
                                       (typeintparse);
                                       (typeboolparse);
                                     ])])) state

let varparse =
  fun state -> (pblock (listparse [(pchar "|");
                                   (oneormoreparse letterparse);
                                   (pchar "|");
                                  ]) state)



let intparse =
  fun state -> (pblock (oneormoreparse digitparse) state)

let boolparse =
  fun state -> (pblock (choiceparse
                  [(listparse [(pchar "t"); (pchar "r"); (pchar "u"); (pchar "e")]);
                   (listparse [(pchar "f"); (pchar "a"); (pchar "l"); (pchar "s"); (pchar "e")])]) state)

let rec lamparse =
  fun state ->
  (pblock
     (listparse [(pchar "\\");
                 varparse;
                 typeparse;
                 (nullparse (pchar "."));
                 expr_parse])
 state)

and apparse =
  fun state ->
  (pblock
     (listparse [(pchar "(");
                 expr_parse;
                 (nullparse (pchar " "));
                 expr_parse;
                 (pchar ")")]) state)

and ifparse =
  fun state ->
  (pblock
     (listparse [(pchar "?");
                 expr_parse;
                 (nullparse (pchar ":"));
                 expr_parse;
                 (nullparse (pchar ":"));
                 expr_parse;
                ])
     state)

and opparse =
  fun state ->
  (pblock (listparse [(choiceparse [(pchar "+");(pchar "-");(pchar "*");(pchar "/");(pchar "=")]);
                       expr_parse;
                      (nullparse (pchar ","));
                      expr_parse;
                     ]) state) 

and expr_parse =
  fun state ->
  (choiceparse [intparse; varparse; boolparse; lamparse; apparse; ifparse; opparse] state)

(*AST Traversal*)

let rec traverse_ast =
  fun node -> match node with
    | None -> raise (Failure "Tree doesn't exist")
    | One h -> EInt (int_of_string h)
    | Many h -> (match (extract_node_char (nth 0 h)) with
        | "\\" -> (make_abstraction (cdr h))
        | "(" -> (make_application (cdr h))
        | "|" -> EVar (make_var (cdr h))
        | "f" -> EBool false
        | "t" -> EBool true
        | "+" -> (make_plus (cdr h))
        | "-" -> (make_sub (cdr h))
        | "/" -> (make_divide (cdr h))
        | "*" -> (make_times (cdr h))
        | "=" -> (make_eq (cdr h))
        | "?" -> (make_if (cdr h))
        | _ -> (EInt (int_of_string (make_int h)))
      )

and identify_type =
  fun node -> let match_atomic_type atom = match (extract_node_char (nth 0 atom)) with
      | "I" -> TInt
      | "B" -> TBool
      | _ -> raise (Failure "Unknown type primitive")
      in 
      let typedefs = (cdr (extract_node_many node)) in
      match List.length (typedefs) with
      | 1 -> (match_atomic_type (extract_node_many (nth 0 (typedefs))))
      | 2 -> (TArrow ((identify_type (Many [(One ":");(nth 0 typedefs)])), (identify_type (Many [(One ":");(nth 0 typedefs)]))))
      | _ -> raise (Failure "Wrong number of types defined")

and make_abstraction h =
  (make_abstract (extract_var_symbol (traverse_ast (nth 0 h))) (identify_type (nth 1 h)) (traverse_ast (nth 2 h)))

and make_application h =
  (make_apply (traverse_ast (nth 0 h)) (traverse_ast (nth 1 h)))

and make_int h = match h with
  | [] -> ""
  | [x] -> (extract_node_char x)
  | h :: t -> (extract_node_char h) ^ (make_int t)

and make_plus h = EAdd ((traverse_ast (nth 0 h)), (traverse_ast (nth 1 h)))
and make_sub h = ESub ((traverse_ast (nth 0 h)), (traverse_ast (nth 1 h)))
and make_times h = ETimes ((traverse_ast (nth 0 h)), (traverse_ast (nth 1 h)))
and make_divide h = EDivide ((traverse_ast (nth 0 h)), (traverse_ast (nth 1 h)))
and make_eq h = EEq ((traverse_ast (nth 0 h)), (traverse_ast (nth 1 h)))

and make_var h = match h with
  | [] -> ""
  | [x] -> ""
  | h :: t -> (extract_node_char h) ^ (make_var t)

and make_if h =
  (EIf ((traverse_ast (nth 0 h)), (traverse_ast (nth 1 h)), (traverse_ast (nth 2 h))))

let parse str = str |> parse_lambda_string |> expr_parse |> extract_result |> traverse_ast
