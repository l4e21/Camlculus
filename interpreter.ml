include Parser

let rec equal a b =
  match (a, b) with
  | (TInt, TInt) -> true
  | (TBool, TBool) -> true
  | ((TArrow (param_a, body_a)), (TArrow(param_b,body_b))) ->
    if (equal param_a param_b) then
      (if (equal body_a body_b) then true else false)
      else false
  | _ -> false

let rec infer exp env = match exp with
  | EInt h -> TInt
  | EBool h -> TBool
  | EVar h -> (Context.find h env)
  | EAbstract (param, typ, body) -> let new_env = (Context.add param typ env) in
    let body_typ = (infer body new_env) in (TArrow (typ, body_typ))
  | EApply (func, arg) -> let func_typ = (infer func env) in
    let arg_typ = (infer arg env) in
    begin match func_typ with
    | TInt -> raise (Failure "Attempted to apply primitive (typecheck)")
    | TBool -> raise (Failure "Attempted to apply primitive (typecheck)")
    | TArrow (typ, body_typ) -> if (equal typ arg_typ) then body_typ else
        raise (Failure (Printf.sprintf "Type error in application"))
    end
  | EIf (p, l, r) -> TArrow (TBool, (TArrow ((infer l env), (infer r env)))) 
  | EAdd (e1, e2) -> TArrow (TInt, (TArrow (TInt, TInt))) 
  | ESub (e1, e2) -> TArrow (TInt, (TArrow (TInt, TInt))) 
  | ETimes (e1, e2) -> TArrow (TInt, (TArrow (TInt, TInt))) 
  | EDivide (e1, e2) -> TArrow (TInt, (TArrow (TInt, TInt)))
  | EEq (e1, e2) -> TArrow ((infer e1 env), (TArrow ((infer e2 env), TBool)))
  | ENative e1 -> (TArrow (TBool, TBool))
  | _ -> raise (Failure "Can't infer this expression")
    

let rec eval exp env = match exp with
  | EInt h -> EInt h
  | EBool h -> EBool h
  | EVar h -> (Context.find h env)
  | EAbstract (param, _typ, body) -> EClosure {param=param; body=body; env=env}
  | EApply (func, arg) -> begin
      match (eval func env) with
      | EClosure {param;body;env=env1} -> let arg_val = (eval arg env) in
        (eval body (Context.add param arg_val env1))
      | ENative f -> (f arg)
      | _ -> raise (Failure "Attempted to apply primitive") end
  | EIf (p, l, r) -> (if (extract_bool (eval p env)) then (eval l env) else (eval r env))
  | EAdd (expr1, expr2) -> EInt ((extract_int (eval expr1 env)) +
                                 (extract_int (eval expr2 env)))
  | ESub (expr1, expr2) -> EInt ((extract_int (eval expr1 env)) -
                                 (extract_int (eval expr2 env)))
  | ETimes (expr1, expr2) -> EInt ((extract_int (eval expr1 env)) *
                                   (extract_int (eval expr2 env)))
  | EDivide (expr1, expr2) -> EInt ((extract_int (eval expr1 env)) /
                                    (extract_int (eval expr2 env)))
  | EEq (expr1, expr2) -> begin match ((eval expr1 env), (eval expr2 env)) with
      | (EInt h1, EInt h2) -> EBool (h1=h2)
      | (EBool h1, EBool h2) -> EBool (h1=h2)
      | _ -> raise (Failure "Can't find some boolean equality for these expressions")
    end
    
  | EClosure {param;body;env} -> EClosure {param;body;env}
  | ENative f -> (raise (Failure "How do you even eval a native"))
               

let env_init =
  Context.empty
  |> Context.add "x" (EInt 3)
  |> Context.add "p" (ENative (fun x -> (print_endline "hello world."; x)))


let interp str = let typereturn = (infer (parse str) Context.empty) in
  print_endline (format_typeval (typereturn));
  (eval (parse str) env_init)
                 
  (*Useage*);
  (*(\|x|.|x| |x|)  -->  |x| *)
  (*(\|x|.(|x| |x|) \|x|.(|x| |x|)) --> Infinite recursion (Omega Combinator) *)
  (*Numbers and Booleans can also be passed instead of variables*)
  (*(\|x|.?|x|:3:False True) --> 3*)

  (*TODO*)
  (*Equality statements =*)
