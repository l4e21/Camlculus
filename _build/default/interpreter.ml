open Parser

let rec eval exp env = match exp with
  | EInt h -> EInt h
  | EBool h -> EBool h
  | EVar h -> (Context.find h env)
  | EAbstract (param, body) -> EClosure {param=param; body=body; env=env}
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
  | EClosure {param;body;env} -> EClosure {param;body;env}
  | ENative f -> (raise (Failure "How do you even eval a native"))
               

let env_init =
  Context.empty
  |> Context.add "x" (EInt 3)
  |> Context.add "p" (ENative (fun x -> (print_endline "hello world."; x)))


let interp str = (eval (parse str) env_init)
