(*Type definitions*)

type 'a node =
  | None
  | One of 'a
  | Many of 'a node list

type ok_state = {result: string node; remaining: string list; position: int}

type error_state = {remaining: string list; reason: string; position: int}

type state =
  | Ok of ok_state
  | Error of error_state

(*Type utils*)

let extract_state =
  fun state -> match state with
    | Ok {result;remaining;position} -> {result=result; remaining=remaining; position=position}
    | Error {remaining;reason;position} -> raise (Failure (Printf.sprintf "Attempted parse on error %s in position %i" reason position))

let extract_result =
  fun state -> match state with
  | Ok {result;remaining;position} -> result
  | Error {remaining;reason;position} -> raise (Failure (Printf.sprintf "Attempted parse on error %s in position %i" reason position))

let extract_remaining =
  fun state -> match state with
  | Ok {result;remaining;position} -> remaining
  | Error {remaining;reason;position} -> raise (Failure (Printf.sprintf "Attempted parse on error %s in position %i" reason position))

let extract_position =
  fun state -> match state with
  | Ok {result;remaining;position} -> position
  | Error {remaining;reason;position} -> raise (Failure (Printf.sprintf "Attempted parse on error %s in position %i" reason position))

let inc_position =
  fun state -> (extract_position state) + 1

let extract_node_char =
  fun node -> match node with
    | Many h -> raise (Failure "attempted extraction of many")
    | One h -> h
    | None -> raise (Failure "attempted to extract None")

let extract_node_many =
  fun node -> match node with
    | Many h -> h
    | One h -> raise (Failure "attempted to extract One instead of many")
    | None -> []

let make_ok =
  fun result remaining position ->
  Ok {result=result; remaining=remaining; position=position}

let make_error =
  fun remaining reason position ->
  Error {remaining=remaining; reason=reason;position=position}

let explode (str: string): string list =
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) ((String.make 1 str.[a]) :: b)
  in
  exp (String.length str - 1) []

let parse_lambda_string =
  fun str -> (make_ok None (explode str) 0)


(*List utils*)

let nth n xs =
  let rec aux acc n xs = (match xs with
      | [] -> (raise (Failure "Either empty list or too large n"))
      | [x] -> (if (acc = n) then x else (raise (Failure "nth of list too big")))
      | h::t -> if (acc = n) then h else (aux (acc + 1) n t)) in
  (aux 0 n xs)

let reverse xs =
  let rec aux acc xs =
    match xs with
    | [] -> (raise (Failure "Empty list"))
    | [x] -> x::acc
    | h :: t -> aux (h::acc) xs in
  aux [] xs

let backnth n xs =
  let r_xs = (reverse xs) in
  (nth n r_xs)

let cdr xs = let rec aux acc xs =
                    match xs with
                    | [] -> []
                    | [x] -> [x]
                    | h :: t -> if (acc = 0) then (aux (acc + 1) t) else (h :: (aux (acc + 1 ) t)) in
  aux 0 xs

(*Generic combinators*)
              
let pchar c =
  fun state -> match (extract_remaining state) with
    | [] -> (make_error [] "End" (extract_position state))
    | h :: t -> if (h=c) then
        (match (extract_result state) with
         | None -> (make_ok (One h) t (inc_position state))
         | One x -> (make_ok (Many [One x; One h]) t (inc_position state))
         | Many x -> (make_ok (Many (x @ [(One h)])) t (inc_position state)) 
        )
      else (make_error
              (extract_remaining state)
              (Printf.sprintf ("No match on %s looking for %s on position %i") h c (inc_position state))
              (inc_position state))

let npchar c =
  fun state -> match (extract_remaining state) with
    | [] -> (make_error [] "End" (extract_position state))
    | h :: t -> if (h=c)
      then (make_error
              (extract_remaining state)
              (Printf.sprintf "Found character %s on %i" h (inc_position state))
              (inc_position state))
      else (make_ok (extract_result state)
              (extract_remaining state)
              (inc_position state))

let pblock parser1 =
  fun state ->
  let r1 = (parser1 (make_ok None (extract_remaining state) (extract_position state))) in
  match r1 with
  | Ok {result;remaining; position} -> (match (extract_result state) with
      | None -> (make_ok result remaining position)
      | One h -> (make_ok (Many ([One h] @ [result])) remaining position)
      | Many h -> (make_ok
                     (Many ((extract_node_many (extract_result state)) @ [result]))
                     remaining position))
  | Error {remaining;reason;position} -> make_error remaining reason position

let rec listparse parsers =
  fun state ->
  match parsers with
  | [] -> (make_error (extract_remaining state) "Empty parser list" (extract_position state))
  | [x] -> (x state)
  | h :: t -> let r1 = (h state) in
    match r1 with
    | Ok {result;remaining;position} -> (listparse t (make_ok result remaining position))
    | Error {remaining;reason;position} -> (make_error remaining reason position)
  
let rec choiceparse parsers =
  fun state ->
  match parsers with
  | [] -> (make_error (extract_remaining state)
             (Printf.sprintf "No choice matched on %i" (extract_position state))
             (extract_position state))
  | [x] ->
    (match (x state) with
     | Ok {result;remaining;position} -> (make_ok result remaining position)
     | Error {remaining;reason;position} -> (make_error (extract_remaining state)
                                               (Printf.sprintf "No choice matched on %i" (extract_position state)) (extract_position state)))
  | h :: t -> let r1 = (h state) in
    match r1 with
    | Ok {result;remaining;position} -> (make_ok result remaining position)
    | Error {remaining;reason;position} -> (choiceparse t state)

let nullparse parser1 =
  fun state -> let r1 = (parser1 state) in
  match r1 with
  | Ok {result;remaining;position} -> (make_ok (extract_result state) remaining position)
  | Error {remaining;reason;position} -> (make_error remaining reason position)

let oneormoreparse parser1 =
  fun state ->
  let rec aux acc state =
    let r1 = (parser1 state) in match r1 with
    | Ok {result;remaining;position} -> (aux (acc+1) (make_ok result remaining position))
    | Error {remaining;reason;position} -> if acc = 0 then
        (make_error remaining reason position) else
        (make_ok (extract_result state) remaining position) in
  aux 0 state

let letterparse =
  fun state -> (choiceparse [(pchar "a");(pchar "b");(pchar "c");(pchar "d");(pchar "e");(pchar "f");(pchar "g");(pchar "h");(pchar "i");(pchar "j"); (pchar "l"); (pchar "m"); (pchar "n"); (pchar "o"); (pchar "p");(pchar "x");(pchar "y");] state)

let digitparse =
  fun state -> (choiceparse [(pchar "0"); (pchar "1"); (pchar "2"); (pchar "3"); (pchar "4"); (pchar "5"); (pchar "6"); (pchar "7"); (pchar "8"); (pchar "9")] state)
