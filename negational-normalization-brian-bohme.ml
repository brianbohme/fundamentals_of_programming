(* negational-normalization-brian-bohme.ml *)
(* Fundamentals of Programming Mini-Project #2 *)
(* Brian Bohme *)
(* A0144436H *)
(* brianbohme@u.yale-nus.edu.sg *)
(* March 2, 2017 *)

(* Types and exceptions *)

type name = string;;

type boolean_expression_nnf =
  | Variable_nnf of name
  | Conjunction_nnf of boolean_expression_nnf * boolean_expression_nnf
  | Disjunction_nnf of boolean_expression_nnf * boolean_expression_nnf
  | Negation_nnf of name;;

type boolean_expression =
  | Variable of name
  | Conjunction of boolean_expression * boolean_expression
  | Disjunction of boolean_expression * boolean_expression
  | Negation of boolean_expression;;

exception Unbound of string;;

(* Environment *)

module type ENVIRONMENT =
  sig
    type 'a environment
    val empty : 'a environment
    val extend : name -> 'a -> 'a environment -> 'a environment
    val lookup : name -> 'a environment -> 'a option
  end;;

module Environment : ENVIRONMENT =
  struct
    type 'a environment = (name * 'a) list

    let empty = []

    let extend n d e
      = (n, d) :: e

    let lookup n e_current =
      let rec look_it_up e =
        match e with
        | [] ->
           None
        | (n', d) :: e' ->
           if n = n'
           then Some d
           else look_it_up e'
      in look_it_up e_current

  end;;

let rec all_possible_Boolean_environments ns =
  match ns with
  | [] ->
     [Environment.empty]
  | n :: ns' ->
     let es = all_possible_Boolean_environments ns'
     in List.append (List.map (fun e -> Environment.extend n true e) es)
                    (List.map (fun e -> Environment.extend n false e) es);;

(* Auxiliary functions *)

(* Lookup in the environment *)

let lookup x env = 
  match Environment.lookup x env with
      | Some b -> 
          b
      | None -> 
          raise (Unbound "variable");;

(* Generates a list of all variables in boolean expression for optimized space usage *)

let boolean_expression_variables e_init = 
    let rec visit e lst = 
      match e with 
      | Variable x -> 
          if List.mem x lst 
          then lst 
          else x :: lst
      | Conjunction (e1, e2) -> 
          visit e1 (visit e2 lst)
      | Disjunction (e1, e2) -> 
          visit e1 (visit e2 lst)
      | Negation x -> 
          visit x lst
in visit e_init [];;

(* Generates a list of all variables in boolean expression nnf for optimized space usage *)

let boolean_expression_nnf_variables e_init = 
    let rec visit e lst = 
      match e with 
      | Variable_nnf x -> 
          if List.mem x lst 
          then lst 
          else x :: lst
      | Conjunction_nnf (e1, e2) -> 
          visit e1 (visit e2 lst)
      | Disjunction_nnf (e1, e2) -> 
          visit e1 (visit e2 lst)
      | Negation_nnf x -> 
          if List.mem x lst 
          then lst 
          else x :: lst
in visit e_init [];;

(* Generates a random boolean expression with variables "0", "1", "2"... "9" *)

let rec random_boolean_expression_generator () =
    match Random.int 6 with 
    | 0 -> Negation (random_boolean_expression_generator ())
    | 1 -> Conjunction (random_boolean_expression_generator (), random_boolean_expression_generator ())
    | 2 -> Disjunction (random_boolean_expression_generator (), random_boolean_expression_generator ())
    | _ -> Variable (string_of_int (Random.int 10));;


(* Part 1 *)  

(* Negational normal map for boolean expressions *)

let boolean_to_negational_normal (e_init : boolean_expression) : boolean_expression_nnf =
  let rec visit e = 
    match e with
    | Variable x -> (fun polarity -> 
          if polarity 
          then Variable_nnf x 
          else Negation_nnf x)
    | Conjunction (e1, e2) -> (fun polarity -> 
          if polarity
          then Conjunction_nnf (visit e1 polarity, visit e2 polarity)
          else Disjunction_nnf (visit e1 polarity, visit e2 polarity))
    | Disjunction (e1, e2) -> (fun polarity -> 
          if polarity
          then Disjunction_nnf (visit e1 polarity, visit e2 polarity)
          else Conjunction_nnf (visit e1 polarity, visit e2 polarity))
    | Negation e' -> (fun polarity -> 
          visit e' (not polarity))
  in visit e_init true;;


(* unit tests *)

let boolean_to_negational_normal_test candidate = 
   (candidate (Negation (Variable "x")) = 
              (Negation_nnf "x")) &&
   (candidate (Negation (Negation (Variable "x"))) = 
              (Variable_nnf "x")) &&
   (candidate (Negation (Conjunction (Variable "x", Variable "y"))) =
              Disjunction_nnf (Negation_nnf "x", Negation_nnf "y")) &&
   (candidate (Negation (Conjunction (Negation (Variable "x"), Variable "y"))) =
              Disjunction_nnf (Variable_nnf "x", Negation_nnf "y")) &&
   (candidate (Negation (Conjunction (Variable "x", Negation (Variable "y")))) =
              Disjunction_nnf (Negation_nnf "x", Variable_nnf "y")) &&
   (candidate (Negation (Conjunction (Negation (Variable "x"), Negation (Variable "y")))) =
              Disjunction_nnf (Variable_nnf "x", Variable_nnf "y")) &&
   (candidate (Negation (Disjunction (Variable "x", Variable "y"))) =
              Conjunction_nnf (Negation_nnf "x", Negation_nnf "y")) &&
   (candidate (Negation (Disjunction (Negation (Variable "x"), Variable "y"))) =
              Conjunction_nnf (Variable_nnf "x", Negation_nnf "y")) &&
   (candidate (Negation (Disjunction (Variable "x", Negation (Variable "y")))) =
              Conjunction_nnf (Negation_nnf "x", Variable_nnf "y")) &&
   (candidate (Negation (Disjunction (Negation (Variable "x"), Negation (Variable "y")))) =
              Conjunction_nnf (Variable_nnf "x", Variable_nnf "y"))
   (* etc *);;

let _ = assert(boolean_to_negational_normal_test boolean_to_negational_normal);;


(* Part 2 *)

(* Interpreter for boolean expressions *)

let rec boolean_expression_interpreter (e_init : boolean_expression) env_init = 
  let rec visit e env = 
    match e with
    | Variable x ->
          lookup x env
    | Conjunction (e1, e2) ->
          ((visit e1 env) && (visit e2 env))
    | Disjunction (e1, e2) ->
          ((visit e1 env) || (visit e2 env))
    | Negation e' -> 
          match e' with
          | Variable x ->
                not (lookup x env) 
          | Conjunction (e1, e2) ->
                not ((visit e1 env) && (visit e2 env))
          | Disjunction (e1, e2) ->
                not ((visit e1 env) || (visit e2 env))
          | Negation e'' ->
                visit e'' env
  in visit e_init env_init;;

(* Below tests tautologies *)

let boolean_expression_interpreter_test candidate = 
  (List.map (candidate (Disjunction (Variable "x", Negation (Variable "x")))) 
             (all_possible_Boolean_environments ["x"]) = [true; true]) &&
  (List.map (candidate (Disjunction (Negation (Variable "x"), Variable "x"))) 
             (all_possible_Boolean_environments ["x"]) = [true; true]) &&
  (List.map (candidate (Disjunction ((Disjunction (Variable "x", Negation (Variable "x"))), Negation (Variable "x"))))
             (all_possible_Boolean_environments ["x"]) = [true; true]) &&
  (List.map (candidate (Disjunction (Negation (Variable "x"), Disjunction (Variable "x", Negation (Variable "x")))))
             (all_possible_Boolean_environments ["x"]) = [true; true]) &&
  (List.map (candidate (Negation (Conjunction (Negation (Variable "x"), Variable "x")))) 
             (all_possible_Boolean_environments ["x"]) = [true; true]) &&
  (List.map (candidate (Negation (Conjunction (Variable "x", Negation (Variable "x"))))) 
             (all_possible_Boolean_environments ["x"]) = [true; true]) &&
  (List.map (candidate (Negation (Conjunction (Variable "x", Conjunction (Negation (Variable "x"), Variable "x")))))
             (all_possible_Boolean_environments ["x"]) = [true; true]) &&
  (List.map (candidate (Negation (Conjunction (Conjunction (Negation (Variable "x"), Variable "x"), Variable "x"))))
             (all_possible_Boolean_environments ["x"]) = [true; true])
  (* etc *);;

let _ = assert(boolean_expression_interpreter_test boolean_expression_interpreter);;


(* Interpreter for negational normalized boolean expressions *)

let boolean_expression_nnf_interpreter (e_init : boolean_expression_nnf) env_init : bool = 
  let rec visit e env = 
    match e with
    | Variable_nnf x ->
        lookup x env
    | Conjunction_nnf (e1, e2) ->
        ((visit e1 env) && (visit e2 env))
    | Disjunction_nnf (e1, e2) ->
        ((visit e1 env) || (visit e2 env))
    | Negation_nnf x -> 
        not (lookup x env)
  in visit e_init env_init;;

(* Testing tautologies *)

let boolean_expression_nnf_interpreter_test candidate = 
  (List.map (candidate (Disjunction_nnf (Variable_nnf "x", Negation_nnf "x")))
             (all_possible_Boolean_environments ["x"]) = [true; true]) &&
  (List.map (candidate (Disjunction_nnf (Negation_nnf "x", Variable_nnf "x")))
             (all_possible_Boolean_environments ["x"]) = [true; true]) &&
  (List.map (candidate (Disjunction_nnf (Disjunction_nnf (Variable_nnf "x", Negation_nnf "x"), Negation_nnf "x")))
             (all_possible_Boolean_environments ["x"]) = [true; true]) &&
  (List.map (candidate (Disjunction_nnf (Disjunction_nnf (Variable_nnf "x", Negation_nnf "x"), Negation_nnf "x")))
             (all_possible_Boolean_environments ["x"]) = [true; true]) &&
  (List.map (candidate (boolean_to_negational_normal (Negation (Conjunction (Negation (Variable "x"), Variable "x")))))
             (all_possible_Boolean_environments ["x"]) = [true; true]) &&
  (List.map (candidate (boolean_to_negational_normal (Negation (Conjunction (Variable "x", Negation (Variable "x"))))))
             (all_possible_Boolean_environments ["x"]) = [true; true]) &&
  (List.map (candidate (boolean_to_negational_normal (Negation (Conjunction (Variable "x", Conjunction (Negation (Variable "x"), Variable "x"))))))
             (all_possible_Boolean_environments ["x"]) = [true; true]) &&
  (List.map (candidate (boolean_to_negational_normal (Negation (Conjunction (Conjunction (Negation (Variable "x"), Variable "x"), Variable "x")))))
             (all_possible_Boolean_environments ["x"]) = [true; true]) 
  (* etc *);;

let _ = assert(boolean_expression_nnf_interpreter_test boolean_expression_nnf_interpreter);;


(* Verify that all for inputs the result of evaluating the source equals the result of evaluating the normalized form *)

let verify_all_possible_Boolean_environments (e : boolean_expression) =
  let variables = all_possible_Boolean_environments (boolean_expression_variables e)
  and normalized = boolean_to_negational_normal e
  in
  List.map (boolean_expression_interpreter e) variables 
  = 
  List.map (boolean_expression_nnf_interpreter normalized) variables;;


(* unit tests *)

let verify_all_possible_Boolean_environments_test candidate = 
   (candidate (Negation (Variable "x")) = 
              true) &&
   (candidate (Negation (Negation (Variable "x"))) = 
              true) &&
   (candidate (Negation (Conjunction (Variable "x", Variable "y"))) =
              true) &&
   (candidate (Negation (Conjunction (Negation (Variable "x"), Variable "y"))) =
              true) &&
   (candidate (Negation (Conjunction (Variable "x", Negation (Variable "y")))) =
              true) &&
   (candidate (Negation (Conjunction (Negation (Variable "x"), Negation (Variable "y")))) =
              true) &&
   (candidate (Negation (Disjunction (Variable "x", Variable "y"))) =
              true) &&
   (candidate (Negation (Disjunction (Negation (Variable "x"), Variable "y"))) =
              true) &&
   (candidate (Negation (Disjunction (Variable "x", Negation (Variable "y")))) =
              true) &&
   (candidate (Negation (Disjunction (Negation (Variable "x"), Negation (Variable "y")))) =
              true)
   (* etc *);;

let _ = assert(verify_all_possible_Boolean_environments_test verify_all_possible_Boolean_environments);;


(* Tests n different random boolean expressions if the normalized form evaulates the same for all possible inputs *)

let rec extensive_test n = 
    if n = 0 
    then verify_all_possible_Boolean_environments (random_boolean_expression_generator ())
    else 
    (verify_all_possible_Boolean_environments (random_boolean_expression_generator ())) && (extensive_test (n-1));;


let _ = assert(extensive_test 1 && extensive_test 10 && extensive_test 100 && extensive_test 1000);;


(* Fold right for boolean expressions *)

let fold_right_boolean_expression var con dis neg exp =
  let rec visit e =
    match e with
    | Variable x ->
       var x
    | Conjunction (e1, e2) ->
       let e1' = visit e1
       and e2' = visit e2
       in con (e1', e2')
    | Disjunction (e1, e2) ->
       let e1' = visit e1
       and e2' = visit e2
       in dis (e1', e2')
    | Negation e' -> 
       neg (visit e') 
in visit exp;;

(* Implementation *)

(* Maps boolean expressions to negational normal form using fold right *)

let fold_right_boolean_to_negational_normal (e_init : boolean_expression) : boolean_expression_nnf = 
  fold_right_boolean_expression 
  (fun x -> (fun polarity -> 
     if polarity 
     then Variable_nnf x 
     else Negation_nnf x))
  (fun (c1, c2) -> (fun polarity -> 
     if polarity
     then Conjunction_nnf (c1 polarity, c2 polarity)
     else Disjunction_nnf (c1 polarity, c2 polarity)))
  (fun (c1, c2) -> (fun polarity -> 
     if polarity
     then Disjunction_nnf (c1 polarity, c2 polarity)
     else Conjunction_nnf (c1 polarity, c2 polarity)))
  (fun c -> (fun polarity -> 
     c (not polarity)))
  e_init 
  true;;

let _ = assert(boolean_to_negational_normal_test fold_right_boolean_to_negational_normal);;

(* Interpreter for boolean expressions using fold right *)

let fold_right_boolean_expression_interpreter (e: boolean_expression) env: bool = 
  fold_right_boolean_expression 
  (fun x -> lookup x env)
  (fun (c1, c2) -> c1 && c2) 
  (fun (d1, d2) -> d1 || d2) 
  (fun x -> not x) 
  e;;

let _ = assert(boolean_expression_interpreter_test boolean_expression_interpreter);;


(* Fold right for nnf expressions *)

let fold_right_boolean_expression_nnf var con dis neg exp = 
  let rec visit e = 
  match e with
  | Variable_nnf x ->
     var x
  | Conjunction_nnf (e1, e2) ->
     let e1' = visit e1
     and e2' = visit e2
     in con (e1', e2')
  | Disjunction_nnf (e1, e2) ->
     let e1' = visit e1
     and e2' = visit e2
     in dis (e1', e2')
  | Negation_nnf x ->
     neg x
in visit exp;;

(* Implementation *)

(* Interpreter for boolean expression in negational normal form using fold right *)

let fold_right_boolean_expression_nnf_interpreter (e : boolean_expression_nnf) env : bool =
  fold_right_boolean_expression_nnf 
  (fun x -> lookup x env)
  (fun (c1, c2) -> c1 && c2) 
  (fun (d1, d2) -> d1 || d2) 
  (fun x -> not (lookup x env)) 
  e;;

let _ = assert(boolean_expression_nnf_interpreter_test fold_right_boolean_expression_nnf_interpreter);;


(* end of document *)
