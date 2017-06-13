(* week-15_Magritte-and-the-40-optimizing-compilers.ml *)
(* YSC2204 -- introduction to data structures and algorithms *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Version of 16 Apr 2017 *)

(* ********** *)

(*
   Your name: Brian Bohme
   Your e-mail address: brianbohme@u.yale-nus.edu.sg
 *)

(* ********** *)

(* Task: Complete at least 7 of the 8 exercises *)

exception Negative_integer of int;;

exception Stop_everything;;

(* ******************** *)

module type SOURCE_SYNTAX =
  sig
    type arithmetic_expression =
      Literal of int
    | Plus of arithmetic_expression * arithmetic_expression
    | Times of arithmetic_expression * arithmetic_expression

    val show : arithmetic_expression -> string
  end;;

module Source_syntax : SOURCE_SYNTAX =
  struct
    type arithmetic_expression =
      Literal of int
    | Plus of arithmetic_expression * arithmetic_expression
    | Times of arithmetic_expression * arithmetic_expression

    let show e_init =
     (* show : arithmetic_expression -> string *)
      let rec traverse e =
        match e with
          Literal n ->
           if n < 0
           then raise (Negative_integer n)
           else string_of_int n
        | Plus (e1, e2) ->
           "(" ^ (traverse e1) ^ " + " ^ (traverse e2) ^ ")"
        | Times (e1, e2) ->
           "(" ^ (traverse e1) ^ " * " ^ (traverse e2) ^ ")"
      in traverse e_init
  end;;

(* ********** *)

module type SOURCE_INTERPRETER =
  sig
    val interpret_to_string : Source_syntax.arithmetic_expression -> string
    val interpret_to_source : Source_syntax.arithmetic_expression -> Source_syntax.arithmetic_expression
  end;;

(* ********** *)

module Source_sample =
  struct
    open Source_syntax

    let e0 = Literal 0

    let e3 = Plus (Literal 1,
                   Literal 2)

    let e6 = Times (Literal 2,
                    Literal 3)

    let e120 = Times (Times (Literal 2,
                             Literal 3),
                      Times (Literal 4,
                             Literal 5))

    let e0' = Times (Times (Times (Literal 0,
                                   Literal 1),
                            Times (Literal 2,
                                   Literal 3)),
                     Times (Literal 4,
                            Literal 5))

    let e6' = Plus (Plus (Literal 1, 
                          Literal 2), 
                    Literal 3)

    let e6'' = Plus (Plus (Literal 0,
                           Literal 1),
                     Plus (Literal 2,
                           Literal 3))

    let e71 = Plus (Plus (Times (Plus (Plus (Literal 1, 
                                           Literal 2),
                                     Literal 3), 
                               Literal 4),
                        Literal 5), 
                  Times (Literal 6, 
                         Literal 7));;

    let e0'' = Times (Literal 0, e71)
    

    (* etc., of course *)
  end;;

(*
   # Source_syntax.show Source_sample.e0;;
   - : string = "0"
   # Source_syntax.show Source_sample.e3;;
   - : string = "(1 + 2)"
   # Source_syntax.show Source_sample.e6;;
   - : string = "(2 * 3)"
   # Source_syntax.show Source_sample.e120;;
   - : string = "((2 * 3) * (4 * 5))"
   # Source_syntax.show Source_sample.e0';;
   - : string = "(((0 * 1) * (2 * 3)) * (4 * 5))"
   # 
*)

module Test_Source_interpreter_maker (Source_interpreter : SOURCE_INTERPRETER) : sig end =
  struct
    let test e n =
      let to_string = (Source_interpreter.interpret_to_string e = string_of_int n)
                      ||
                      (Printf.printf "fail: string-interpreting Source_sample.%s\n" (Source_syntax.show e);
                       false)
      and to_source = (Source_interpreter.interpret_to_source e = Source_syntax.Literal n)
                      ||
                      (Printf.printf "fail: source-interpreting Source_sample.%s\n" (Source_syntax.show e);
                       false)
      in to_string && to_source

    open Source_sample

    let z = true

    let z = test e0 0 && z

    let z = test e3 3 && z

    let z = test e6 6 && z

    let z = test e120 120 && z

    let z = test e0' 0 && z

    let z = test e6' 6 && z

    let z = test e6'' 6 && z

    let z = test e71 71 && z

    let z = test e0'' 0 && z

    (* etc., of course *)

    let _ = z || raise Stop_everything
  end;;

(*
   # module Source_interpreter_bogus : SOURCE_INTERPRETER = struct let interpret_to_string _ = "42" let interpret_to_source _ = Source_syntax.Literal 42 end;;
   module Source_interpreter_bogus : SOURCE_INTERPRETER
   # module Test_Source_interpreter_bogus = Test_Source_interpreter_maker (Source_interpreter_bogus);;
   fail: string-interpreting Source_sample.0
   fail: source-interpreting Source_sample.0
   fail: string-interpreting Source_sample.(1 + 2)
   fail: source-interpreting Source_sample.(1 + 2)
   fail: string-interpreting Source_sample.(2 * 3)
   fail: source-interpreting Source_sample.(2 * 3)
   fail: string-interpreting Source_sample.((2 * 3) * (4 * 5))
   fail: source-interpreting Source_sample.((2 * 3) * (4 * 5))
   fail: string-interpreting Source_sample.(((0 * 1) * (2 * 3)) * (4 * 5))
   fail: source-interpreting Source_sample.(((0 * 1) * (2 * 3)) * (4 * 5))
   Exception: Stop_everything.
   # 
*)

(* ********** *)

module type NATURAL_NUMBER =
  sig
    type nat
    val quote : int -> nat
    val plus : nat * nat -> nat
    val times : nat * nat -> nat
    val string_of_nat : nat -> string
    val syntax_of_nat : nat -> Source_syntax.arithmetic_expression
  end;;

(* ***** *)

module Natural_number_semantics : NATURAL_NUMBER =
  struct
    type nat = int

    let quote n =
      n

    let plus (n1, n2) =
      n1 + n2

    let times (n1, n2) =
      n1 * n2

    let string_of_nat n
      = string_of_int n

    let syntax_of_nat n =
      Source_syntax.Literal n
  end;;

(* ********** *)

module Source_interpreter_maker (Natural_number : NATURAL_NUMBER) : SOURCE_INTERPRETER =
  struct
    let rec evaluate e =
      (* evaluate : Source_syntax.arithmetic_expression -> Natural_number.nat *)
      match e with
        Source_syntax.Literal n ->
         if n < 0
         then raise (Negative_integer n)
         else Natural_number.quote n
      | Source_syntax.Plus (e1, e2) ->
         Natural_number.plus (evaluate e1, evaluate e2)
      | Source_syntax.Times (e1, e2) ->
         Natural_number.times (evaluate e1, evaluate e2)

    let interpret_to_string e
      = Natural_number.string_of_nat (evaluate e)

    let interpret_to_source e
      = Natural_number.syntax_of_nat (evaluate e)
  end;;

(* ***** *)

module Source_interpreter_standard : SOURCE_INTERPRETER =
  Source_interpreter_maker (Natural_number_semantics);;

module Test_Source_interpreter_standard =
  Test_Source_interpreter_maker (Source_interpreter_standard);;

(* And indeed:

   # Source_interpreter_standard.interpret_to_string Source_sample.e0;;
   - : string = "0"
   # Source_interpreter_standard.interpret_to_source Source_sample.e0;;
   - : Source_syntax.arithmetic_expression = Source_syntax.Literal 0
   # Source_interpreter_standard.interpret_to_string Source_sample.e120;;
   - : string = "120"
   # Source_interpreter_standard.interpret_to_source Source_sample.e120;;
   - : Source_syntax.arithmetic_expression = Source_syntax.Literal 120
   # 
*)

(* ******************** *)

module type TARGET_SYNTAX =
  sig
    type byte_code_instruction = Push of int | Add | Mul

    type byte_code_program = Pro of byte_code_instruction list
  end

module Target_syntax : TARGET_SYNTAX =
  struct
    type byte_code_instruction = Push of int | Add | Mul

    type byte_code_program = Pro of byte_code_instruction list
  end;;

(* ********** *)

exception Bad_byte_code_program of string;;

module type TARGET_INTERPRETER =
  sig
    val interpret_to_string : Target_syntax.byte_code_program -> string
    val interpret_to_source : Target_syntax.byte_code_program -> Source_syntax.arithmetic_expression
  end;;

(* ********** *)

module Target_interpreter_maker (Natural_number : NATURAL_NUMBER) : TARGET_INTERPRETER =
  struct
    type data_stack = Natural_number.nat list

    type result_of_execution =
      OK of data_stack
    | KO of string

    let decode_execute bci ds =
      (* decode_execute : Target_syntax.byte_code_instruction -> data_stack -> result_of_execution *)
      match bci with
        Target_syntax.Push n ->
         OK (Natural_number.quote n :: ds)
      | Target_syntax.Add ->
         (match ds with
            [] ->
             KO "stack underflow for Add"
          | d2 :: [] ->
             KO "stack underflow for Add"
          | d2 :: d1 :: ds' ->
             OK (Natural_number.plus (d1, d2) :: ds'))
      | Target_syntax.Mul ->
         (match ds with
            [] ->
             KO "stack underflow for Mul"
          | d2 :: [] ->
             KO "stack underflow for Mul"
          | d2 :: d1 :: ds' ->
             OK (Natural_number.times (d1, d2) :: ds'))

    let run (Target_syntax.Pro bcis_init) =
     (* run : Target_syntax.byte_code_program -> string *)
      let rec fetch_decode_execute bcis ds =
           (* fetch_decode_execute : Target_syntax.byte_code_instruction list -> data_stack -> result_of_execution *)
        match bcis with
          [] ->
           OK ds
        | bci :: bcis' ->
           (match decode_execute bci ds with
              OK ds' ->
               fetch_decode_execute bcis' ds'
            | KO s ->
               KO s)
      in fetch_decode_execute bcis_init []

    let interpret_to_string p =
      match run p with
        OK [] ->
         raise (Bad_byte_code_program "stack underflow at the end")
      | OK (n :: []) ->
         Natural_number.string_of_nat n
      | OK (d :: _ :: _) ->
         raise (Bad_byte_code_program "stack overflow at the end")
      | KO s ->
         raise (Bad_byte_code_program s)

    let interpret_to_source p =
      match run p with
        OK [] ->
         raise (Bad_byte_code_program "stack underflow at the end")
      | OK (n :: []) ->
         Natural_number.syntax_of_nat n
      | OK (d :: _ :: _) ->
         raise (Bad_byte_code_program "stack overflow at the end")
      | KO s ->
         raise (Bad_byte_code_program s)
  end;;

(* ***** *)

module Target_interpreter_standard : TARGET_INTERPRETER =
  Target_interpreter_maker (Natural_number_semantics);;

(* ******************** *)

module type COMPILER =
  sig
    val compile : Source_syntax.arithmetic_expression -> Target_syntax.byte_code_program
  end

module Compiler : COMPILER =
  struct
    let compile e_init =
     (* compile : arithmetic_expression -> result_of_compilation *)
      let rec flatten e =
           (* flatten : arithmetic_expression -> Target_syntax.byte_code_instruction list *)
        match e with
          Source_syntax.Literal n ->
           if n < 0
           then raise (Negative_integer n)
           else [Target_syntax.Push n]
        | Source_syntax.Plus (e1, e2) ->
           flatten e1 @ flatten e2 @ [Target_syntax.Add]
        | Source_syntax.Times (e1, e2) ->
           flatten e1 @ flatten e2 @ [Target_syntax.Mul]
      in Target_syntax.Pro (flatten e_init)
  end;;

(* Task 1:

   Write an accumulator-based version of this compiler.
   Is your accumulator-based version more efficient than this append-based version?
*)

(* ******** *)

let fold_right_arithmetic_expressions lit add mul e_init  = 
    let rec visit e = 
      match e with 
      | Source_syntax.Literal n -> lit e 
      | Source_syntax.Plus (e1, e2) -> add (visit e1) (visit e2)
      | Source_syntax.Times (e1, e2) -> mul (visit e1) (visit e2)
    in visit e_init;;

(* ********** *)

(*

module Compiler_acc : COMPILER =
  struct
    let compile e_init =
     (* compile : arithmetic_expression -> result_of_compilation *)
      let rec flatten e a =
        match e with 
        | Source_syntax.Literal n -> 
          	if n < 0 
          	then raise (Negative_integer n)
          	else ((Target_syntax.Push n)::a)
        | Source_syntax.Plus (e1, e2) ->
           flatten e1 (flatten e2 (Target_syntax.Add::a))
        | Source_syntax.Times (e1, e2) ->
           flatten e1 (flatten e2 (Target_syntax.Mul::a))
      in Target_syntax.Pro (flatten e_init [])
  end;;

 *)

module Compiler_acc : COMPILER =
  struct
    let compile e_init =
     (* compile : arithmetic_expression -> result_of_compilation *)
      let rec flatten e =
        match e with 
        | Source_syntax.Literal n -> (fun acc -> 
          	if n < 0 
          	then raise (Negative_integer n)
          	else ((Target_syntax.Push n)::acc))
        | Source_syntax.Plus (e1, e2) -> (fun acc -> 
           flatten e1 (flatten e2 (Target_syntax.Add::acc)))
        | Source_syntax.Times (e1, e2) -> (fun acc -> 
           flatten e1 (flatten e2 (Target_syntax.Mul::acc)))
      in Target_syntax.Pro (flatten e_init [])
  end;;


(* Just for fun!! *)

let compiler_acc_using_fold_right e_init = 
	Target_syntax.Pro 
	((fold_right_arithmetic_expressions
	 (fun e -> (fun acc -> 
	 	  (match e with 
	 	  | Source_syntax.Literal n -> 
	 	  	if n < 0 
          	then raise (Negative_integer n)
          	else ((Target_syntax.Push n)::acc)
	 	  | _ -> 
	 	  	raise Stop_everything)))
	 (fun e1 e2 -> 
	 		(fun acc -> 
	 			e1 (e2 (Target_syntax.Add::acc))))
	 (fun e1 e2 -> 
	 		(fun acc -> 
	 			e1 (e2 (Target_syntax.Mul::acc)))) 
	 e_init )
	 [] );;


let random_arithmetic_expression_generator n_init = 
  let rec safety n = 
    if n = 0 then Source_syntax.Literal (Random.int 50) else
      match Random.int 4 with
      | 0 -> Source_syntax.Plus (safety (n - 1), safety (n - 1))
      | 1 -> Source_syntax.Times (safety (n - 1), safety (n - 1))
      | _ -> Source_syntax.Literal (Random.int 50)
    in if n_init < 0 
	   then raise (Negative_integer n_init)
	   else safety n_init;;
    

let test_list_of_arithmetic_expressions n_init depth =
  let rec generator n acc = 
    if n = 0 
    then acc
    else 
    generator (n-1) ((random_arithmetic_expression_generator depth)::acc)
  in if n_init < 0 
	 then raise (Negative_integer n_init)
	 else generator n_init [];;


let test_of_equality n_init acc =
	let rec visit n = 
	let e = 
		random_arithmetic_expression_generator 200 
	in 
	let test = 
		(Compiler.compile e = acc e)
	in 
	if n = 0 then test
	else test && visit (n-1)
	in visit n_init;;

let _ = assert (test_of_equality 10000 Compiler_acc.compile)

let _ = assert (test_of_equality 10000 compiler_acc_using_fold_right)

(*
let _ = assert (andmap 
			   (fun e -> Compiler.compile e = Compiler_acc.compile e)
               (test_list_of_arithmetic_expressions 10000 200));;


let _ = assert (andmap 
			   (fun e -> Compiler.compile e = compiler_acc_using_fold_right e)
               (test_list_of_arithmetic_expressions 10000 200));;
*)

let test_of_speed compiler =
    let start = Unix.gettimeofday ()
    in let _result = List.map (fun l -> compiler l) (test_list_of_arithmetic_expressions 10000 200)
    in let stop = Unix.gettimeofday ()
    in Printf.printf "Done!\nExecution time: %fs\n%!" (stop -. start);;

test_of_speed Compiler_acc.compile;; 

test_of_speed Compiler.compile;;

(* The accumulator version is able to complete the task in under a second, while the non-accumulating compiler 
takes between 5-9 seconds long. With n = 500 or greater on the random_arithmetic_expression_generator, the 
accumulator version is able to complete the task very quickly while the normal compiler overflows, where n is 
the maximal depth of one sub-arithmetic expression *)

(* ********** *)

module Commutation_test_maker (Source_interpreter : SOURCE_INTERPRETER)
                              (Compiler : COMPILER)
                              (Target_interpreter : TARGET_INTERPRETER) : sig end =
  struct
    let test e =
      ((Source_interpreter.interpret_to_string e
        = Target_interpreter.interpret_to_string (Compiler.compile e))
       ||
       (Printf.printf "fail: string-commuting Source_sample.%s\n" (Source_syntax.show e);
        false))
      &&
      ((Source_interpreter.interpret_to_source e
        = Target_interpreter.interpret_to_source (Compiler.compile e))
       ||
       (Printf.printf "fail: source-commuting Source_sample.%s\n" (Source_syntax.show e);
        false))

    open Source_sample

    let z = true

    let z = test e0 && z

    let z = test e3 && z

    let z = test e6 && z

    let z = test e120 && z

    let z = test e0' && z

    let z = test e6' && z

    let z = test e6'' && z

    let z = test e71 && z

    let z = test e0' && z

    (* etc., of course *)

    let _ = z || raise Stop_everything
  end;;

(* ***** *)

(* Task 2:

   Verify that the diagram commutes.
*)

module Commutation_test_standard =
  Commutation_test_maker (Source_interpreter_standard)
                         (Compiler)
                         (Target_interpreter_standard);;


module Commutation_test_standard =
  Commutation_test_maker (Source_interpreter_standard)
                         (Compiler_acc)
                         (Target_interpreter_standard);;


(* ***** *)

module type JUST_IN_TIME =
  sig
    val compile_and_run_to_string : Source_syntax.arithmetic_expression -> string
    val compile_and_run_to_source : Source_syntax.arithmetic_expression -> Source_syntax.arithmetic_expression
  end;;

module Just_in_time_maker (Compiler : COMPILER)
                          (Target_interpreter : TARGET_INTERPRETER) : JUST_IN_TIME =
  struct
    let compile_and_run_to_string e =
      Target_interpreter.interpret_to_string (Compiler.compile e)

    let compile_and_run_to_source e =
      Target_interpreter.interpret_to_source (Compiler.compile e)
  end;;

module Just_in_time_standard =
  Just_in_time_maker (Compiler)
                     (Target_interpreter_standard);;

(* And indeed:

   # Just_in_time_standard.compile_and_run_to_string Source_sample.e0;;
   - : string = "0"
   # Just_in_time_standard.compile_and_run_to_string Source_sample.e3;;
   - : string = "3"
   # Just_in_time_standard.compile_and_run_to_string Source_sample.e6;;
   - : string = "6"
   # Just_in_time_standard.compile_and_run_to_string Source_sample.e120;;
   - : string = "120"
   # Just_in_time_standard.compile_and_run_to_string Source_sample.e0';;
   - : string = "0"
   # 
*)

(* ******************** *)

(* Magritte here we come:
   this is not a number,
   this is the (syntactic) representation of a number.
 *)

(* ***** *)

module Natural_number_syntax : NATURAL_NUMBER =
  struct
    open Source_syntax

    type nat = arithmetic_expression

    let quote n =
      Literal n

    let plus (e1, e2) =
      Plus (e1, e2)

    let times (e1, e2) =
      Times (e1, e2)

    let string_of_nat e =
      show e

    let syntax_of_nat e =
      e
  end;;

(* ***** *)

module Source_interpreter_Magritte : SOURCE_INTERPRETER =
  Source_interpreter_maker (Natural_number_syntax);;

module Target_interpreter_Magritte : TARGET_INTERPRETER =
  Target_interpreter_maker (Natural_number_syntax);;

(* ***** *)

(* Task 3:

   Verify that the diagram commutes.
*)

module Commutation_test_Magritte =
  Commutation_test_maker (Source_interpreter_Magritte)
                         (Compiler)
                         (Target_interpreter_Magritte);;

module Commutation_test_Magritte =
  Commutation_test_maker (Source_interpreter_Magritte)
                         (Compiler_acc)
                         (Target_interpreter_Magritte);;

(* ***** *)

module Just_in_time_Magritte =
  Just_in_time_maker (Compiler)
                     (Target_interpreter_Magritte);;


(* *********** *)


module type DECOMPILER =
  sig
    val decompile : Target_syntax.byte_code_program -> Source_syntax.arithmetic_expression
  end;;

(* ***** *)

module Test_decompiler_maker (Decompiler : DECOMPILER) : sig end =
  struct
    let test e =
      (Decompiler.decompile (Compiler.compile e) = e)
      ||
      (Printf.printf "fail: decompiling Source_sample.%s\n" (Source_syntax.show e);
       false)

    open Source_sample

    let z = true

    let z = test e0 && z

    let z = test e3 && z

    let z = test e6 && z

    let z = test e120 && z

    let z = test e0' && z

    let z = test e6' && z

    let z = test e6'' && z

    let z = test e71 && z

    let z = test e0' && z

    (* etc., of course *)

    let _ = z || raise Stop_everything
  end;;

(* ***** *)

(* Task 4:

   Implement a decompiler.
*)


module Decompiler : DECOMPILER =
  struct
  

let decompile e_init = 
    let rec visit e e_acc = 
      match e with 
      | [] -> 
      		  (match e_acc with 
               | e_sub_decompiled::stack -> 
              	 	e_sub_decompiled
               | _ -> 
              	 	raise Stop_everything)
      | e_sub::queue -> 
      			(match e_sub with 
                 | Target_syntax.Push n -> 
                   		visit queue ((Source_syntax.Literal n) :: e_acc)
                 | Target_syntax.Add -> 
                   		(match e_acc with 
                   		 | e_sub_decompiled::(next_decompiled_sub::stack) -> 
                   	  		 visit queue ((Source_syntax.Plus (next_decompiled_sub, e_sub_decompiled)) :: stack)
                    	 | _ -> 
                    	 	 raise Stop_everything)
                | Target_syntax.Mul -> 
                		(match e_acc with 
                         | e_sub_decompiled::(next_decompiled_sub::stack) -> 
                         	 visit queue ((Source_syntax.Times (next_decompiled_sub, e_sub_decompiled)) :: stack)
                         | _ -> 
                          	 raise Stop_everything))
    in match e_init with 
      | Target_syntax.Pro instructions -> 
      			visit instructions []

  end;;


module Test_decompiler =
  Test_decompiler_maker (Decompiler);;


let rec test_compile_then_decompiled n = 
    let expression = random_arithmetic_expression_generator 50 in
    let test = (Decompiler.decompile (Compiler.compile expression) = expression) in
    if n = 0 then test
    else 
    test && test_compile_then_decompiled (n-1);;


let _ = assert(test_compile_then_decompiled 50);;

(* ********** *)

module type SOURCE_TO_SOURCE =
  sig
    val transform : Source_syntax.arithmetic_expression -> Source_syntax.arithmetic_expression
  end;;

module Test_transformer_maker (Transformer : SOURCE_TO_SOURCE)
                              (Compiler : COMPILER)
                              (Decompiler : DECOMPILER) : sig end =
  struct
    let test e =
      (Transformer.transform e
       = Decompiler.decompile (Compiler.compile e))
      ||
      (Printf.printf "fail: transforming Source_sample.%s\n" (Source_syntax.show e);
       false)

    open Source_sample

    let z = true

    let z = test e0 && z

    let z = test e3 && z

    let z = test e6 && z

    let z = test e120 && z

    let z = test e0' && z

    let z = test e6' && z

    let z = test e6'' && z

    let z = test e71 && z

    let z = test e0' && z

    (* etc., of course *)

    let _ = z || raise Stop_everything
  end;;

(* ***** *)

module Test_idempotence_maker (Transformer : SOURCE_TO_SOURCE) : sig end =
  struct
    let test e =
      let e' = Transformer.transform e
      in (e' = Transformer.transform e')
         ||
         (Printf.printf "fail: idempotence Source_sample.%s\n" (Source_syntax.show e);
          false)

    open Source_sample

    let z = true

    let z = test e0 && z

    let z = test e3 && z

    let z = test e6 && z

    let z = test e120 && z

    let z = test e0' && z

    let z = test e6' && z

    let z = test e6'' && z

    let z = test e71 && z

    let z = test e0' && z

    (* etc., of course *)

    let _ = z || raise Stop_everything
  end;;

(* ***** *)

module Source_to_source : SOURCE_TO_SOURCE =
  struct

    let transform e = 
    e
    
  end;;

(* Task 5:

   Verify that the identity source-to-source transformation
   makes the above diagram commute.

   Verify that the identity source-to-source transformation
   is idempotent.
*)

module Test_transformer =
  Test_transformer_maker (Source_to_source)
                         (Compiler)
                         (Decompiler);;


module Test_idempotence =
  Test_idempotence_maker (Source_to_source);;


(* ********** *)

module Compiler_surprising : COMPILER =
  struct
    let compile e_init =
     (* compile : arithmetic_expression -> result_of_compilation *)
      let rec visit e k0 k1 k =
        match e with
          Source_syntax.Literal n ->
           if n < 0
           then raise (Negative_integer n)
           else (match n with
                   0 ->
                   k0 ()
                 | 1 ->
                    k1 ()
                 | _ ->
                    k [Target_syntax.Push n])
        | Source_syntax.Plus (e1, e2) ->
           visit e1
                 (fun () ->
                   visit e2
                         k0
                         k1
                         k)
                 (fun () ->
                   visit e2
                         k1
                         (fun () ->
                           k [Target_syntax.Push 1; Target_syntax.Push 1; Target_syntax.Add])
                         (fun bcis2 ->
                           hammer bcis2 [Target_syntax.Add] (fun bcis ->
                                                             k (Target_syntax.Push 1 :: bcis))))
                 (fun bcis1 ->
                   visit e2
                         (fun () ->
                           k bcis1)
                         (fun () ->
                           hammer bcis1 [Target_syntax.Push 1; Target_syntax.Add] k)
                         (fun bcis2 ->
                           weld bcis1 bcis2 [Target_syntax.Add] k))
        | Source_syntax.Times (e1, e2) ->
           visit e1
                 k0
                 (fun () ->
                   visit e2
                         k0
                         k1
                         k)
                 (fun bcis1 ->
                   visit e2
                         k0
                         (fun () ->
                           k bcis1)
                         (fun bcis2 ->
                           weld bcis1 bcis2 [Target_syntax.Mul] k))
        and hammer xs ys k =
          match xs with
            [] ->
             k ys
          | x :: xs' ->
             hammer xs' ys (fun zs ->
                             k (x :: zs))
        and weld xs ys zs k =
          hammer ys zs (fun ws ->
                         hammer xs ws k)
      in Target_syntax.Pro (visit e_init
                                  (fun () ->
                                    [Target_syntax.Push 0])
                                  (fun () ->
                                    [Target_syntax.Push 1])
                                  (fun bcis ->
                                    bcis))
  end;;

(* ***** *)

(* Task 6a:

   Verify that the diagram commutes.
*)

module Commutation_test_standard_surprising =
  Commutation_test_maker (Source_interpreter_standard)
                         (Compiler_surprising)
                         (Target_interpreter_standard);;


(* ***** *)

module Just_in_time_Magritte_surprising =
  Just_in_time_maker (Compiler_surprising)
                     (Target_interpreter_Magritte);;

(* ***** *)

(* Task 6b:

   Implement the corresponding source-to-source transformation.
*)


module Source_to_source_surprising : SOURCE_TO_SOURCE =
  struct
    open Source_syntax

    let transform e_init =
      let rec normalize e =
        match e with 
        | Literal n -> 
        	  e
        | Plus (e1, e2) -> 
        	 (match (normalize e1), (normalize e2) with 
              | Literal 0, e2' -> 
              		e2'
              | e1', Literal 0 -> 
              		e1' 
              | e1', e2' -> 
              		Plus (e1', e2'))
        | Times (e1, e2) -> 
        	 (match (normalize e1), (normalize e2) with 
              | Literal 0, _ -> 
              		Literal 0
              | _ , Literal 0 -> 
              		Literal 0 
              | e1', Literal 1 -> 
              		e1'
              | Literal 1, e2' -> 
              		e2'
              | e1', e2' -> 
              		Times (e1', e2'))
     in normalize e_init

  end;;


(* ***** *)

(* Task 6c:

   Verify that the diagram commutes.
*)


module Test_transformer_surprising =
  Test_transformer_maker (Source_to_source_surprising)
                         (Compiler_surprising)
                         (Decompiler);;


let rec test_compile_then_decompiled_surprising n = 
    let expression = random_arithmetic_expression_generator 5 in
    let test = 
   		 (Decompiler.decompile (Compiler_surprising.compile expression) 
		 = Source_to_source_surprising.transform expression) 
   	in
    if n = 0 then test
    else 
    test && test_compile_then_decompiled (n-1);;


let _ = assert(test_compile_then_decompiled_surprising 50);;



(* ***** *)

(* Task 6d:

   Verify whether the transformer is idempotent.
*)

module Test_idempotence_surprising =
  Test_idempotence_maker (Source_to_source_surprising);;


(* Task 6f: 

    Implement a fold-right function for arithmetic expressions and express the source-to-source transformation using it *)


let transformer_surprising_using_fold_right e_init = 
    fold_right_arithmetic_expressions 
    (fun n -> n)
    (fun e1 e2 -> (match e1, e2 with 
                   | Source_syntax.Literal 0, e2' -> e2'
                   | e1', Source_syntax.Literal 0 -> e1' 
                   | e1', e2' -> Source_syntax.Plus (e1', e2'))
    )
    (fun e1 e2 -> (match e1, e2 with 
                   | Source_syntax.Literal 0, _ -> Source_syntax.Literal 0
                   | _ , Source_syntax.Literal 0 -> Source_syntax.Literal 0 
                   | e1', Source_syntax.Literal 1 -> e1'
                   | Source_syntax.Literal 1, e2' -> e2'
                   | e1', e2' -> Source_syntax.Times (e1', e2'))
    )
	e_init;;


let rec test_transformer_surprising_using_fold_right n = 
    let expression = random_arithmetic_expression_generator 50 in
    let test = 
    	(Decompiler.decompile (Compiler_surprising.compile expression) 
    	 = transformer_surprising_using_fold_right expression) 
    in
    if n = 0 then test
    else 
    test && test_transformer_surprising_using_fold_right (n-1);;


let _ = assert(test_transformer_surprising_using_fold_right 50);;


(* Task 7e:

  Characterize its optimization in English precisely and concisely
*)

(*

Simplifies additions by 0 and multiplication by 1 or 0. Multiplication by 0 becomes Literal 0 and 
addition/multiplication by 0/1 respectively is just the other multiplier. In mathematical terms, anything 
times 0 is 0; anything times 1 or plus 0 is itself. Because of the associativity of multiplication and 
addition, mathematically this is true whether the 0 or 1 is on the right or the left of the operand, and
the compiler reflects this.

*)

(* Task 7e:

  Implement a syntax checker that verifies whether a given arithmetic expression is in normal form, 
and use it to test your source-to-source transformer
*)

let syntax_checker_surprising e_init = 
    let rec check e = 
      match e with 
      | Source_syntax.Literal n -> 
      		 true
      | Source_syntax.Plus (e1, e2) -> 
      		(match e1, e2 with 
             | Source_syntax.Literal 0, _ -> false
             | _, Source_syntax.Literal 0 -> false
             | _, _ -> check e1 && check e2)
      | Source_syntax.Times (e1, e2) -> 
      		(match e1, e2 with 
             | Source_syntax.Literal 0, _ -> false
             | _, Source_syntax.Literal 0 -> false
             | Source_syntax.Literal 1, _ -> false
             | _, Source_syntax.Literal 1 -> false 
             | _, _ -> check e1 && check e2)
    in check e_init;;

let syntax_checker_surprising_2 e_init = 
    let rec check e = 
      match e with 
      | Source_syntax.Literal n -> 
      		 e 
      | Source_syntax.Plus (e1, e2) -> 
      		(match check e1, check e2 with 
             | Source_syntax.Literal 0, _ -> Source_syntax.Literal 0
             | _, Source_syntax.Literal 0 -> Source_syntax.Literal 0
             | _, _ -> e_init)
      | Source_syntax.Times (e1, e2) -> 
      		(match check e1, check e2 with 
             | Source_syntax.Literal 0, _ -> Source_syntax.Literal 0 
             | _, Source_syntax.Literal 0 -> Source_syntax.Literal 0
             | Source_syntax.Literal 1, _ -> Source_syntax.Literal 0
             | _, Source_syntax.Literal 1 -> Source_syntax.Literal 0
             | _, _ -> e_init)
    in if check e_init = e_init 
       then true 
	   else false;;


let rec verify_syntax_checker_surprising n = 
    let expression = random_arithmetic_expression_generator 50 in
    let test = (syntax_checker_surprising (Source_to_source_surprising.transform expression) = true) in
    if n = 0 then test
    else 
    test && verify_syntax_checker_surprising (n-1);;


let rec verify_syntax_checker_surprising_2 n = 
    let expression = random_arithmetic_expression_generator 50 in
    let test = (syntax_checker_surprising_2 (Source_to_source_surprising.transform expression) = true) in
    if n = 0 then test
    else 
    test && verify_syntax_checker_surprising (n-1);;


let _ = assert(verify_syntax_checker_surprising 50);;


let _ = assert(verify_syntax_checker_surprising_2 50);;


(* Task 7e:

  Compare the relative efficiencies of
    -interpreting with your optimizing source-to-source transformer and 
    -compiling and decompiling.

*)

let test_of_compile_then_decompile_speed compiler =
    let start = Unix.gettimeofday ()
    in let _result = List.map (fun l -> Decompiler.decompile (compiler l)) (test_list_of_arithmetic_expressions 10000 200)
    in let stop = Unix.gettimeofday ()
    in Printf.printf "Done!\nExecution time: %fs\n%!" (stop -. start);;


let test_of_transformer_speed transformer =
    let start = Unix.gettimeofday ()
    in let _result = List.map (fun l -> transformer l) (test_list_of_arithmetic_expressions 10000 200)
    in let stop = Unix.gettimeofday ()
    in Printf.printf "Done!\nExecution time: %fs\n%!" (stop -. start);;


test_of_compile_then_decompile_speed Compiler_surprising.compile;; 

test_of_transformer_speed Source_to_source_surprising.transform;;


(* Source to source transformer is almost 30 times faster than compiling and decompiling in this test! *)



(* ********** *)

module Compiler_bizarre : COMPILER =
  struct
    let append xs ys =
      xs @ ys

    let compile e_init =
     (* compile : arithmetic_expression -> result_of_compilation *)
      let rec visit e =
        match e with
          Source_syntax.Literal n ->
           if n < 0
           then raise (Negative_integer n)
           else [Target_syntax.Push n]
        | Source_syntax.Plus (e1, e2) ->
           visit_plus e1
                      (fun bcis1 a1 ->
                        append bcis1 (visit_plus e2 append a1))
                      [Target_syntax.Add]
        | Source_syntax.Times (e1, e2) ->
           visit_times e1
                       (fun bcis1 a1 ->
                         append bcis1 (visit_plus e2 append a1))
                       [Target_syntax.Mul]
        and visit_plus e k a =
          match e with
            Source_syntax.Literal n ->
             if n < 0
             then raise (Negative_integer n)
             else k [Target_syntax.Push n] a 
          | Source_syntax.Plus (e1, e2) ->
             visit_plus e1
                        (fun bcis1 a1 ->
                          append bcis1 (visit_plus e2 k a1))
                        (Target_syntax.Add :: a)
          | Source_syntax.Times (e1, e2) ->
             k (visit_times e1
                            (fun bcis1 a1 ->
                              append bcis1 (visit_times e2 append a1))
                            [Target_syntax.Mul])
               a
        and visit_times e k a =
          match e with
            Source_syntax.Literal n ->
              if n < 0
              then raise (Negative_integer n)
              else k [Target_syntax.Push n] a
          | Source_syntax.Plus (e1, e2) ->
             k (visit_plus e1
                           (fun bcis1 a1 ->
                             append bcis1 (visit_plus e2 append a1))
                           [Target_syntax.Add])
               a
          | Source_syntax.Times (e1, e2) ->
             visit_times e1
                         (fun bcis1 a1 ->
                           append bcis1 (visit_times e2 k a1))
                         (Target_syntax.Mul :: a)
      in Target_syntax.Pro (visit e_init)
  end;;

(* ***** *)

(* Task 7a:

   Verify that the diagram commutes.
*)

module Commutation_test_standard_bizarre =
  Commutation_test_maker (Source_interpreter_standard)
                         (Compiler_bizarre)
                         (Target_interpreter_standard);;



(* ***** *)

module Just_in_time_Magritte_bizarre =
  Just_in_time_maker (Compiler_bizarre)
                     (Target_interpreter_Magritte);;

(* ***** *)

(* Task 7b:

   Implement the corresponding source-to-source transformation.
*)


module Source_to_source_bizarre : SOURCE_TO_SOURCE =
  struct
    open Source_syntax

    let transform e_init =
      let rec visit e = 
        match e with 
        | Literal n -> 
        		 e
        | Plus (e1, e2) -> 
        		(match e1 with 
                 | Plus (e1', e2') -> visit (Plus (e1', Plus (e2', e2)))
                 | _ -> Plus (visit e1, visit e2))
        | Times (e1, e2) -> 
        		(match e1 with 
                 | Times (e1', e2') -> visit (Times (e1', Times (e2', e2)))
                 | _ -> Times (visit e1, visit e2))
      in visit e_init

  end;;


(*

  let rec constructer lst = 
  		match lst with
  		| [] -> raise Stop_everything 
  		| h::[] -> h
  		| h::(h2::[]) -> Plus (h, h2)
  		| h::t -> Plus (h, constructer t);;

  let transform e_init =
      let rec visit e plus_acc = 
        match e with 
        | Literal n ->   
        	   (match plus_acc with 
        	   | [] -> e
        	   | h :: _ -> Plus (e, constructer plus_acc))
        | Plus (e1, e2) -> (match e1 with 
                            | Plus (e1', e2') -> 
                           		visit e1 (e2 :: plus_acc)
                            | _ -> Plus (visit e1 plus_acc, visit e2 plus_acc))
        | Times (e1, e2) -> (match e1, e2 with 
                            | Times (e1', e2'), _ -> visit (Times (e1', Times (e2', e2))) []
                            | _, _ -> Times (visit e1 [], visit e2 []) 
        )
      in visit e_init [];;


*)

(*


	  let transform e_init =
      let rec visit e plus_acc = 
        match e with 
        | Literal n ->   
        	   (match plus_acc with 
        	   | [] -> e
        	   | h :: _ -> Plus (e, h))
        | Plus (e1, e2) -> (match e1 with 
                            | Plus (e1', e2') -> 
                           		Plus (visit e1' plus_acc, visit e2' (e2::plus_acc))
                            | _ -> Plus (visit e1 plus_acc, visit e2 plus_acc))
        | Times (e1, e2) -> (match e1, e2 with 
                            | Times (e1', e2'), _ -> visit (Times (e1', Times (e2', e2))) []
                            | _, _ -> Times (visit e1 [], visit e2 []) 
        )
      in visit e_init [];;


 *)

(* ***** *)

(* Task 7c:

   Verify that the diagram commutes.
*)


module Test_transformer_bizarre =
  Test_transformer_maker (Source_to_source_bizarre)
                         (Compiler_bizarre)
                         (Decompiler);;


let rec test_compile_then_decompiled_bizarre n = 
    let expression = random_arithmetic_expression_generator 50 in
    let test = 
    	(Decompiler.decompile (Compiler_bizarre.compile expression) 
   		 = Source_to_source_bizarre.transform expression) 
    in
    if n = 0 then test
    else 
    test && test_compile_then_decompiled (n-1);;


let _ = assert(test_compile_then_decompiled_bizarre 50);;


(* ***** *)

(* Task 7d:

   Verify whether the transformer is idempotent.
*)



module Test_idempotence_bizarre =
  Test_idempotence_maker (Source_to_source_bizarre);;


(* Task 7e:

  Characterize its optimization in English precisely and concisely
*)

(*
Utilizes associativity of multiplication and addition to associate consecutive instances of addition or multiplication to the 
tail of the expression (the inner-most sub-expression of consecutive addition/multiplication is at the right-most position in the 
subexpression.)
If parsed into a string with parentheses, this means normalizes expression so all consecutive addition and/or multiplication parentheses 
are justified right, therefore standardizing expressions by utilizing the property of associativity with multiplication and addition.
*)

(* Task 7e:

  Implement a syntax checker that verifies whether a given arithmetic expression is in normal form, 
and use it to test your source-to-source transformer
*)

let syntax_checker_bizarre e_init = 
    let rec check e = 
      match e with 
      | Source_syntax.Literal n -> true
      | Source_syntax.Plus (e1, e2) -> 
      			(match e1 with 
                 | Source_syntax.Plus (e1', e1'') -> 
                 	false
                 | _ -> 
             		check e1 && check e2)
      | Source_syntax.Times (e1, e2) -> 
      			(match e1 with 
                 | Source_syntax.Times (e1', e1'') -> 
                 	false
                 | _ -> 
             		check e1 && check e2)
    in check e_init;;


let rec verify_syntax_checker_bizarre n = 
    let expression = random_arithmetic_expression_generator 50 in
    let test = (syntax_checker_bizarre (Source_to_source_bizarre.transform expression) = true) in
    if n = 0 then test
    else 
    test && verify_syntax_checker_bizarre (n-1);;


let _ = assert(verify_syntax_checker_bizarre 50);;


(* Task 7e:

  Compare the relative efficiencies of
    -interpreting with your optimizing source-to-source transformer and 
    -compiling and decompiling.

*)

test_of_compile_then_decompile_speed Compiler_bizarre.compile;; 

test_of_transformer_speed Source_to_source_bizarre.transform;;

(* In this test, the source to source transformer is almost 30 times faster than compiling then decompiling! *)



(* ********** *)

module Compiler_quaint : COMPILER =
  struct
    let cons x xs =
      x :: xs

    let put_together bcis1 bcis2 bci =
      List.fold_right cons bcis1 (List.fold_right cons bcis2 [bci])

    let compile e_init =
     (* compile : arithmetic_expression -> result_of_compilation *)
      let rec visit e k =
        match e with
          Source_syntax.Literal n ->
           if n < 0
           then raise (Negative_integer n)
           else k [Target_syntax.Push n]
        | Source_syntax.Plus (e1, e2) ->
           put_together (visit e1 k)
                        (visit e2 k)
                        Target_syntax.Add
        | Source_syntax.Times (e1, e2) ->
           visit e1 (fun bcis1 ->
                      visit e2 (fun bcis2 ->
                                 k (put_together bcis1 bcis2 Target_syntax.Mul)))
      in Target_syntax.Pro (visit e_init (fun bcis -> bcis))
  end;;

(* ***** *)

(* Task 8a:

   Verify that the diagram commutes.
*)

module Commutation_test_standard_quaint =
  Commutation_test_maker (Source_interpreter_standard)
                         (Compiler_quaint)
                         (Target_interpreter_standard);;


(* ***** *)

module Just_in_time_Magritte_quaint =
  Just_in_time_maker (Compiler_quaint)
                     (Target_interpreter_Magritte);;

(* ***** *)

(* Task 8b:

   Implement the corresponding source-to-source transformation.
*)

(*
module Source_to_source_quaint : SOURCE_TO_SOURCE =
  struct
    open Source_syntax

    let transform e_init =
      let rec distribute e = 
        match e with 
        | Literal n -> e
        | Plus (e1, e2) -> Plus (distribute e1, distribute e2)
        | Times (e1, e2) -> (match e1, e2 with
                            | Plus (e1', e2'), Plus (e1'', e2'') -> distribute (Plus (Plus (Times (e1', e1''), Times (e1', e2'')), Plus (Times (e2', e1''), Times (e2', e2''))))
                            | Plus (e1', e2'), Literal n -> distribute (Plus (Times (e1', Literal n), Times (e2', Literal n)))
                            | Literal n, Plus (e1', e2') -> distribute (Plus (Times (Literal n, e1'), Times (Literal n, e1')))
                            | Plus (e1', e2'), Times (e1'', e2'') -> distribute (Plus (Times (e1', Times (e1'', e2'')), Times (e2', Times (e1'', e2''))))
                            | Times (e1', e2'), Plus (e1'', e2'') -> distribute (Plus (Times (Times (e1', e2'), e1''), Times (Times (e1', e2'), e2'')))
                            | e1', e2' -> Times (distribute e1', distribute e2')
        )
      in distribute e_init

  end;;

let comp q = 
Decompiler.decompile (Compiler_quaint.compile q);;
*)

(* ***** *)


(* Task 8e: 
    
    Characterize its optimization in English precisely and concisely. 

*)

(*

Distributes multiplication across subexpressions, such that the product of summands (e.g. Times (Plus (Literal 1, Literal 2), Literal 3) 
becomes the sum of factors (e.g. the prior example becomes: Plus (Times (Literal 1, Literal 3), Times (Literal 2, Literal 3))). 

If this were unparsed using the Margritte interpreter, this would show as a distribution of multiplication inside parenthesized values.

*)


(* ********** *)

(* end of week-15_Magritte-and-the-40-optimizing-compilers.ml *)