(* implementing-a-sieve-brian-bohme.ml *)
(* Fundamentals of Programming Mini-Project #1 *)
(* Brian Bohme *)
(* A0144436H *)
(* brianbohme@u.yale-nus.edu.sg *)
(* March 2, 2017 *)

(* Types *)

type 'a xstream = Xcons of 'a * (unit -> 'a xstream);;

exception Negative_int of string;;

(* Helper functions *)

let force thunk =
    thunk ();;

let rec stream n =
  	Xcons (n, fun () -> stream (n + 1));;

let rec zero = 
	Xcons (0, fun () -> zero);;

let rec one = 
	Xcons (1, fun () -> one);;

let rec prefix xs n =
  if n = 0
  then []
  else match xs with 
  	  | Xcons (hd, tl) -> hd :: (prefix (force tl) (n - 1));;

(* Test streams *)

let naturals = stream 0;;

let naturals1 = stream 1;;

let zeros = 
	match zero with 
	| Xcons (hd, tl) -> Xcons (1, tl);;

let two_then_zeros = 
	match zero with 
	| Xcons (hd, tl) -> Xcons (2, tl);;

let ten_then_zeros = 
	match zero with 
	| Xcons (hd, tl) -> Xcons (10, tl);;

let ones = one;;


(*****************************)


(* 1 *)

let striker xs_init f = 
	let rec filter xs f_counter = 
		match xs with 
		| Xcons (hd, tl) -> 
			if f_counter = -1
			then filter (force tl) f
			else Xcons (hd, fun () -> filter (force tl) (f_counter - 1))
	in if f < 0 
	then raise (Negative_int "striker") 
	else filter xs_init f;;


let test_striker candidate = 
	((prefix (candidate naturals 0) 10 = [0;2;4;6;8;10;12;14;16;18]) &&
	 (prefix (candidate naturals1 0) 10 = [1;3;5;7;9;11;13;15;17;19]) &&
	 (prefix (candidate naturals 1) 10 = [0;1;3;4;6;7;9;10;12;13]) &&
	 (prefix (candidate naturals1 1) 10 = [1;2;4;5;7;8;10;11;13;14]))
	 (* etc *);;

let _ = assert(test_striker striker);;


(* 2 *)
let partial_sums xs_init = 
	let rec visit xs acc = 
		match xs with 
		| Xcons (hd, tl) -> 
			let partial_sum = acc + hd 
			in Xcons (partial_sum, fun () -> visit (force tl) partial_sum)
	in visit xs_init 0;;


let test_partial_sums candidate = 
	((prefix (candidate ones) 10 = [1;2;3;4;5;6;7;8;9;10]) &&
	 (prefix (candidate (striker naturals1 0)) 10 = [1;4;9;16;25;36;49;64;81;100]) &&
	 (prefix (candidate naturals) 10 = [0;1;3;6;10;15;21;28;36;45]))
	 (* etc *);;

let _ = assert(test_partial_sums partial_sums);;


(* 3 *)
let strike_and_partial_sum xs_init n =  
	partial_sums (striker xs_init n);;


let test_strike_and_partial_sum candidate = 
	((prefix (candidate naturals 0) 10 = [0;2;6;12;20;30;42;56;72;90]) &&
	 (prefix (candidate naturals1 1) 10 = [1;3;7;12;19;27;37;48;61;75]) &&
	 (prefix (candidate ones 2) 10 = [1;2;3;4;5;6;7;8;9;10]))
	 (* etc *);;

let _ = assert(test_strike_and_partial_sum strike_and_partial_sum);;


(* 4 *)

let sieve xs_init k = 
	let rec visit xs n = 
		let xs_new = strike_and_partial_sum xs n in
		if n = 0 
		then xs_new
		else visit xs_new (n - 1)
	in visit xs_init k;;


(* 
Resulting stream is the stream of the k-powers of natural numbers (squares, cubes, fourth-powers, ... k-powers.)
*)


let test_sieve candidate = 
	((prefix (candidate zeros 1) 10 = [1;2;3;4;5;6;7;8;9;10]) &&
	 (prefix (candidate zeros 2) 10 = [1;4;9;16;25;36;49;64;81;100]) &&
	 (prefix (candidate zeros 3) 10 = [1;8;27;64;125;216;343;512;729;1000]))
	 (* etc *);;

let _ = assert(test_sieve sieve);;

let rec power k a = 
	match k with 
  	| 0 -> 1
  	| 1 -> a
  	| n -> 
    	let b = power (n / 2) a in
    	b * b * (if n mod 2 = 0 then 1 else a);;

let k_powers_test k = 
	let n = Random.int 1000 in 
	prefix (sieve zeros k) n
	= 
	List.map (power k) (prefix naturals1 n);;

let _ = assert(k_powers_test 1 && 
			   k_powers_test 2 && 
			   k_powers_test 3 && 
			   k_powers_test 4 &&
			   k_powers_test 5 && 
			   k_powers_test 6 &&
			   k_powers_test 7 && 
			   k_powers_test 8 &&
			   k_powers_test 9 && 
			   k_powers_test 10)
			   (* etc *);; 


(* Testing the 2,0,0,0... input of sieve against a list of natural numbers 1) to the power of k and 2) multiplied by 2 *)

let two_times_powers_test k = 
	let n = Random.int 1000 in 
	prefix (sieve two_then_zeros k) n
	= 
	List.map (fun i -> 2 * i) (List.map (power k) (prefix naturals1 n));;

let _ = assert(two_times_powers_test 1 && 
			   two_times_powers_test 2 && 
			   two_times_powers_test 3 && 
			   two_times_powers_test 4 &&
			   two_times_powers_test 5 && 
			   two_times_powers_test 6 &&
			   two_times_powers_test 7 && 
			   two_times_powers_test 8 &&
			   two_times_powers_test 9 && 
			   two_times_powers_test 10)
			   (* etc *);; 


(* Testing the 10,0,0,0... input of sieve against a list of natural numbers 1) to the power of k and 2) multiplied by 10 *)

let ten_times_powers_test k = 
	let n = Random.int 1000 in 
	prefix (sieve ten_then_zeros k) n
	= 
	List.map (fun i -> 10 * i) (List.map (power k) (prefix naturals1 n));;


let _ = assert(ten_times_powers_test 1 && 
			   ten_times_powers_test 2 && 
			   ten_times_powers_test 3 && 
			   ten_times_powers_test 4 &&
			   ten_times_powers_test 5 && 
			   ten_times_powers_test 6 &&
			   ten_times_powers_test 7 && 
			   ten_times_powers_test 8 &&
			   ten_times_powers_test 9 && 
			   ten_times_powers_test 10)
			   (* etc *);; 

(* Testing patterns with even/odd streams *)

(* Multiply each element by its index *)

let rec index_multiplier lst_init = 
	let rec visit lst k = 
		match lst with
		| [] -> lst
		| h::t -> (k * h) :: (visit t (k + 1))
	in visit lst_init 1;; 

(* Repeat a mapping n times *)

let rec repeated_map lst f n = 
	if n = -1 then lst 
	else repeated_map (f lst) f (n - 1);;

let odds = striker naturals1 0;;

(* Test for odds *)

let odd_stream_sieve k = 
	let n = Random.int 1000 in 
	prefix (sieve odds k) n
	= 
	repeated_map (prefix odds n) index_multiplier k;;

let _ = assert(odd_stream_sieve 1 && 
			   odd_stream_sieve 2 && 
			   odd_stream_sieve 3 && 
			   odd_stream_sieve 4 &&
			   odd_stream_sieve 5 && 
			   odd_stream_sieve 6 &&
			   odd_stream_sieve 7 && 
			   odd_stream_sieve 8 &&
			   odd_stream_sieve 9 && 
			   odd_stream_sieve 10)
			   (* etc *);; 

let evens = striker naturals 0;;

(* Test for evens *)

let evens_stream_sieve k = 
	let n = Random.int 1000 in 
	prefix (sieve evens k) n
	= 
	repeated_map (prefix evens n) index_multiplier k;;

let _ = assert(evens_stream_sieve 1 && 
			   evens_stream_sieve 2 && 
			   evens_stream_sieve 3 && 
			   evens_stream_sieve 4 &&
			   evens_stream_sieve 5 && 
			   evens_stream_sieve 6 &&
			   evens_stream_sieve 7 && 
			   evens_stream_sieve 8 &&
			   evens_stream_sieve 9 && 
			   evens_stream_sieve 10)
			   (* etc *);; 

(* end of document *)

