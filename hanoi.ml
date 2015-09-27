(*
Authors: Simon Bihel, Florestan De-Moor
Institute: ENS Rennes, Computer Science Department
Course: PROG1

First version: only basic features.
*)


(* TIMER *)
let c = ref 0;;
let init() = c := 0;;
let step() = incr c;;
let get() = !c;;


let movement origin destination =
(** Print the movement to do *)
    print_string ( "move a disc from rod " ^ origin ^ " to rod " ^ destination );
    print_newline();;

let nb_of_rod rod =
    match rod with
    | "A" -> 1
    | "B" -> 2
    | "C" -> 3;;

let middle_rod origin destination =
(** Return the third rod *)
    let x = (nb_of_rod origin) + (nb_of_rod destination) in
    match x with
        | 3 -> "C"
        | 4 -> "B"
        | 5 -> "A";;

let hanoi n origin destination =
    (* game with n discs and 3 rods *)
    
    init(); (*initialize the timer *)
    
    (* recursive resolution of the problem *)
    let rec hanoi_towers n origin destination =
        match n with
        | 0 -> ()
        
        | 1 -> movement origin destination; step()
        
        | n -> let middle = middle_rod origin destination in
            hanoi_towers (n-1) origin middle;
            movement origin destination; step();
            hanoi_towers (n-1) middle destination
    in
    
    hanoi_towers n origin destination;
    
    (* print the number of movements *)
    let count = get() in
    print_int count;;

hanoi 5 "A" "C";;
