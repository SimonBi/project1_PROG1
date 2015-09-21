(* TIMER *)
let c = ref 0;;
let init() = c := 0;;
let step() = incr c;;
let get() = !c;;


let movement origin destination =
    print_string ( "move a disc from rod " ^ origin ^ " to rod " ^ destination );
    print_newline();;

let nb_of_rod rod = match rod with
    | "A" -> 1
    | "B" -> 2
    | "C" -> 3;;

let middle_rod origin destination =
    let x = (nb_of_rod origin) + (nb_of_rod destination) in
    match x with
        | 3 -> "C"
        | 4 -> "B"
        | 5 -> "A";;

let hanoi n origin destination =
    init();
    let rec hanoi_towers n origin destination = match n with
        | 0 -> ()
        | 1 -> movement origin destination; step()
        | n -> let middle = middle_rod origin destination in
            hanoi_towers (n-1) origin middle;
            movement origin destination; step();
            hanoi_towers (n-1) middle destination
    in
    hanoi_towers n origin destination;
    let count = get() in
    print_int count;;

hanoi 6 "A" "C";;
