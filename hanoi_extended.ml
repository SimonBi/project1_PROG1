open Graphics;;
open_graph " 1000*600";;


let rod_status = Array.make 3 [];;

let nb_of_rod rod = match rod with
    | "A" -> 1
    | "B" -> 2
    | "C" -> 3;;


(* TIMER *)

let c = ref 0;;
let init() = c := 0;;
let step() = incr c;;
let get() = !c;;

(* TICK *)

let sleep () =
    for i = 0 to int_of_float(10.**7.7) do
        ()
    done;;

(* GRAPHIC FUNCTIONS *)

let init_rod () =
    set_color black;
    for i = 1 to 3 do
        fill_poly [| (250*i-5,0) ; (250*i+5,0) ;
                     (250*i+5,600) ; (250*i-5,600) |];
    done;
    fill_poly [| (0,0) ; (1000,0) ; (1000,20) ; (0,20) |];;

let init_rod_status n origin =
    let nb_origin = nb_of_rod origin in
    for i = 1 to n do
        rod_status.(nb_origin-1) <- (i)::(rod_status.(nb_origin-1))
    done;;
    
let draw_disc_on_rod nb_rod (height, radius_sub) =
    let rec draw_disc height nb_rod rank_on_rod discs =
        match discs with
            | [] -> ()
            | h::t ->
                fill_poly [| (250 * nb_rod - (110 - radius_sub*h),20 + height * (rank_on_rod-1)) ; 
                             (250 * nb_rod + (110 - radius_sub*h),20 + height * (rank_on_rod-1)) ; 
                             (250 * nb_rod + (110 - radius_sub*h),20 + height * rank_on_rod) ; 
                             (250 * nb_rod - (110 - radius_sub*h),20 + height * rank_on_rod) |];
                draw_disc height nb_rod (rank_on_rod + 1) t
    in
    set_color blue;
    draw_disc height nb_rod 1 (List.rev (rod_status.(nb_rod-1)));;
    
let draw_disc_out h nb_rod (height, radius_sub) =
        fill_poly [| (250 * nb_rod - (120 - radius_sub*h),650) ; 
                     (250 * nb_rod + (120 - radius_sub*h),650) ; 
                     (250 * nb_rod + (120 - radius_sub*h),650 + height) ; 
                     (250 * nb_rod - (120 - radius_sub*h),650 + height) |];;
                     
let draw_update h nb_rod_out (height, radius_sub) is_out =
    clear_graph();
    init_rod();
    for i = 1 to 3 do
        draw_disc_on_rod i (height, radius_sub)
    done;
    if is_out then
        draw_disc_out h nb_rod_out (height, radius_sub);;

let move_and_draw origin destination (height, radius_sub) =
    let nb_rod1 = nb_of_rod origin
    and nb_rod2 = nb_of_rod destination in
    let h = List.hd (rod_status.(nb_rod1-1)) in
    rod_status.(nb_rod1-1) <- List.tl (rod_status.(nb_rod1-1));
    draw_update h nb_rod1 (height, radius_sub) true;
    sleep ();
    draw_update h nb_rod2 (height, radius_sub) true;
    sleep ();
    rod_status.(nb_rod2-1) <- h::(rod_status.(nb_rod2-1));
    draw_update h nb_rod2 (height, radius_sub) false;
    sleep ();;

(* HANOI FUNCTIONS *)

let movement origin destination =
    print_string ( "move a disc from rod " ^ origin ^ " to rod " ^ destination );
    print_newline();;

let middle_rod origin destination =
    let x = (nb_of_rod origin) + (nb_of_rod destination) in
    match x with
        | 3 -> "C"
        | 4 -> "B"
        | 5 -> "A";;

let hanoi n origin destination =
    let height = 300 / n and radius_sub = 100 / n in
    
    init();
    init_rod();
    init_rod_status n origin;
    
    let rec hanoi_towers n origin destination = match n with
        | 0 ->  ()
        | 1 ->  move_and_draw origin destination (height, radius_sub);
                step()
        | n ->  let middle = middle_rod origin destination in
                hanoi_towers (n-1) origin middle;
                move_and_draw origin destination (height, radius_sub); step();
                hanoi_towers (n-1) middle destination
    in
    hanoi_towers n origin destination;
    let count = get() in
    print_int count;;

hanoi 6 "A" "C";;
