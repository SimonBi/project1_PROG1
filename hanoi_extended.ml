(*
Authors: Simon Bihel, Florestan De-Moor
Institute: ENS Rennes, Computer Science Department
Course: PROG1

Second version: advanced features :
Graphic representation of the movements
*)

#load "unix.cma"

open Unix;;
open Graphics;;
open_graph " 1000x700";;


(* TIMER *)
let c = ref 0;;
let init() = c := 0;;
let step() = incr c;;
let get() = !c;;


let sleep sec =
(** Sleep function. *)
  try ignore (Unix.select [] [] [] sec) with
    | _ -> print_string "Error for sleep on OS X.\n";;


(* GRAPHIC FUNCTIONS *)


let fill_rec x y w h =
(** Fill a rectangle whose lower ridge middle is in (x,y), whose length is 2*w
and chose height is h *)
    fill_poly [| (x-w,y) ; (x+w,y) ; (x+w,y+h) ; (x-w,y+h) |];;

let init_rod () =
(** Draw 3 rods on a pedestal *)
    set_color black;
    for i = 1 to 3 do
        fill_rec (250*i) 0 5 400;
    done;
    fill_rec 500 0 500 20;;

let init_rod_status n origin =
(** Return an array of 3 piles
    Place n discs in the pile corresponding to rod origin *)
    let rod_status = Array.make 3 [] in
    for i = 1 to n do
        rod_status.(origin-1) <- (i)::(rod_status.(origin-1))
    done;
    rod_status;;
    
let draw_disc_on_rod (nb_rod) (rod_status) (height, radius_sub) =
(** Draw all discs on rod nb_rod *)
    let rec draw_disc nb_rod rank_on_rod discs =
        match discs with
            | [] -> ()
            | h::t ->
                fill_rec (250 * nb_rod) ( 20 + height * (rank_on_rod-1) ) (120 - radius_sub*h) height;
                draw_disc nb_rod (rank_on_rod + 1) t
    in
    set_color blue;
    draw_disc nb_rod 1 (List.rev (rod_status.(nb_rod-1)));;
    
let draw_disc_out h nb_rod (height, radius_sub) =
(** Draw the h-th disc out of the rod nb_rod *)
        fill_rec (250 * nb_rod) 500 (120 - radius_sub*h) height;;
                     
let draw_update h nb_rod_out (rod_status) (height, radius_sub) is_out =
(** Clear all drawings and draw the new game status
    If is_out then draw the h-th disc out of rod nb_rod *)
    clear_graph();
    init_rod();
    for i = 1 to 3 do
        draw_disc_on_rod i (rod_status) (height, radius_sub)
    done;
    if is_out then
        draw_disc_out h nb_rod_out (height, radius_sub);;

let move_and_draw origin destination (rod_status) (height, radius_sub) =
(** Move the top disc of origin to destination and update the game status *)
    
    (* 1. Remove top disc of origin and draw it out of origin *)
    let h = List.hd (rod_status.(origin-1)) in
    rod_status.(origin-1) <- List.tl (rod_status.(origin-1));
    draw_update h origin (rod_status) (height, radius_sub) true;
    sleep 0.4;
    
    (* 2. Draw it out of destination *)
    draw_update h destination (rod_status) (height, radius_sub) true;
    sleep 0.4;
    
    (* 3. Place it on top of destination, and draw final status *)
    rod_status.(destination-1) <- h::(rod_status.(destination-1));
    draw_update h destination (rod_status) (height, radius_sub) false;
    sleep 0.4;;


(* HANOI FUNCTIONS *)


let middle_rod origin destination =
(** Return the number of the third rod *)
    let x = origin + destination in
    match x with
        | 3 -> 3
        | 4 -> 2
        | 5 -> 1
        | _ -> failwith "middle_rod";;

let hanoi n origin destination =
    
    (* discs on rods are stacked *)
    
    
    let height = 300 / n (* disc height *)
    and radius_sub = 100 / n in (* elementary radius of the discs *)
    
    init(); (* initialize the timer *)
    
    (* initializing the graphic representation *)
    init_rod();
    let rod_status = init_rod_status n origin in
    draw_update n origin (rod_status) (height, radius_sub) false;
    sleep 0.4;
    
    (* resursive resolution of the problem *)
    let rec hanoi_towers n origin destination =
        match n with
        | 0 ->  ()
        
        | 1 ->  move_and_draw origin destination (rod_status) (height, radius_sub);
                step()

        | n ->  let middle = middle_rod origin destination in
                hanoi_towers (n-1) origin middle;
                move_and_draw origin destination (rod_status) (height, radius_sub);
                step();
                hanoi_towers (n-1) middle destination

    in
    hanoi_towers n origin destination;
    
    (* print the number of movements *)
    let count = get() in
    print_int count;;


hanoi 3 1 3;;
