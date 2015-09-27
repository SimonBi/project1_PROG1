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
  ignore (Unix.select [] [] [] sec);;


(* GRAPHIC FUNCTIONS *)

let color_of_disc h =
(** Create a gradient of colors for the disc *)
    ( ((h*int_of_float(16e6)) mod 16777215) : color);;

let fill_rec x y w h =
(** Fill a rectangle whose lower ridge middle is in (x,y), whose length is 2*w
and chose height is h *)
    fill_poly [| (x-w,y) ; (x+w,y) ; (x+w,y+h) ; (x-w,y+h) |];;

let init_rod height p =
(** Draw p rods on a pedestal *)
    set_color black;
    let x = size_x () / (p+1) in
    let h = size_y () / 2 in
    for i = 1 to p do
        fill_rec (x*i) 0 5 (h + height);
    done;
    fill_rec (size_x () / 2) 0 (size_x () / 2) 20;;

let init_rod_status n origin p =
(** Return an array of 3 piles
    Place n discs in the pile corresponding to rod origin *)
    let rod_status = Array.make p [] in
    for i = 1 to n do
        rod_status.(origin-1) <- (i)::(rod_status.(origin-1))
    done;
    rod_status;;
    
let draw_disc_on_rod (nb_rod) (rod_status) (height, radius_sub) p =
(** Draw all discs on rod nb_rod *)
    let x = size_x () / (p+1) in
    let rec draw_disc nb_rod rank_on_rod discs =
        match discs with
            | [] -> ()
            | h::t ->
                set_color (color_of_disc h);
                fill_rec (x * nb_rod) ( 20 + height * (rank_on_rod-1) ) (x/2 + 5 - radius_sub*h) height;
                draw_disc nb_rod (rank_on_rod + 1) t
    in
    draw_disc nb_rod 1 (List.rev (rod_status.(nb_rod-1)));;
    
let draw_disc_out h nb_rod (height, radius_sub) p = 
(** Draw the h-th disc out of the rod nb_rod *)
    let x = size_x () / (p+1) in
    set_color (color_of_disc h);
    fill_rec (x * nb_rod) (size_y () / 2 + 3 * height) (x/2 + 5 - radius_sub*h) height;;
                     
let draw_update h nb_rod_out (rod_status) (height, radius_sub) is_out p =
(** Clear all drawings and draw the new game status
    If is_out then draw the h-th disc out of rod nb_rod *)
    clear_graph();
    init_rod height p;
    for i = 1 to p do
        draw_disc_on_rod i (rod_status) (height, radius_sub) p
    done;
    if is_out then
        draw_disc_out h nb_rod_out (height, radius_sub) p;;

let move_and_draw origin destination (rod_status) (height, radius_sub) p =
(** Move the top disc of origin to destination and update the game status *)
    
    (* 1. Remove top disc of origin and draw it out of origin *)
    let h = List.hd (rod_status.(origin-1)) in
    rod_status.(origin-1) <- List.tl (rod_status.(origin-1));
    draw_update h origin (rod_status) (height, radius_sub) true p;
    sleep 0.4;
    
    (* 2. Draw it out of destination *)
    draw_update h destination (rod_status) (height, radius_sub) true p;
    sleep 0.4;
    
    (* 3. Place it on top of destination, and draw final status *)
    rod_status.(destination-1) <- h::(rod_status.(destination-1));
    draw_update h destination (rod_status) (height, radius_sub) false p;
    sleep 0.4;;


(* HANOI FUNCTIONS *)


let other_rods origin destination p =
(** Return the list of the rods without origin and destination *)
    let rod_list = ref [] in
    for i = p downto 1 do
        rod_list := i :: (! rod_list)
    done;
    let f = function x -> (origin <> x) && (destination <> x) in
    List.filter f ( !rod_list);;

let move_lower_discs n origin destination (rod_list) (rod_list2) (rod_status) (height, radius_sub) p =
(** Move the p-2 lower discs *)
        for i = 2 to n do
            let x = List.hd ( !rod_list) in
            rod_list := List.tl ( !rod_list);
            rod_list2 := x::( !rod_list2);
            move_and_draw origin x (rod_status) (height, radius_sub) p;
            step()
        done;
                
        move_and_draw origin destination (rod_status) (height, radius_sub) p;
        step();
                
        for i = 2 to n do
            let x = List.hd ( !rod_list2) in
            rod_list2 := List.tl ( !rod_list2);
            move_and_draw x destination (rod_status) (height, radius_sub) p;
            step()
        done;;


let hanoi n origin destination p =
    (* game with n discs and p rods *)
    (* discs on rods are stacked *)
    
    let height = size_y() / (2*n) (* disc height *)
    and radius_sub = size_x() / (2*(p+1)*n) in (* elementary radius of the discs *)
    
    init(); (* initialize the timer *)
    
    (* initializing the graphic representation *)
    init_rod height p;
    let rod_status = init_rod_status n origin p in
    draw_update n origin (rod_status) (height, radius_sub) false p;
    sleep 0.4;
    
    (* resursive resolution of the problem *)
    let rec hanoi_towers n origin destination =
        match n with
        | 0 ->  ()
        
        | n when n < p -> 
                let rod_list = ref (other_rods origin destination p)
                and rod_list2 = ref [] in
                
                move_lower_discs n origin destination (rod_list) (rod_list2) (rod_status) (height, radius_sub) p
 

        | n ->  let rod_list = ref (other_rods origin destination p)
                and rod_list2 = ref [] in
                let x = List.hd ( !rod_list) in
                rod_list := List.tl ( !rod_list);
                
                hanoi_towers (n-p+2) origin x;
                move_lower_discs (p-2) origin destination (rod_list) (rod_list2) (rod_status) (height, radius_sub) p;
                hanoi_towers (n-p+2) x destination

    in
    hanoi_towers n origin destination;
    
    (* print the number of movements *)
    let count = get() in
    print_int count;;


hanoi 10 1 6 6;;
