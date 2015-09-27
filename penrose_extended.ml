(*
Authors: Simon Bihel, Florestan De-Moor
Institute: ENS Rennes, Computer Science Department
Course: PROG1

Second version: advanced features.
*)

#load "graphics.cma"
#load "unix.cma"
open Unix;;
open Graphics;;

close_graph();;
open_graph " 1000*1000";;
resize_window 600 600;;
let phi = (1. +. sqrt(5.)) /. 2.;;
type gold_triangle = Obtuse | Acute;;
let pi = 4. *. atan (1.);;


(** Variables that can be changed. *)
let wait_each_triangle = false;;
let nb_generations = 5;;
let show_each_generation = true;;
let robinson_single_triangle = false;;
let robinson_wheel_triangles = true;;


(** Sleep function. *)
let minisleep sec =
  try ignore (Unix.select [] [] [] sec) with
    | _ -> print_string "Error for sleep on OS X.\n";;


(** Extract the values of a couple *)
let extract t =
  (fst t) (snd t);;

(** Draw a triangle. *)
let draw points t_type =
  moveto (fst points.(0)) (snd points.(0));
  set_color black;
  set_line_width 2;
  if t_type = Obtuse then begin
    lineto (fst points.(1)) (snd points.(1));
    lineto (fst points.(2)) (snd points.(2));
    set_color yellow;
    lineto (fst points.(0)) (snd points.(0))
  end
  else begin
    moveto (fst points.(2)) (snd points.(2));
    lineto (fst points.(0)) (snd points.(0));
    lineto (fst points.(1)) (snd points.(1));
    set_color blue;
    lineto (fst points.(2)) (snd points.(2))
  end;
  fill_poly points;
  if wait_each_triangle then  minisleep 0.05;;


(** Divide a segment and return the point of separation. *)
let compute_newpoint (x1, y1) (x2, y2) =
  let x = int_of_float ((float_of_int(x2-x1)) /. phi) + x1 
  and y = int_of_float ((float_of_int(y2-y1)) /. phi) + y1
  in (x, y);; 


(** Divide a triangle into multiple smaller triangles. *)
let divise points t_type =
  let x0, y0 = points.(0)
  and x1, y1 = points.(1)
  and x2, y2 = points.(2) in

  match t_type with
  | Obtuse ->
     let x,y = compute_newpoint (x0, y0) (x2, y2) in
     [|[|points.(0); points.(1); (x, y)|] ;
       [|points.(2); (x, y); points.(1)|]|]
  | Acute ->
     let x,y = compute_newpoint (x1, y1) (x0, y0)
     and xp,yp = compute_newpoint (x0, y0) (x2, y2) in
     [|[|points.(1); points.(2); (xp, yp)|];
       [|points.(1); (xp, yp); (x, y)|];
       [|points.(0); (x, y); (xp, yp)|]|]


(** Divide triangles recursively, creating a Penrose tiling. *)
let rec divide generation points t_type =
  if generation = 0 then draw points t_type
  else begin
    let new_triangles = divise points t_type in
    match t_type with
    | Obtuse ->
       divide (generation-1) new_triangles.(0) Acute;
       divide (generation-1) new_triangles.(1) Obtuse
    | Acute -> 
       divide (generation-1) new_triangles.(0) Acute;
       divide (generation-1) new_triangles.(1) Acute;
       divide (generation-1) new_triangles.(2) Obtuse
  end;;


if robinson_single_triangle then 
  let y = sin(pi /. 5.) in
  let points = [|(0, 0); (int_of_float(150. *. phi), int_of_float(300. *. y));
                 (int_of_float(300. *. phi),0)|] in
  if show_each_generation then 
    for i = 0 to nb_generations do
      divide i points Obtuse;
      minisleep 0.2
    done
  else divide nb_generations points Obtuse;;


(** Build a wheel of acute triangles and call divide for each of them. *)
if robinson_wheel_triangles then begin
  let t_type = Acute
  and triangles = ref [] in
  let zoom = 250. in begin
    for i = 1 to 5 do
      let fi = float_of_int i 
      and center = 300. in
      let intC = int_of_float center in
      let point1 = (int_of_float(center +. (zoom *. cos((fi -. 1.)  *.  pi /. 5.))),
                    int_of_float(center +. (zoom *. sin((fi -. 1.)  *.  pi /. 5.))))
      and point2 = (int_of_float(center +. (zoom *. cos(fi  *.  pi /. 5.))),
                    int_of_float(center +. (zoom *. sin(fi  *.  pi /. 5.)))) in
      begin
        if i mod 2 = 0 then 
          triangles := ( !triangles) @ [[|(intC, intC); point1; point2|];
                                        [|(intC, intC); (-1 * (fst point2) + 2 * intC, -1 * (snd point2) + 2 * intC); 
                                          (-1 * (fst point1) + 2 * intC, -1 * (snd point1) + 2 * intC)|]]
        else
          triangles := ( !triangles) @ [[|(intC, intC); point2; point1|];
                                        [|(intC, intC); (-1 * (fst point1) + 2 * intC, -1 * (snd point1) + 2 * intC); 
                                          (-1 * (fst point2) + 2 * intC, -1 * (snd point2) + 2 * intC)|]]
      end
    done;

    while !triangles <> [] do
      divide nb_generations (List.hd !triangles) t_type;
      triangles := (List.tl !triangles)
    done
  end
  end;;


(** Ask to exit. *)
let quit_loop = ref false in
while not !quit_loop
do
  print_string "Exit ? (y/N) ";
  let command_input = read_line () in
  if command_input.[0] = 'y' then
    quit_loop := true
done;;
