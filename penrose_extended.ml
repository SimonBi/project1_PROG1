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
open List;;

close_graph();;
open_graph " 1000*1000";;
resize_window 600 600;;
let phi = (1. +. sqrt(5.)) /. 2.;;
type gold_triangle = Obtuse | Acute;;
let pi = 4. *. atan (1.);;


(** Variables that can be changed. *)
let wait_each_triangle = false;;
let nb_generations = 6;;
let show_each_generation = true;;
let robinson_single_triangle = false;;
let robinson_wheel_triangles = not robinson_single_triangle;;
let type_wheel_triangles = Acute;;
    (* Acute triangles create a wheel, obtuse create a star. *)
let reverse_triangles = false;;
let homothetic = true;;
let general_center = 300;;


let reverse_triangle t t_type = match reverse_triangles with
  |true -> if t_type = Acute then
             [|t.(0); t.(2); t.(1)|]
           else
             [|t.(2); t.(1); t.(0)|]
  |false -> t;;


(** Sleep function. *)
let minisleep sec =
  try ignore (Unix.select [] [] [] sec) with
    | _ -> print_string "Error for sleep on OS X.\n";;


(** Extract the values of a couple *)
let extract t =
  (fst t) (snd t);;


let intT_of_floatT points = Array.map (fun (x,y) -> (int_of_float x, int_of_float y)) points;;


(** Draw a triangle. *)
let draw pointsF t_type =
  let points = intT_of_floatT pointsF in
  moveto (fst points.(0)) (snd points.(0));
  set_color black;
  set_line_width 2;
  (* Only two borders of a triangle are drawn to create new shapes
  when two triangles are side-by-side. *)
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
  let x = ((x2 -. x1) /. phi) +. x1 
  and y = ((y2 -. y1) /. phi) +. y1
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


let point_farther_center center point =
  (center +. ((fst point) -. center) *. phi, 
   center +. ((snd point) -. center) *. phi);;


let zoom_triangle t center = Array.map (point_farther_center center) t;;


(** Divide triangles recursively, creating a Penrose tiling. *)
let rec divide generation points t_type =
  if generation = 0 then draw points t_type
  else begin
    let base_t = if homothetic then 
                   zoom_triangle points (float_of_int general_center)
                 else points in
    let new_triangles = divise base_t t_type in
    match t_type with
    | Obtuse ->
       divide (generation-1) new_triangles.(0) Acute;
       divide (generation-1) new_triangles.(1) Obtuse
    | Acute -> 
       divide (generation-1) new_triangles.(0) Acute;
       divide (generation-1) new_triangles.(1) Acute;
       divide (generation-1) new_triangles.(2) Obtuse
  end;;

(** Create a single obtuse triangle and call divide for it. *)
if robinson_single_triangle then 
  let y = sin(pi /. 5.) in
  let points = reverse_triangle [|(0., 0.); (150. *. phi, 300. *. y);
                 (300. *. phi,0.)|] Obtuse in
  if show_each_generation then 
    for i = 0 to nb_generations do
      divide i points Obtuse;
      minisleep 0.2
    done
  else divide nb_generations points Obtuse;;


let point_closer_center point center = 
  (center +. ((fst point) -. center) /. phi, 
   center +. ((snd point) -. center) /. phi);;


(** Build a wheel of identical triangles and call divide for each of them. *)
if robinson_wheel_triangles then begin
  let t_type = type_wheel_triangles
  and triangles = ref [] in
  let zoom = if homothetic then 25. else 250. in begin
    (* Build the triangles of the wheel.
       For each for loop we create a triangle and it's mirror one. *)
    for i = 1 to 5 do
      let fi = float_of_int i 
      and center = float_of_int general_center in

      let point1 = (center +. (zoom *. cos((fi -. 1.)  *.  pi /. 5.)),
                    center +. (zoom *. sin((fi -. 1.)  *.  pi /. 5.)))
      and point2 = (center +. (zoom *. cos(fi  *.  pi /. 5.)),
                    center +. (zoom *. sin(fi  *.  pi /. 5.))) in
      let obtuse_point1 = point_closer_center point1 center 
      and obtuse_point2 = point_closer_center point2 center in

      if t_type = Acute then begin
          if i mod 2 = 0 then 
            triangles := ( !triangles) @ [[|(center, center); point1; point2|];
                                          [|(center, center);
                                            (-1. *. (fst point2) +. 2. *. center,
                                             -1. *. (snd point2) +. 2. *. center); 
                                            (-1. *. (fst point1) +. 2. *. center,
                                             -1. *. (snd point1) +. 2. *. center)|]]
          else
            triangles := ( !triangles) @ [[|(center, center); point2; point1|];
                                          [|(center, center);
                                            (-1. *. (fst point1) +. 2. *. center, 
                                             -1. *. (snd point1) +. 2. *. center); 
                                            (-1. *. (fst point2) +. 2. *. center,
                                             -1. *. (snd point2) +. 2. *. center)|]]
        end
      (* Reversing an obtuse triangle to eventually create a star 
         is a little more complicated. *)
      else begin
          if i mod 2 = 0 then 
            triangles := ( !triangles) @ [[|(center, center);
                                            obtuse_point1;
                                            point2|];
                                          [|(center, center);
                                            (-1. *. (fst obtuse_point2) +. 2. *. center,
                                             -1. *. (snd obtuse_point2) +. 2. *. center); 
                                            (-1. *. (fst point1) +. 2. *. center,
                                             -1. *. (snd point1) +. 2. *. center)|]]
          else
            triangles := ( !triangles) @ [[|(center, center); obtuse_point2; point1|];
                                          [|(center, center);
                                            (-1. *. (fst obtuse_point1) +. 2. *. center,
                                             -1. *. (snd obtuse_point1) +. 2. *. center); 
                                            (-1. *. (fst point2) +. 2. *. center,
                                             -1. *. (snd point2) +. 2. *. center)|]]
        
      end
    done;
  
    (* Drawing of the triangles. *)
    if show_each_generation then
      for i = 0 to nb_generations do
        let triangles_copy = ref !triangles in
        while !triangles_copy <> [] do
          divide i (reverse_triangle (hd !triangles_copy) t_type) t_type;
          triangles_copy := (tl !triangles_copy)
        done;
        minisleep 0.2
      done
    else
      while !triangles <> [] do
        divide nb_generations (reverse_triangle (hd !triangles) t_type) t_type;
        triangles := (tl !triangles)
      done;
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
