(*
Authors: BIHEL Simon, De-Moor Florestan
Institute: ENS Rennes
*)

#load "graphics.cma";;
open Graphics;;

let colors = [|"yellow"; "blue"|]

(* Close any possible open window *)
close_graph ();;

(* Open a 800x600 window. Make sure to include a space at the beginning of the heometry specification *)
open_graph " 800x600-0+0";;


(** Test if a triangle is obtuse *)
let is_obtuse = false;;

(** Draw a triangle from an array of 3 points and a colour. *)
let draw points kind = begin
  begin
    if kind = "obtuse" then
      set_color (colors[0])
    else
      set_color (colors[1]);
  end;;
  fill_poly points
  end;;

let length_side points = ((points[0][0] - points[1][0])**2 + (points[0][1] - points[1][1]))**0.5;;

(** Longest sides of an acute triangle. *)
let longest_sides points = begin
  let length_sides = [|(length_side [|points[0]; points[1]|]);(length_side [|points[1]; points[2]|]);(length_side [|points[2]; points[0]|])|];
  if length_sides[0] > length_sides[1] then
    [|[|0;1|];[|2;0|]|]
  else begin
    if length_sides[0] < length_sides[1] then
      [|[|1;2|];[|2;0|]|]
    else
      [|[|0;1|];[|1;2|]|]
  end
  end;;

(** Longest side of an obtuse triangle. *)
let longest_side points = begin
    let length_sides = [|(length_side [|points[0]; points[1]|]);(length_side [|points[1]; points[2]|]);(length_side [|points[2]; points[0]|])|];
    if length_sides[0] > length_sides[1] then
      [|[|0;1|]|]
    else begin
      if length_sides[0] < length_sides[1] then
        [|[|1;2|]|]
      else
        [|[|2;0|]|]
    end
  end;;


(** Draw the children of a triangle *)
let rec divide generation divise points kind =
  if generation = 0
    then draw points kind
  else begin
    if kind = obtuse then
      
    else
      
  end;;

(** Ask to exit. *)
let quit_loop = ref false in
while not !quit_loop
do
  print_string "Exit ? (y/n) ";
  let command_input = read_line () in
  if command_input.[0] = 'y' then
    quit_loop := true
done;;
