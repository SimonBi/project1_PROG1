(*
Author: BIHEL Simon, De-Moor Florestan
Institute: ENS Rennes
*)

#load "graphics.cma";;
open Graphics;;

(* Close any possible open window *)
close_graph ();;

(* Open a 800x600 window. Make sure to include a space at the beginning of the heometry specification *)
open_graph " 800x600-0+0";;

(* Move to point 50x100 *)
moveto 50 100;;

(* Draw a line to point 300x150 *)
lineto 300 150;;

(* Set the filling color: red, blue, yellow, cyan, magenta, etc. *)
set_color green;;

(* Fill the polygon defined by points 0x0, 10x50 and 50x100 *)
fill_poly [|(0,0);(10,50);(50,10)|];;
