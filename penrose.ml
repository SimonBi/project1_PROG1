open Graphics;;
open_graph " 1000*1000";;

let draw points =
    fill_poly points;;
    
let phi = (1. +. sqrt(5.)) /. 2.;;
let pi = 4. *. atan (1.);;
let y = sin(pi /. 5.) ;;
let points = [| (0,0) ; (int_of_float(300. *. phi),0) ; (int_of_float(150. *. phi), int_of_float(300. *. y)) |];;
set_color yellow;;
draw points;;

let rec divide generation divise points t_type =
    if generation = 0
        then draw points
    else begin
        if t_type = "obtuse" then
         ()
    end;;
