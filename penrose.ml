open Graphics;;
open_graph " 1000*1000";;

type gold_triangular = Obtuse | Acute;;

let draw points t_type =
    if t_type = Obtuse then set_color yellow
                       else set_color blue;
    fill_poly points;;
    
let phi = (1. +. sqrt(5.)) /. 2.;;
let pi = 4. *. atan (1.);;
let y = sin(pi /. 5.) ;;
let points = [| (0,0) ; (int_of_float(150. *. phi), int_of_float(300. *. y)) ; (int_of_float(300. *. phi),0) |];;

let rec divide generation points t_type =
    if generation = 0
        then draw points t_type
    else begin
        if t_type = Obtuse then begin
         let x1,y1 = points.(0) and x2,y2 = points.(1)
            and x3,y3 = points.(2) in
            let d1 = sqrt( float_of_int(x1-x2)**2. +. float_of_int(y1-y2)**2. )
             and d2 = sqrt( float_of_int(x1-x3)**2. +. float_of_int(y1-y3)**2. )
            in
            let x = int_of_float( (d1 /. d2) *. float_of_int(x3-x1)) + x1
                and y = int_of_float ( (d1 /. d2) *. float_of_int(y3-y1) ) + y1
            in
            let points_Obtuse = [|points.(2) ; (x,y) ; points.(1)|] and
                points_Acute = [| (x,y) ; points.(0) ; points.(1) |]
            in
                divide (generation-1) points_Obtuse Obtuse;
                divide (generation-1) points_Acute Acute
                
            end
    end;;
    
divide 1 points Obtuse;;
