open Graphics;;
open_graph " 1000*1000";;

let phi = (1. +. sqrt(5.)) /. 2.;;

type gold_triangle = Obtuse | Acute;;

let draw points t_type =
    if t_type = Obtuse then set_color yellow
                       else set_color blue;
    fill_poly points;;
    

let pi = 4. *. atan (1.);;
let y = sin(pi /. 5.) ;;
let points = [| (0,0) ; (int_of_float(150. *. phi), int_of_float(300. *. y)) ; (int_of_float(300. *. phi),0) |];;

let compute_newpoint (x1,y1) (x2,y2) =
	let x = int_of_float ( (float_of_int(x2-x1)) /. phi) + x1 
	and y = int_of_float ( (float_of_int(y2-y1)) /. phi) + y1
	in (x,y);; 

let divise points t_type =

	let x0,y0 = points.(0)
	and x1,y1 = points.(1)
	and x2,y2 = points.(2) in
	
	match t_type with
		| Obtuse ->
			let x,y = compute_newpoint (x0,y0) (x2,y2) in
            [| [|points.(0) ; points.(1) ; (x,y)|] ;
               [|points.(2) ; (x,y) ; points.(1)|] |]
		| Acute ->
			let x,y = compute_newpoint (x1,y1) (x0,y0)
			and xp,yp = compute_newpoint (x0,y0) (x2,y2) in
			[| [|points.(1) ; points.(2) ; (xp,yp)|] ;
              [|points.(1) ; (xp,yp) ; (x,y)|] ;
              [|points.(0) ; (x,y) ; (xp,yp)|] |]


let rec divide generation points t_type =
    if generation = 0
        then draw points t_type
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
    
divide 6 points Obtuse;;
