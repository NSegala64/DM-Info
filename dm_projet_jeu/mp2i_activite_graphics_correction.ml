#load "graphics.cma";;
open Graphics;;

(* Un petit test *)
open_graph "";;

set_color (rgb 255 0 255);;

fill_rect 0 0 (size_x ()) (size_y());;
(* Question 1 *)
let psychedelique n = 
	for i = 1 to n do 
		let largeur = 1+ Random.int (size_x()/2)
		and hauteur = 1+ Random.int (size_y() /2)
		and couleur = rgb (Random.int 256) (Random.int 256) (Random.int 256) in
			let x = Random.int (size_x() - largeur)
			and y = Random.int (size_y() - hauteur) in
				set_color couleur;
				fill_rect x y largeur hauteur;		
	done;;

psychedelique 50;;
(* Question 2 *)
open_graph "";;
let plot x y = fill_rect (x*10) (y*10) 10 10;;
(* sans flottants *)
let trace x1 y1 x2 y2 =
	let b = y1-(y2-y1)*x1/(x2-x1) in
		for x = x1 to x2 do
			let y = (y2-y1)*x/(x2-x1) + b in
				plot x y
		done;;
		
(* sans multiplications/divisions (et sans flottants) *)
let trace_bresenhamn x1 y1 x2 y2 = 
	let dx = x2 - x1
	and dy = y2 - y1 
	and y = ref y1
	and reste = ref 0 in
		for x = x1 to x2 do
			plot x !y;
			reste := !reste + dy;
			if !reste >= dx then
				begin
					reste := !reste - dx;
					incr y
				end
		done;;
		
trace_bresenhamn 2 2 50 40;;


(* Un petit jeu *)
let alea_dans_fenetre marge =
	let x = marge + Random.int (size_x() - 2*marge)
	and y = marge + Random.int (size_y() - 2*marge) in
		x, y;;

let distance2 (x1, y1) (x2, y2) = 
	let dx = x1- x2 and dy = y1-y2 in
		dx * dx  + dy * dy;;
		
wait_next_event [Button_up];;

let jeu () =  
	open_graph "";
	let r = ref 100 in
		while true do
			let a,b = alea_dans_fenetre !r in
				clear_graph ();
				fill_circle a b !r ;
				let dedans = ref false in 
					while not !dedans do
						let clic = wait_next_event [Button_up] in
							dedans := distance2 (a,b) (clic.mouse_x, clic.mouse_y) <= !r * !r;
					done;
				r := !r-5;
		done;;
		
jeu ();;

