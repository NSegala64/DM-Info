#use "topfind";;
#require "graphics";;
#require "unix";;
(*#require "lib"*)

(*open Sdlevent;;*)
open Graphics;;
open Unix;;


let hauteur=720 and largeur=1280;;
open_graph "";resize_window largeur hauteur;;

let t = ref  (Unix.gettimeofday () );;
let dt = ref (1./.60.);;

type point = {
   x : float;
   y : float;
};;

type vecteur = {
   mutable vx : float;
   mutable vy : float;
};;




let add_vecteur vect1 vect2 = {vx = vect1.vx +. vect2.vx ; vy = vect1.vy +. vect2.vy };;
let oppose_vecteur vect = {vx = -.vect.vx; vy = -.vect.vy};;

let substract_vecteur vect1 vect2 = add_vecteur vect1 (oppose_vecteur vect2);;

let produit_scalaire vect1 vect2 = vect1.vx *. vect2.vx +. vect1.vy *. vect2.vy;;

let norme vect = sqrt(produit_scalaire vect vect);;

let unitaire vect = let facteur = norme vect in { vx=vect.vx /. facteur; vy = vect.vy /.facteur;};;

let scale_vecteur_bord vect norme = let u = unitaire vect in vect.vx <- (u.vx *. norme) ; vect.vy <- (u.vy *. norme);;
let scale_vecteur vect norme = let u = unitaire vect in { vx=u.vx *. norme; vy = u.vy *. norme;};;


let origine = ref ({ x = (float_of_int (size_x()/2)); y=(float_of_int (size_y()/2));});;

let vecteur_normal vect = { vx = vect.vy ; vy = -. vect.vx};;

let make_vect_vitesse vect_position norme_v = let normal = vecteur_normal vect_position in  scale_vecteur normal norme_v;;

let zoom = ref ((min !origine.x !origine.y)/.50.) ;;

let make_point x y = { x = x *. !zoom +. !origine.x ; y = y*. !zoom +. !origine.y};;

let make_vecteur point1 point2 = { vx = (point2.x -. point1.x) ; 
                                   vy = (point2.y -. point1.y) ;}
;;


let affiche_vecteur vect = 
   let x,y = vect.vx, vect.vy in 
   (* print_float x; 
   print_string " "; print_float y;
   print_newline();  print_newline();
   *)
   moveto (int_of_float (!origine.x) ) (int_of_float (!origine.y) );
   lineto (int_of_float (!origine.x +. x *. !zoom)) (int_of_float (!origine.y +. y *. !zoom));;

let affiche_cercle vect r couleur_interieur couleur_bord = 
   let x,y,r = int_of_float(!origine.x +. vect.vx *. !zoom), int_of_float (!origine.y +. vect.vy *. !zoom), int_of_float (r *. !zoom) in 
   set_color couleur_interieur;
   fill_circle x y r;
   set_color couleur_bord;
   draw_circle x y r;;



let update vect_position vect_vitesse =
  let position1 = (add_vecteur vect_position vect_vitesse)  in
  let vitesse1 = make_vect_vitesse position1 (norme vect_vitesse) in 
  (position1,vitesse1) ;;

let rotation vect_position vect_centre omega = 
   let vecteur_difference = (substract_vecteur vect_position vect_centre) in 
   let rayon = norme vecteur_difference in 
   let vect_vitesse = make_vect_vitesse vecteur_difference (omega *.rayon) in
      let pos,vit = update vecteur_difference vect_vitesse in
          (add_vecteur (scale_vecteur pos rayon) vect_centre, vit);;


type point_bleu = { mutable pos : vecteur ; mutable vitesse : vecteur ; pr : float};;

type carre_rouge = {mutable cx : float ; mutable cy : float ; cr : float};;


type piece_jaune = { piece_x : float ; piece_y : float ; piece_r : float};;

type mur = { haut_gauche : point ;  haut_droit : point ; bas_gauche : point ; bas_droit : point };;

type zone_verte = {a_remplir : float };;

let update_points_bleus liste_points_bleus = 
   for i=0 to (Array.length liste_points_bleus)-1 do
      let pos,vit = liste_points_bleus.(i).pos, liste_points_bleus.(i).vitesse in
         let pos1,vit1 = update pos vit in 
            liste_points_bleus.(i).pos<-pos1;
            liste_points_bleus.(i).vitesse<-vit1;
   done;
;;

let affiche_points_bleus liste_points_bleus =
   set_color blue;
   for i=0 to (Array.length liste_points_bleus)-1 do
      affiche_cercle liste_points_bleus.(i).pos liste_points_bleus.(i).pr blue black;
   done;
;;
      

let affiche_grille lignes colonnes = 
   let dx = size_x()/colonnes
   and dy = size_y()/lignes in 
   for i = 0 to colonnes do
      moveto (i*dx) 0;
      lineto (i*dx) (size_y());
   done;
   for i = 0 to lignes do
      moveto 0 (i*dy);
      lineto (size_x()) (i*dy);
   done;
;;

let main () = 
   auto_synchronize false;
   set_color black;
   moveto (int_of_float !origine.x) (int_of_float !origine.y);
   
   let point1 = make_point (3.) 2.
   and norme_vit = 0. 
   in
   let pos = ref ((make_vecteur !origine point1)) in
   let vitesse = ref (make_vect_vitesse !pos norme_vit) in 
   affiche_cercle !pos 3. red black;
   print_float !pos.vx ; print_float !pos.vy; print_newline();
   print_float (point1.x) ; print_float point1.y; print_newline();

   let centre = ref (make_vecteur !origine (make_point 3. 3.) ) in 
   while true do
      if Unix.gettimeofday() > !t +. !dt then
         begin
            clear_graph;
            origine :=  { x = (float_of_int (size_x()/2)); y=(float_of_int (size_y()/2));};
            zoom:= ((min !origine.x !origine.y)/.50.);
            (*
            let pos1,vit1 = update !pos !vitesse  in
               affiche_grille 8 8;
               affiche_vecteur pos1;
               affiche_vecteur vit1;
               affiche_cercle !pos 5. blue black;
               
               pos:= pos1; vitesse:=vit1;
            *)
            let pos1, vit1 = rotation !pos !centre 0.1 in
               affiche_grille 15 15;
               affiche_cercle !centre 0.5 black black;
               affiche_vecteur pos1;
               affiche_vecteur vit1;
               affiche_cercle pos1 3. red black;

               pos:= pos1; vitesse:=vit1;

               if button_down() then 
                  begin
                  let centre1,vitesse_centre = rotation !centre (make_vecteur !origine !origine) 0.01 in centre:=centre1; 
                  end;

            synchronize(); 

            t:=Unix.gettimeofday();

         end
   done;
;;

main();;

