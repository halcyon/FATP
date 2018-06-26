(*************************************************************************)
(*                                                                       *)
(*                          MLgraph ' library                            *)
(*                                                                       *)
(*                        Computing tilings in ML                        *)
(*                                                                       *)
(*************************************************************************)
(*                                                                       *)
(*                        Guy COUSINEAU                                  *)
(*                            LIENS                                      *)
(*                        45 rue d'Ulm                                   *)
(*                         75005 PARIS                                   *)
(*                            France                                     *)
(*                                                                       *)
(*************************************************************************)

(* load_escher_stuff.ml:     to obtain hyperbolic tilings                *)
(*                           similar to Escher's Circle Limit III        *)



#open "MLgraph";;
change_graphics_directory  "/usr/local/lib/caml-light7/";;
#directory "../Util/";;

load_object "MLgraph";;
#open "MLgraph";;
load_object "book_prelude";;
#open "book_prelude";;

load_object "permutations";;
#open "permutations";;
load_object "symmetry_groups";;
#open "symmetry_groups";;

load_object "hyperbolic_group_3_3_4";;
#open "hyperbolic_group_3_3_4";;

load_object "complex";;
#open "complex";;
load_object "hyperbolic_geometry";;
#open "hyperbolic_geometry";;
load_object "circle_limit";;
#open "circle_limit";;

load_object "poisson";;
#open "poisson";;
load_object "kang1";;
#open "kang1";;
load_object "kang2";;
#open "kang2";;
load_object "kang3";;
#open "kang3";;


(* +all+ *)

let make_tiling generate interpret tile n =
 group_pictures (map tile (interpret (generate n)));;

(* The most natural fonction *)
let make_circle_limit_picture  motif depth min_size =
  make_tiling 
    (generate_coltrans_group circle_limit_color_group)
    (interpret_circle_limit_colored_group min_size)
    motif
    depth;;

let message s = output_string std_out s; output_string std_out "\n";
                flush std_out;;

(* A modified function to memoize the transformations *)
let make_circle_limit_picture  motif depth min_size =
  let transfos =
    let file_name = ("circle_limit_transfos" ^
                     "_" ^ (string_of_int depth) ^ 
                     "_" ^ (string_of_float min_size))
    in try let ch = open_in file_name
           in input_value ch
       with _ -> message "Recomputing transformations";
                 let transfos 
                   = interpret_circle_limit_colored_group 
                      min_size
                      (generate_coltrans_group circle_limit_color_group depth)
                 in let ch = open_out file_name
                    in  message "Saving transformations";
                        output_value ch transfos;close_out ch; transfos
  in  message "Computing tiling";
      group_pictures (map motif transfos);;


let make_circle_limit_picture_with_circle motif color depth min_size =
  group_pictures
     [make_fill_picture
         (Nzfill, color)
             (make_sketch[Arc(origin, 0.99, 0.0, 360.0)]);
      make_circle_limit_picture  motif depth min_size];;

let size_up  x = scale_picture (x,x);;


let escher_poisson color_map n d =
 size_up 250. (make_circle_limit_picture (poisson color_map) n d);;

let escher_poisson_avec_cercle color_map n d =
 size_up 250. 
   (make_circle_limit_picture_with_circle 
       (poisson color_map) (color_map 0) n d);;

let escher_kangourou color_map n d =
 size_up 250. (make_circle_limit_picture (kang3 color_map) n d);;

let escher_kangourou_avec_cercle color_map n d =
 size_up 250. 
   (make_circle_limit_picture_with_circle 
      (kang3 color_map) (color_map 0) n d);;


set_default_linewidthcoef 0.001;;
set_default_color black;;

         

let kangourou_original = 
  size_up 200.0
     (hflip_picture (kang3  colors_3  (id_permut 4,hyp_identity)));;

(*  Example

let k= escher_kangourou colors_3 2 0.1 ;;
*)

(* +all+ *)
