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

(* circle_limit.ml:     the hyperbolic isometries involved in the        *)
(*                      Circle limit Pictures                            *)

#open "MLgraph";;
#open "complex";;
#open "hyperbolic_geometry";;
#open "symmetry_groups";;
#open "hyperbolic_group_3_3_4";;

(* +all+ *)

(* +pointP+ *)
let ptP= cx_of_pol (1.0/.sqrt(1.0+.sqrt 2.0)) 22.5;;

(* +pointP+ *)


(* +ABCD+ *)
let ptA= cx_of_pol (sqrt(sinus 7.5 /. cosinus 37.5)) 0.0;;
let ptB= cx_of_pol (sqrt(sinus 7.5 /. cosinus 37.5)) 45.0;;
let ptC= cx_0;;

let rotA= hyp_rotation ptA 120.0;;
let rotB= hyp_rotation ptB 120.0;;
let rotC=  hyp_rotation ptC 90.0;;
let ptD= apply_hyp_iso rotC ptA;;

(* +ABCD+ *)

(* +circle_limit_transfo+ *)
let transfo_memory = ref [([]:circle_limit_gen list), hyp_identity];;
let reset_transfo_memory () = 
  transfo_memory:= [([]:circle_limit_gen list), hyp_identity];;

let  tA = hyp_rotation ptA 120.0
and  tB = hyp_rotation ptB 120.0
and  tC = hyp_rotation ptC 90.0
and  ta = hyp_rotation ptA (-.120.0)
and  tb = hyp_rotation ptB (-.120.0)
and  tc = hyp_rotation ptC (-.90.0);;
let circle_limit_transfo =
  let transfo = fun
    TA -> tA
  | TB -> tB
  | TC -> tC
  | Ta -> ta
  | Tb -> tb
  | Tc -> tc
  in (fun  [] ->   hyp_identity
       | ((t::tl') as tl) -> let h = assoc tl' (!transfo_memory) in
                       let h2 = compose_hyp_iso (transfo t) h in
                         begin transfo_memory:=
                                  (tl,h2)::!transfo_memory;
                                h2
                         end);;
(* +circle_limit_transfo+ *)


(* +euclidian_size+ *)
let euclidian_size t =
   euclidian_distance 
         (apply_hyp_iso t ptA)
         (apply_hyp_iso t ptD);;
(* +euclidian_size+ *)


let rec map_select p f=
  fun []  ->  []
   | (a::l) -> let t = f a in
                if p t then t:: map_select p f l
                 else map_select p f l;;

(* +make_circle_limit_transfos+ *)
let make_circle_limit_transfos n d = 
     reset_transfo_memory ();
     map_select  
            (fun x -> euclidian_size x  >. d)
               circle_limit_transfo
            (generate_transformations_group circle_limit_group n);;
(* +make_circle_limit_transfos+ *)




(* +circle_limit_colored_transfo+ *)
let circle_limit_colored_transfo =
  let transfo = fun
    TA -> tA
  | TB -> tB
  | TC -> tC
  | Ta -> ta
  | Tb -> tb
  | Tc -> tc
  in (fun (ct,tl) ->
            (ct, match tl with
                     [] ->   hyp_identity
                  | (t::tl')
                    -> let h = assq tl' (!transfo_memory) in
                       let h2 = compose_hyp_iso (transfo t) h in
                         begin transfo_memory:=
                                  (tl,h2)::!transfo_memory;
                                h2
                         end));;

(* +circle_limit_colored_transfo+ *)


(* +interpret_circle_limit_colored_group+ *)
let interpret_circle_limit_colored_group d gen_list = 
     reset_transfo_memory ();
     map_select  
            (fun (_,x) -> euclidian_size x  >. d)
               circle_limit_colored_transfo gen_list
(*            (generate_coltrans_group circle_limit_color_group n) *)
;;
(* +interpret_circle_limit_colored_group+ *)

let colors_1 = 
  let col0=Gra 0.3 and col1=Gra 0.5
  and col2=Gra 0.7 and col3=Gra 0.8
  in function 0 -> col0 | 1 -> col1 | 2 -> col2 | 3 -> col3
| _ -> failwith "wrong color";;


let colors_2=
  let c = 255.0 in
  let col0=Rgb (255.0 /. c, 182.0 /. c, 48.0 /. c) 
  and col1=Rgb (110.0 /. c, 156.0 /. c, 84.0 /. c)
  and col2=Rgb (182.0 /. c, 65.0 /. c, 33.0 /. c)
  and col3=Rgb (51.0 /. c, 102.0 /. c, 90.0 /. c)  
  in function 0 -> col0 | 1 -> col1 | 2 -> col2 | 3 -> col3
| _ -> failwith "wrong color";;

let colors_3=
  let c = 255.0 in
  let col0=Rgb (255.0 /. c, 182.0 /. c, 96.0 /. c) 
  and col1=Rgb (140.0 /. c, 186.0 /. c, 114.0 /. c)
  and col2=Rgb (222.0 /. c, 105.0 /. c, 73.0 /. c)
  and col3=Rgb (91.0 /. c, 142.0 /. c, 130.0 /. c)  
  in function 0 -> col0 | 1 -> col1 | 2 -> col2 | 3 -> col3
| _ -> failwith "wrong color";;

(* +all+ *)

