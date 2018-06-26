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

(* hyperbolic_geometry.ml:     to compute with hyperbolic isometries     *)
(*                                                                       *)

#open "MLgraph";;
#open "complex";;


(* +all+ *)

(* +type_isometry+ *)
type hyp_isometry= {iso_m:complex; iso_a:complex};;
(* +type_isometry+ *)
(* +iso_id+ *)
let hyp_identity = {iso_m=cx_1;iso_a=cx_0};;
(* +iso_id+ *)



(* +apply_hyp_iso+ *)
let apply_hyp_iso {iso_m=mu; iso_a=a} z=
    mult_cx mu (div_cx (add_cx z a) 
                       (add_cx cx_1 (mult_cx (conjugate a) z)));;


let apply_hyp_iso {iso_m={re_part=rm; im_part=im}; 
                   iso_a={re_part=ra; im_part=ia}} 
                  {re_part=r; im_part=i} =
let ra_conj =ra and ia_conj= -.ia in
let r1 = ra_conj*.r-.ia_conj*.i
and i1 = ia_conj*.r+.i*.ra_conj in
let r2 = r1+.1.
and i2 = i1  in
let r3 = r+.ra
and i3 = i+.ia in
let rho = r2*.r2 +. i2*.i2 in 
let r4 = (r3*.r2+.i3*.i2)/.rho
and i4 = (i3*.r2-.r3*.i2)/.rho in
{re_part=rm*.r4-.im*.i4; im_part=im*.r4+.i4*.rm};;

(* +apply_hyp_iso+ *)


(* +compose_hyp_iso+ *)
let compose_hyp_iso {iso_m=mu1; iso_a=a1} {iso_m=mu2; iso_a=a2}=
  {iso_m= div_cx  (mult_cx mu2 (add_cx mu1 (mult_cx a2 (conjugate a1))))
                   (add_cx cx_1 (mult_cx mu1 (mult_cx (conjugate a2) a1))); 
   iso_a= div_cx (add_cx a2 (mult_cx mu1 a1)) 
                 (add_cx mu1 (mult_cx a2 (conjugate a1)))};;

(* +compose_hyp_iso+ *)




(* +hyp_rotation+ *)
let hyp_rotation center angle=
  compose_hyp_iso 
     {iso_m=cx_1;iso_a=uminus_cx center}
     (compose_hyp_iso 
           {iso_m=mk_cx (cosinus angle) (sinus angle); 
            iso_a=cx_0}
           {iso_m=cx_1; iso_a=center});;

(* +hyp_rotation+ *)


(* +euclidian_distance+ *)
let euclidian_distance {re_part=r1; im_part=i1}
                       {re_part=r2; im_part=i2}
= let x=r1-.r2 and y=i1-.i2
  in sqrt(x*.x+.y*.y);;
(* +euclidian_distance+ *)


(* +point_of_cx+ *)
let point_of_cx {re_part=r; im_part=i} = {xc=r; yc=i};;
let cx_of_point {xc=r; yc=i} = {re_part=r; im_part=i};;

(* +point_of_cx+ *)

(* +all+ *)

(* ++ *)
(* ++ *)
(* ++ *)
(* ++ *)
