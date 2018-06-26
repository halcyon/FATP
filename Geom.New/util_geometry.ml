
#open "../Util/prelude";;
#open "MLgraph";;
#infix "ctrans";;

(* +point_translation+ *)
let point_translation = fun
  {xc=x1; yc=y1} {xc=x2; yc=y2} -> translation (x2-.x1, y2-.y1);;
(* +point_translation+ *)


(* +comp_trans+ *)
let comp_trans t1 t2 = compose_transformations [t2;t1];;
(* +comp_trans+ *)

(* +points_of_arc+ *)

let points_of_arc = fun
  (Arc (center,radius,a1,a2)) n
  -> let a = (a2 -. a1) /. (float_of_int n) in
     let pt1= transform_point
                (point_translation origin center)
                {xc= radius *. (cosinus a1); yc= radius *. (sinus a1)}
     in (map (fun a -> transform_point (rotation center a) pt1)
           (interval_float a1 a2 a));;


(* +points_of_arc+ *)

(* +transform_points+ *)
let xunit = {xc=1.0; yc=0.0};;
let rec transform_points t pts =
 map (transform_point t) pts;;

(* +transform_points+ *)
