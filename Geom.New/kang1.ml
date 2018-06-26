
#open "MLgraph";;
#open "complex";;
#open "hyperbolic_geometry";;
#open "circle_limit";;
    
(* +all+ *)

(* +points_du_kangourou+ *)
let k1 = mk_cx 0.3 0.06
and k2 = mk_cx 0.22 0.06
and k3 = mk_cx 0.1 0.013
and k41 = mk_cx 0.06 (-.0.04)
and k42 = mk_cx 0.05 (-.0.08)
and k4 = mk_cx 0.06 (-.0.1);;
let [k4';k42';k41';k3';k2';k1'] 
    = map (apply_hyp_iso rotC) [k4;k42;k41;k3;k2;k1];;
let k5 = mk_cx 0.34 0.254
and k6 = mk_cx 0.44 0.232
and k7 = mk_cx 0.49 0.230
and k8 = mk_cx 0.5 0.230
and k81 = mk_cx 0.52 0.25
and k82 = mk_cx 0.53 0.24
and k83 = mk_cx 0.53 0.2
and k9 = mk_cx 0.52 0.16
and k10 = mk_cx 0.51 0.13
and k11 = mk_cx 0.496 0.156
and k12 = mk_cx 0.485 0.172
and k13 = mk_cx 0.472 0.143
and k14 = mk_cx 0.475 0.12
and k15 = mk_cx 0.483 0.1
and k16 = mk_cx 0.483 0.09
and k17 = mk_cx 0.48 0.08
and k18 = mk_cx 0.47 0.074
and k19 = mk_cx 0.463 0.08
and k20 = mk_cx 0.47 0.1
and k21 = mk_cx 0.463 0.112
and k22 = mk_cx 0.45 0.1
and k23 = mk_cx 0.44 0.06
and k24 = mk_cx 0.425 0.03;;
let [k24';k23';k22';k21';k20';k19';k18';k17';k16';k15';
     k14';k13';k12';k11';k10';k9';k83';k82';k81';k8';k7';k6';k5'] 
    = map (fun p -> apply_hyp_iso rotB (apply_hyp_iso rotB p)) 
            [k24;k23;k22;k21;k20;k19;k18;k17;k16;k15;
             k14;k13;k12;k11;k10;k9;k83;k82;k81;k8;k7;k6;k5] ;;
let p1= mk_cx 0.34 0.14
and p2= mk_cx 0.35 0.07
and o1= mk_cx 0.515 0.21
and o2= mk_cx 0.52 0.20
;;

(*
let ik1 = mk_cx 0.465 0.152
and ik2 = k12
and ik3 = mk_cx 0.36 0.085
and ik4 = mk_cx 0.36 0.085
and ik3 = mk_cx 0.36 0.085
*)
(* +points_du_kangourou+ *)


(* +contour_kangourou+ *)
let contour_kangourou t = let tt z = point_of_cx (apply_hyp_iso t z)
in group_sketches
    [make_sketch [Seg (map tt 
     [ptA;k1;k2;k3;k41;k42;(*k4;*)ptC(*;k4'*);k42';k41';k3';k2';k1';ptD;
      k24';k23';k22';k21';k20';k19';k18';k17';k16';
      k15';k14';k13';k11';k10';k9';k83';k82';k81';k8';k7';k6';k5';ptB;
      k5;k6;k7;k8;k81;k82;k83;k9;k10;k11;k12;k11;k13;k14;k15;k16;k17;
      k18;k19;k20;k21;k22;k23;k24;ptA])];
     make_sketch [Seg (map tt [p1;p2;ptA;p2;p1])];
     make_sketch [Seg (map tt [o1;o2])]
];;

(* +contour_kangourou+ *)


set_default_linewidthcoef 0.001;;
set_default_color black;;

let kang1  color_map (permut,t) =
  let contours_sk =contour_kangourou t
  in  group_pictures
       [make_fill_picture
          (Nzfill, color_map (permut.(0))) contours_sk;
        make_default_draw_picture contours_sk];;

(* +all+ *)


