

#open "MLgraph";;
#open "complex";;
#open "hyperbolic_geometry";;
#open "circle_limit";;



(* +all+ *)

(* +points_du_kangourou+ *)
let tr x = x *. 0.4056 /. 47.0;;
let mk x y = mk_cx (tr x) (tr y);;


let k1 = mk (-2.0) (-3.0)
and k2 = mk (-2.5) (-5.5)
and k3 = mk (-0.5) (-8.0)
and k4 = mk (3.0)  (-10.0)
and k5 = mk (6.5)  (-9.5)
and k6 = mk  10.0  (-9.0)
and k7 = mk  13.0  (-7.5)
and k8 = mk  12.5  (-11.0)
and k9 = mk  10.0  (-12.5)
and k10 = mk 7.5  (-13.5)
and k11 = mk 5.0  (-14.5)
and k12 = mk 2.5  (-15.5)
and k13 = mk 1.0  (-18.0)
and k14 = mk (-0.5) (-19.5)
and k15 = mk 0.0  (-20.5)
and k16 = mk 1.5  (-21.0)
and k17 = mk 6.5  (-21.4)
and k18 = mk 15.5 (-23.0)
and k19 = mk 16.0 (-29.0)
and k20 = mk 15.0 (-33.5)
and k21 = mk 15.1 (-36.5)
and k22 = mk 20.0 (-34.0)
and k23 = mk 21.5 (-33.0)
and k24 = mk 22.0 (-29.0)
and k25 = mk 21.0 (-25.0)
and k26 = mk 21.0 (-21.0)
and k27 = mk 21.5 (-18.0)
and k28 = mk 24.5 (-14.0)
and k29 = mk 28.0 (-11.5)
and k30 = mk 37.0 (-8.0);;

let [k30';k29';k28';k27';k26';k25';k24';k23';k22';k21';k20';
     k19';k18';k17';k16';k15';k14';k13';k12';k11';k10';
     k9';k8';k7';k6';k5';k4';k3';k2';k1'] 
    = map (apply_hyp_iso rotC) 
    [k30;k29;k28;k27;k26;k25;k24;k23;k22;k21;k20;
     k19;k18;k17;k16;k15;k14;k13;k12;k11;k10;
     k9;k8;k7;k6;k5;k4;k3;k2;k1]
    ;;
let l1 = mk 46.0 5.0
and l2 = mk 44.5 11.0
and l3 = mk 45.5 18.0
and l4 = mk 46.8 26.0
and l5 = mk 45.0 26.1
and l6 = mk 39.0 28.5
and l7 = mk 36.0 30.5;;

let [l1';l2';l3';l4';l5';l6';l7'] 
    = map (fun p -> apply_hyp_iso rotB (apply_hyp_iso rotB p)) 
            [l1;l2;l3;l4;l5;l6;l7] ;;

(* +points_du_kangourou+ *)


(* +contour_kangourou+ *)
let contour_kangourou t = let tt z = point_of_cx (apply_hyp_iso t z)
in make_sketch [Seg (map tt 
     [ptC;k1;k2;k3;k4;k5;k6;k7;k8;k9;k10;k11;k12;k11;k13;
      k14;k15;k16;k17;k18;k19;k20;k21;k22;k23;k24;k25;k26;
      k27;k28;k29;k30;ptA;l1;l2;l3;l4;l5;l6;l7;ptB;
      l7';l6';l5';l4';l3';l2';l1';ptD; k30';k29';k28';
      k27';k26';k25';k24';k23';k22';k21';k20';k19';k18';k17';
      k16';k15';k14';k13';k11';k10';k9';k8';k7';k6';k5';
      k4';k3';k2';k1';ptC])];;

(* +contour_kangourou+ *)


let centre_oeil = {xc=0.12;yc= -.0.177};;
let oeil =
 group_pictures
    [make_fill_picture
       (Nzfill, white)
       (make_sketch[Arc({xc=0.12;yc= -.0.177}, 0.0155, 0.0, 360.0)]);
     make_fill_picture
       (Nzfill, black)
       (make_sketch[Arc({xc=0.122;yc= -.0.174}, 0.0077, 0.0, 360.0)])];;

set_default_linewidthcoef 0.001;;
set_default_color white;;

let kang2 color_map (permut,t) =
  let contours_sk =contour_kangourou t
  in group_pictures
       [make_fill_picture
          (Nzfill, color_map (permut.(0))) contours_sk;
        make_default_draw_picture contours_sk  ;
        center_picture
          (let c= euclidian_size t /. euclidian_distance ptA ptD
           in scale_picture (c,c) oeil)
          (point_of_cx (apply_hyp_iso t (cx_of_point centre_oeil)))
           ];;
(* +all+ *)

