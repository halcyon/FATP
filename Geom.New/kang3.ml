
#open "MLgraph";;
#open "complex";;
#open "hyperbolic_geometry";;
#open "circle_limit";;



    



(* +all+ *)

(* +points_du_kangourou+ *)
let tr x = x *. 0.4056 /. 72.0;;
let mk x y = mk_cx (tr x) (tr y);;


let k1 = mk (-3.0) (-0.5)
and k2 = mk (-7.5) (-2.5)
and k3 = mk (-9.0) (-4.0)
and k4 = mk (-9.0)  (-6.0)
and k5 = mk (-8.5)  (-9.0)
and k6 = mk  (-6.5)  (-12.0)
and k7 = mk  (-4.0)  (-14.0)
and k8 = mk  0.0  (-15.0)
and k9 = mk  4.5  (-14.5)
and k91 = mk 8.5 (-13.5)
and k92 = mk 11.5 (-12.5)
and k93 = mk 13.5 (-11.5)
and k10 = mk 18.0  (-8.0)
and k11 = mk 19.5  (-11.5)
and k12 = mk 19.0  (-15.0)
and k13 = mk 16.0  (-18.0)
and k14 = mk 13.5 (-20.0)
and k15 = mk 9.5  (-22.5)
and k16 = mk 5.5  (-24.0)
and k17 = mk 3.0  (-25.5)
and k18 = mk 2.5 (-29.0)
and k19 = mk 4.0 (-31.0)
and k20 = mk 8.0 (-30.0)
and k21 = mk 21.0 (-30.0);;

let [k21';k20';
     k19';k18';k17';k16';k15';k14';k13';k12';k11';k10';
     k93';k92';k91';k9';k8';k7';k6';k5';k4';k3';k2';k1'] 
    = map (apply_hyp_iso rotC) 
    [k21;k20;
     k19;k18;k17;k16;k15;k14;k13;k12;k11;k10;
     k93;k92;k91;k9;k8;k7;k6;k5;k4;k3;k2;k1]
    ;;

let l0 = mk 21.0 (-30.0)
and l1 = mk 24.0 (-30.0)
and l2 = mk 23.5 (-37.5)
and l3 = mk 25.0 (-41.5)
and l4 = mk 28.0 (-44.0)
and l5 = mk 30.0 (-43.0)
and l6 = mk 31.0 (-40.0)
and l7 = mk 31.5 (-35.0)
and l8 = mk  31.0  (-30.0)
and l9 = mk  31.5  (-22.0)
and l10 = mk 34.0  (-16.5)
and l11 = mk 37.0  (-11.0)
and l12 = mk 42.0  (-7.5)
and l13 = mk 46.0  (-5.5)
and l14 = mk 50.0 (-4.0)
and l15 = mk 55.0 (-2.5)
and l16 = mk 60.0 (-2.0)
and l17 = mk 67.0 (-1.0);;


let [l0';l1';l2';l3';l4';l5';l6';l7';l8';l9';l10';l11';l12';
     l13';l14';l15';l16';l17'] 
    = map (fun p -> apply_hyp_iso rotA (apply_hyp_iso rotA p)) 
            [l0;l1;l2;l3;l4;l5;l6;l7;l8;l9;l10;l11;l12;l13;l14;l15;l16;l17] ;;



let m1 = mk 19.0 67.0
and m2 = mk 18.5 61.0
and m3 = mk 19.0 57.5
and m4 = mk 20.0 54.5
and m41 = mk 22.0 52.0
and m42 = mk 23.5 50.0
and m5 = mk 25.5 48.5
and m6 = mk 28.5 46.0
and m7 = mk 36.0 42.5
and m8 = mk  44.0  38.0
and m9 = mk  47.5  35.0
and m10 = mk 50.0  30.0
and m11 = mk 51.0  26.5
and m12 = mk 50.0  23.0
and m13 = mk 46.5  20.0
and m14 = mk 43.0 19.0
and m15 = mk 36.0 19.5
and m16 = mk 29.0 21.5;;


let [m16';m15';m14';m13';m12';m11';m10';
     m9';m8';m7';m6';m5';m42';m41';m4';m3';m2';m1'] 
    = map (apply_hyp_iso rotB) 
    [m16;m15;m14;m13;m12;m11;m10;
     m9;m8;m7;m6;m5;m42;m41;m4;m3;m2;m1]
    ;;



let o1 = mk 27.0 (-28.0)
and o2 = mk 27.0 (-38.0)
and o3 = mk 28.5 (-36.0)
and o4 = mk 28.5 (-30.0);;


let mu1 = mk 5.0 (-30.5)
and mu2 = mk 6.5 (-27.5)
and mu3 = mk 8.0 (-26.5)
and mu4 = mk 9.5 (-27.5)
and mu5 = mk 11.0 (-29.5)
and mu6 = mk 9.0 (-29.5);;








(* +points_du_kangourou+ *)


(* +contour_kangourou+ *)
let contour_kangourou = 
  let  points =
     [ptC;k1;k2;k3;k4;k5;k6;k7;k8;k9;k91;k92;k93;k10;k11;k12;k13;
      k14;k15;k16;k17;k18;k19;k20;k21;
      l1;l2;l3;l4;l5;l6;l7;l8;l9;l10;l11;l12;l13;l14;l15;l16;l17;ptA;
      l17';l16';l15';l14';l13';l12';l11';l10';l9';l8';
      l7';l6';l5';l4';l3';l2';l1';l0';
      m16';m15';m14';m13';m12';m11';m10';m9';m8';m7';m6';
      m5';m42';m41';m4';m3';m2';m1';ptB;m1;m2;m3;m4;m41;m42;
      m5;m6;m7;m8;m9;m10;m11;m12;m13;m14;m15;m16;
      k21'; k20';k19';k18';k17';
      k16';k15';k14';k13';k12';k11';k10';k93';k92';k91';k9';k8';k7';k6';k5';
      k4';k3';k2';k1'; ptC]
  in function t -> 
   let tt z = point_of_cx (apply_hyp_iso t z)
in make_sketch [Seg (map tt points)]
;;

(* +contour_kangourou+ *)

let contours_oeil t = 
let tt z = point_of_cx (apply_hyp_iso t z) 
and centre_oeil = cx_of_point {xc=0.1183;yc= -0.142}
and bord_oeil = cx_of_point {xc=0.134;yc= -0.142}
in let c = tt centre_oeil
   and r= euclidian_distance 
             (apply_hyp_iso t centre_oeil)
             (apply_hyp_iso t bord_oeil)
   in make_sketch[Arc(c, r, 0.0, 360.0)];;

let contours_interieur_oeil t = 
let tt z = point_of_cx (apply_hyp_iso t z) 
and centre_interieur_oeil = cx_of_point {xc=0.11;yc= -0.142}
and bord_interieur_oeil = cx_of_point {xc=0.117;yc= -0.142}
in let c = tt centre_interieur_oeil
   and r= euclidian_distance 
             (apply_hyp_iso t centre_interieur_oeil)
             (apply_hyp_iso t bord_interieur_oeil)
   in make_sketch[Arc(c, r, 0.0, 360.0)];;

let contours_museau =
  let points = [mu1;mu2;mu3;mu4;mu5;mu6;mu1]
  in function t ->
    let tt z = point_of_cx (apply_hyp_iso t z) 
    in make_sketch [Seg (map tt points)];;

let contours_oreille = 
  let points = [o1;o2;o3;o4;o1]
  in function t ->
let tt z = point_of_cx (apply_hyp_iso t z) 
in make_sketch
    [Seg (map tt points)];;


set_default_linewidthcoef 0.01;;
set_default_color black;;

let kang3 color_map (permut,t) =
  let contours_sk =contour_kangourou t
  and oeil_sk = contours_oeil t
  and interieur_oeil_sk = contours_interieur_oeil t
  and museau_sk = contours_museau t
  and oreille_sk = contours_oreille t
  in group_pictures
       [make_fill_picture
          (Eofill, color_map (permut.(0))) contours_sk;
        make_default_draw_picture contours_sk  ;
        make_fill_picture
          (Eofill, white) oeil_sk;
        make_fill_picture
          (Eofill, black) interieur_oeil_sk;
        make_fill_picture
          (Eofill, black) museau_sk;
        make_fill_picture
          (Eofill, black) oreille_sk];;


(* +all+ *)




