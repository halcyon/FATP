
#open "MLgraph";;
#open "complex";;
#open "hyperbolic_geometry";;
#open "circle_limit";;



(* +all+ *)

(* +points_du_poisson+ *)
let p0	=ptA
and p1	=mk_cx	(0.32)	(-0.018)
and p2	=mk_cx	(0.24)	(0.0)
and p3	=mk_cx	(0.2)	(-0.1)
and p4	=mk_cx	(0.09)	(-0.14);;
let p5	=ptC
and p6	=apply_hyp_iso rotC p4
and p7	=apply_hyp_iso rotC p3
and p8	=apply_hyp_iso rotC p2
and p9	=apply_hyp_iso rotC p1
and p10	=apply_hyp_iso rotC p0;;
let p11	=mk_cx	(0.08)	(0.355)
and p12	=mk_cx	(0.155)	(0.338)
and p13	=mk_cx	(0.18)	(0.25)
and p14	=mk_cx	(0.25)	(0.2);;
let p15	=ptB
and p16	=apply_hyp_iso rotB p14
and p17	=apply_hyp_iso rotB p13
and p18	=apply_hyp_iso rotB p12
and p19	=apply_hyp_iso rotB p11;;

(* +points_du_poisson+ *)


(* +interieur_du_poisson+ *)
let q1	=mk_cx	(0.26)	(0.09);;
let q2	=mk_cx	(0.16)	(0.19);;
let q3	=mk_cx	(0.08)	(0.28);;
let q4	=mk_cx	(0.22)	(0.02);;
let q5	=mk_cx	(0.16)	(0.07);;
let q6	=mk_cx	(0.305)	(0.17);;
let q7	=mk_cx	(0.27)	(0.19);;
let q8	=mk_cx	(0.09)	(0.22);;
let q9	=mk_cx	(0.08)	(0.25);;
let q10	=mk_cx	(0.11)	(0.285);;
let q11	=mk_cx	(0.16)	(0.26);;
let q12	=mk_cx	(0.34)	(0.01);;
let q13	=mk_cx	(0.31)	(0.01);;
let q14	=mk_cx	(0.32)	(0.03);;
let q15	=mk_cx	(0.365)	(0.05);;
let q16	=mk_cx	(0.35)	(0.08);;
let q17	=mk_cx	(0.338)	(0.068);;

let r1	=mk_cx	(0.15)	(0.065);;
let r2	=mk_cx	(0.09)	(0.005);;
let r3	=mk_cx	(0.055)	(-0.055);;
let r4	=mk_cx	(0.18)	(0.04);;
let r5	=mk_cx	(0.12)	(-0.025);;
let r6	=mk_cx	(0.1)	(-0.105);;
let r7	=mk_cx	(0.215)	(0.015);;
let r8	=mk_cx	(0.165)	(-0.05);;
let r9	=mk_cx	(0.16)	(-0.095);;
let r10	=mk_cx (0.3)	(0.175);;
let r11	=mk_cx	(0.35)	(0.25);;
let r12	=mk_cx	(0.275)	(0.195);;
let r13	=mk_cx	(0.315)	(0.275);;
let r14	=mk_cx	(0.035)	(0.29);;
let r15	=mk_cx	(0.09)	(0.34);;

(* +interieur_du_poisson+ *)


(* +corps+ *)
let corps t = let tt z = point_of_cx (apply_hyp_iso t z)
in make_sketch [Seg (map tt [p0;p1;p2;p3;p4;p5;p6;p7;p8;p9;
                             p10;p11;p12;p13;p14;p15;p16;p17;
                             p18;p19;p0])];;

(* +corps+ *)


(* +quad+ *)
let quad t = let tt z = point_of_cx (apply_hyp_iso t z)in
make_sketch [Seg (map tt [ptA;ptB;ptD;ptC;ptA])];;

(* +quad+ *)


(* +dessin+ *)
let dessin t = let tt z = point_of_cx (apply_hyp_iso t z)
in
group_sketches [
make_sketch [Seg (map tt [p0;q1;q2;q3;p10])];
make_sketch [Seg (map tt [q4;q5])];
make_sketch [Seg (map tt [q6;q7])];
make_sketch [Seg (map tt [q8;q9;q10;q11])];
make_sketch [Seg (map tt [q12;q13;q14;q12])];
make_sketch [Seg (map tt [q15;q16;q17;q15])];
make_sketch [Seg (map tt [r1;r2;r3])];
make_sketch [Seg (map tt [r4;r5;r6])];
make_sketch [Seg (map tt [r7;r8;r9])];
make_sketch [Seg (map tt [r10;r11])];
make_sketch [Seg (map tt [r12;r13])];
make_sketch [Seg (map tt [q9;r14])];
make_sketch [Seg (map tt [q10;r15] )]
];;

(* +dessin+ *)




(* +le_poisson_pour_escher+ *)

let poisson color_map (permut,t) =
  let contours_sk = corps t
  and decor_sk = dessin t
  in group_pictures
       [make_fill_picture
          (Nzfill, color_map (permut.(0))) contours_sk;
        make_default_draw_picture
          (group_sketches [contours_sk; decor_sk])];;

(* +le_poisson_pour_escher+ *)

(* +all+ *)

