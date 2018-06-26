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

(* complex.ml:          to compute with complex numbers                  *)
(*                                                                       *)


let rec interval m n =
  if m > n then [] else m::interval (m+1) n;;

let rec interval_float x y dx =
 let s= y-.x in
 if lt_float s 0. then failwith "intervalstep" else
   let n =  int_of_float ((y-.x)/.dx) 
   in let rec interv_rec =
             function  ([],r)  -> r
              |  (a::l,r) -> interv_rec (l, x+.(float_of_int a)*.dx::r)
      in interv_rec (rev(interval 0 n),[y]);;

(* +all+ *)
let pi=acos (-1.);;
let sinus a = sin (a*.pi/.180.);;
let cosinus a = cos (a*.pi/.180.);;
let tangente a = tan (a*.pi/.180.);;

(* +type_complex+ *)
type complex= {re_part:float; im_part:float};;
(* +type_complex+ *)
(* +complex+ *)
let mk_cx r i= {re_part=r; im_part=i};;
let cx_1 = mk_cx 1.0 0.0
and cx_0 = mk_cx 0.0 0.0;;
let cx_of_pol rho theta= {re_part=rho*.cosinus theta; 
                          im_part=rho*.sinus theta};;

(* +complex+ *)



(* +operations_complexes+ *)
let conjugate {re_part=r; im_part=i} = {re_part=r; im_part= -.i};;
let module {re_part=r; im_part=i}= sqrt(r*.r +. i*.i);;
let add_cx {re_part=r1; im_part=i1} {re_part=r2; im_part=i2}=
  {re_part=r1+.r2; im_part=i1+.i2};;
let sub_cx {re_part=r1; im_part=i1} {re_part=r2; im_part=i2}=
  {re_part=r1-.r2; im_part=i1-.i2};;
let uminus_cx {re_part=r; im_part=i}= {re_part= -.r; im_part= -.i};;
let mult_cx {re_part=r1; im_part=i1} {re_part=r2; im_part=i2}=
  {re_part=r1*.r2-.i1*.i2; im_part=i1*.r2+.i2*.r1};;
let div_cx {re_part=r1; im_part=i1} {re_part=r2; im_part=i2} =
   let rho= r2*.r2 +. i2*.i2 in
     if rho=0.0 then failwith "cdiv: division by zero"
       else {re_part=(r1*.r2+.i1*.i2)/.rho;
             im_part=(i1*.r2-.r1*.i2)/.rho};;

(* +operations_complexes+ *)



(* +complex_seq+ *)
let complex_seq c1 c2 n =
 let p = (c2.im_part-.c1.im_part) /. (c2.re_part-.c1.re_part)
 and dx = (c2.re_part-.c1.re_part) /. (float_of_int n)
 in (map (fun x -> {re_part= c1.re_part+. x; im_part=c1.im_part+. p*. x})
        (interval_float 0.0 (c2.re_part-.c1.re_part) dx));;
(* +complex_seq+ *)
(* +all+ *)
