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

(* hyperbolic_group_3_3_4.ml:     The formal group for                   *)
(*                                Circle Limit III                       *)



#open "MLgraph";;
#open "complex";;
#open "hyperbolic_geometry";;
#open "symmetry_groups";;

(* +all+ *)

(* +circle_limit_gen+ *)
type circle_limit_gen = TA | TB | TC | Ta | Tb | Tc ;;

(* +circle_limit_gen+ *)
(* +circle_limit_op+ *)

let circle_limit_op = fun
  TA -> (fun [] -> [TA]
           | ((TA::_)|(TC::_)|(Ta::_)) -> raise Group_gen_exc
           | tl -> TA::tl)
| TB -> (fun [] -> [TB]
           | ((TB::_)|(TA::_)|(Tb::_)) -> raise Group_gen_exc
           | tl -> TB::tl)
| TC -> (fun [] -> [TC]
           | ((TB::_)|(Tc::_)|(TC::TC::_)) -> raise Group_gen_exc
           | tl -> TC::tl)
| Ta -> (fun [] -> [Ta]
           | ((Ta::_)|(Tb::_)|(TC::_)|(TB::_)|(TA::_))
             -> raise Group_gen_exc
           | tl -> Ta::tl)
| Tb -> (fun [] -> [Tb]
           | ((TA::_)|(Tc::_)|(Tb::_)|(TC::TC::_)|(TB::_))
             -> raise Group_gen_exc
           | tl -> Tb::tl)
| Tc -> (fun [] -> [Tc]
           | ((TA::_)|(Tc::_)|(Ta::_)|(TB::_)|(TC::_))
             -> raise Group_gen_exc
           | tl -> Tc::tl);;

(* +circle_limit_op+ *)


(* +circle_limit_group+ *)
let circle_limit_group =
  make_tgroup [TA;TB;TC;Ta;Tb;Tc] circle_limit_op;;

(* +circle_limit_group+ *)


(* +color_permut_for_circle_limit+ *)
let CTA = [|2; 0; 1; 3|];;
let CTB = [|2; 1; 3; 0|];;
let CTC = [|1; 0; 3; 2|];;
let CTa = [|1; 2; 0; 3|];;
let CTb = [|3; 1; 0; 2|];;

(* +color_permut_for_circle_limit+ *)
(* +circle_limit_color_group+ *)

let circle_limit_color_group =
  make_ctgroup [TA; TB; TC; Ta; Tb; Tc]
    [CTA; CTB; CTC; CTa; CTb; CTC]
    circle_limit_op;;

(* +circle_limit_color_group+ *)
(* +all+ *)
