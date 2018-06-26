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

(* p2_group.ml:     The symmetry group p2                                *)
(*                                                                       *)


#open "MLgraph";;
#open "symmetry_groups";;

(* +all+ *)

(* +p2_gen+ *)
type p2_gen = TA | TB | TC | Ta | Tb | Tc;;
(* +p2_gen+ *)
(* +p2_group+ *)
let p2_op =  fun
            TA  -> (fun [] -> [TA]
                        | (((TA|TB|TC|Tb|Tc)::tl) as tl') -> TA::tl'
                        |  (Ta::_) -> raise Group_gen_exc)
     |      TB  -> (fun [] -> [TB]
                        | (((TB|TC|Tc)::tl) as tl') -> TB::tl'
                        | ((TA|Ta|Tb)::_) -> raise Group_gen_exc)
     |      TC  -> (fun [] -> [TC]
                        | ((TA|TB|TC|Ta|Tb|Tc)::_) -> raise Group_gen_exc)
     |      Ta  -> (fun [] -> [Ta]
                        | (((Ta|TB|TC|Tb|Tc)::tl) as tl') -> Ta::tl'
                        |  (TA::_) -> raise Group_gen_exc)
     |      Tb  -> (fun [] -> [Tb]
                        | (((Tb|TC|Tc)::tl) as tl') -> Tb::tl'
                        | ((TA|Ta|TB)::_) -> raise Group_gen_exc)
     |      Tc  -> (fun [] -> [TC]
                        | ((TA|TB|TC|Ta|Tb|Tc)::_) -> raise Group_gen_exc)
;;
let p2_group =
  {tgroup_gens= [TA;TB;TC;Ta;Tb;Tc];
   tgroup_op= p2_op};;

(* +p2_group+ *)


(* +color_for_p2_ctgroup+ *)
let ct1= [|1;0;2|] and ct2= [|2;1;0|];;

(* +color_for_p2_ctgroup+ *)


(* +p2_ctgroup+ *)
let p2_ctgroup =
  make_ctgroup [TA;TB;TC;Ta;Tb;Tc] 
               [cpermut ct1 ct1; cpermut ct1 ct2 ;ct1;
                cpermut ct1 ct1; cpermut ct2 ct1 ; ct1] 
               p2_op;;

(* +p2_ctgroup+ *)


(* +all+ *)
