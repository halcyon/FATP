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

(* p1_group.ml:     The symmetry group p1                                *)
(*                                                                       *)


#open "MLgraph";;
#open "symmetry_groups";;

(* +all+ *)
(* +p1_gen+ *)
type p1_gen = TA | TB | Ta | Tb;;
(* +p1_gen+ *)
(* +p1_group+ *)
let p1_op = fun
            TA  -> (fun [] -> [TA]
                        | (((TA|TB|Tb)::tl) as tl') -> TA::tl'
                        | (Ta::_) -> raise Group_gen_exc)
     |      TB  -> (fun [] -> [TB]
                        | ((TB::tl) as tl') -> TB::tl'
                        | ((TA|Ta|Tb)::_) -> raise Group_gen_exc)
     |      Ta  -> (fun [] -> [Ta]
                        | (((Ta|TB|Tb)::tl) as tl') -> Ta::tl'
                        | (TA::_) -> raise Group_gen_exc)
     |      Tb  -> (fun [] -> [Tb]
                        | ((Tb::tl) as tl') -> Tb::tl'
                        | ((TA|Ta|TB)::_) -> raise Group_gen_exc);;
let p1_group =
  {tgroup_gens= [TA;TB;Ta;Tb];
   tgroup_op= p1_op};;
(* +p1_group+ *)
(* +all+ *)
