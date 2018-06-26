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

(* symmetry_groups.ml:  to define a symmetry group and generate it       *)
(*                                                                       *)



#open "../Util/prelude";;
#open "MLgraph";;
#open "permutations";;
#infix "ctrans";;


(* +all+ *)

(* +tgroup+ *)
type 'a tgroup =
  {tgroup_gens:'a list;
   tgroup_op: 'a -> 'a list -> 'a list};;
(* +tgroup+ *)
(* +make_tgroup+ *)
let make_tgroup trl top =
  {tgroup_gens=trl;
   tgroup_op=top};;
(* +make_tgroup+ *)


let pairing f g = fun
  (x,y) (x',y') -> (f x x', g y y');;



(* +group_gen_exc+ *)
exception Group_gen_exc;;
(* +group_gen_exc+ *)


(* +power+ *)
let rec map_try f = fun
     [] -> [] 
   | (a::l) -> if (try f a; true with _ -> false)
                  then (f a) :: map_try f l
                  else map_try f l;;

let rec product f l1 l2 =
  match l1 with
        []  -> []
    | (a::l) -> map_try (f a) l2 @ product f l l2;;
    
let power f x l = 
pwr [x]
where rec pwr ll = fun
          0   ->  ll
   |      n   ->  ll@ pwr (product f l ll) (n-1);;

(* +power+ *)


(* +generate_transformations_group+ *)
let generate_transformations_group tgroup=
   power tgroup.tgroup_op [] tgroup.tgroup_gens;;

(* +generate_transformations_group+ *)


(* +ctgroup+ *)
type 'a ctgroup =
  {ctgroup_tgens: 'a list;
   ctgroup_colgens: int vect list;
   ctgroup_top: 'a  -> 'a list  -> 'a list};;
   
let make_ctgroup trl ctrl top =
 if list_length trl <> list_length ctrl
    or list_length trl=0
    or (let n = vect_length (hd ctrl)
        in exists (fun v -> vect_length v <> n) (tl ctrl))
  then failwith "make_ctgroup: wrong ctgroup"
  else  {ctgroup_tgens=trl;
   ctgroup_colgens=ctrl;
   ctgroup_top=top};;

(* +ctgroup+ *)



(* +power+ *)
let rec map_try f = fun
     [] -> [] 
   | (a::l) -> if (try f a; true with _ -> false)
                  then (f a) :: map_try f l
                  else map_try f l;;

    
let power f x l = 
pwr [x]
where rec pwr ll = fun
          0   ->  ll
   |      n   ->  ll@ pwr (product f l ll) (n-1);;

(* +power+ *)


(* +generate_coltrans_group+ *)
let generate_coltrans_group ctgroup =
  let k= vect_length (hd ctgroup.ctgroup_colgens)
  in
     power (pairing cpermut ctgroup.ctgroup_top)
         (id_permut k,[])
         (combine (ctgroup.ctgroup_colgens, ctgroup.ctgroup_tgens));;

(* +generate_coltrans_group+ *)

(* +all+ *)

