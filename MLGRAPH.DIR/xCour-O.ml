(*                                                                       *)
(*                     Projet      Formel                                *)
(*                                                                       *)
(* CAML-light: MLgraph library *)
(*                                                                       *)
(*************************************************************************)
(*                                                                       *)
(*                            LIENS                                      *)
(*                        45 rue d'Ulm                                   *)
(*                         75005 PARIS                                   *)
(*                            France                                     *)
(*                                                                       *)
(*************************************************************************)


#open "MLgraph";;


let fn = 
(*Begin Font Description*)
Courier_Oblique,{font_descr_filename="preloaded"; font_descr_name="Courier_Oblique";
font_descr_width=7.2;
font_descr_height=12.0;
font_descr_descr= vect_of_list [
  ]; font_descr_descr_bbox= vect_of_list [
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (2.84731,-0.179993),(5.47899,6.864));
( (3.27263,3.936),(6.38948,6.744));
( (1.70248,-0.384003),(7.04832,7.668));
( (1.38225,-1.51199),(7.07349,7.944));
( (1.76341,-0.179993),(7.1467,7.464));
( (0.954773,-0.179993),(6.86102,6.51601));
( (3.39262,3.936),(5.94548,6.744));
( (3.65634,-1.29601),(6.78253,7.464));
( (1.72853,-1.29601),(4.85471,7.464));
( (2.54491,3.084),(6.96091,7.284));
( (1.5414,0.0),(6.74776,5.112));
( (1.88632,-1.34399),(4.43919,1.464));
( (1.92706,2.856),(6.5891,3.336));
( (2.77095,-0.179993),(4.66881,1.308));
( (1.33165,-0.960007),(7.26866,7.548));
( (1.59375,-0.179993),(7.13054,7.464));
( (1.25999,0.0),(6.06271,7.464));
( (0.923996,0.0),(6.95557,7.464));
( (1.06816,-0.179993),(6.56512,7.464));
( (1.4386,0.0),(6.38654,7.464));
( (1.26198,-0.179993),(7.06827,7.284));
( (1.68974,-0.179993),(7.55054,7.464));
( (2.28976,0.0),(7.26027,7.284));
( (1.41513,-0.179993),(7.18327,7.464));
( (1.19774,-0.179993),(7.05853,7.464));
( (2.77095,-0.179993),(5.37279,4.62));
( (1.88632,-1.34399),(5.37279,4.62));
( (1.03529,-0.0240021),(7.23807,5.136));
( (1.30162,1.21201),(6.98499,3.89999));
( (1.04852,-0.0240021),(7.25128,5.136));
( (2.61931,-0.179993),(7.0979,6.864));
( (1.2146,-0.179993),(7.0768,7.464));
( (0.119995,0.0),(7.16672,6.744));
( (0.600006,0.0),(7.45204,6.744));
( (0.894836,-0.216003),(7.80728,6.96001));
( (0.600006,0.0),(8.03088,6.744));
( (0.720001,0.0),(7.84149,6.744));
( (0.720001,0.0),(7.84149,6.744));
( (0.774826,-0.216003),(7.68729,6.96001));
( (0.468002,0.0),(8.16548,6.744));
( (1.23599,0.0),(7.39749,6.744));
( (0.709686,-0.216003),(8.14148,6.744));
( (0.539993,0.0),(7.97348,6.744));
( (0.647995,0.0),(7.17616,6.744));
( (0.132004,0.0),(8.50148,6.744));
( (0.167999,-0.156006),(8.46548,6.744));
( (0.842499,-0.216003),(7.791,6.96001));
( (1.032,0.0),(7.72665,6.744));
( (0.897095,-1.548),(7.791,6.96001));
( (0.539993,0.0),(7.25349,6.744));
( (0.993744,-0.240005),(7.7179,6.96001));
( (1.39447,0.0),(7.90149,6.744));
( (1.30862,-0.216003),(8.34549,6.744));
( (1.38275,-0.156006),(8.59749,6.744));
( (1.38275,-0.156006),(8.59749,6.744));
( (0.360001,0.0),(8.02148,6.744));
( (1.71875,0.0),(8.26149,6.744));
( (1.032,0.0),(7.32549,6.744));
( (3.03653,-1.29601),(6.89053,7.464));
( (3.06866,-0.960007),(5.53165,7.548));
( (1.62053,-1.29601),(5.47453,7.464));
( (2.10492,4.308),(7.04892,7.464));
( (-0.318832,-1.5),(7.0087,-0.899994));
( (3.52463,3.936),(6.07748,6.744));
( (0.819473,-0.179993),(6.71071,5.29201));
( (0.432007,-0.179993),(7.69344,7.548));
( (1.06221,-0.179993),(7.28954,5.29201));
( (0.863754,-0.179993),(7.59239,7.548));
( (1.06221,-0.179993),(7.22601,5.29201));
( (1.452,0.0),(7.85922,7.548));
( (0.821167,-1.884),(7.79459,5.29201));
( (0.479996,0.0),(6.98671,7.548));
( (1.224,0.0),(6.06271,7.884));
( (0.710892,-1.884),(6.5118,7.884));
( (0.779999,0.0),(7.50659,7.548));
( (1.224,0.0),(6.06271,7.548));
( (0.0240021,0.0),(7.47523,5.29201));
( (0.395996,0.0),(6.90271,5.29201));
( (0.930206,-0.179993),(7.36659,5.29201));
( (-0.20845,-1.884),(7.45345,5.29201));
( (0.863754,-1.884),(8.09459,5.29201));
( (0.804001,0.0),(7.61563,5.29201));
( (1.09995,-0.179993),(6.83865,5.29201));
( (1.78156,-0.179993),(6.40259,6.73199));
( (1.33586,-0.179993),(6.74672,5.112));
( (1.20386,-0.119995),(8.0826,5.112));
( (0.987869,-0.119995),(8.2986,5.112));
( (0.324005,0.0),(7.7706,5.112));
( (-0.364456,-1.884),(7.71059,5.112));
( (1.272,0.0),(7.0266,5.112));
( (2.87506,-1.29601),(6.74654,7.464));
( (3.17995,-0.960007),(5.43239,7.548));
( (1.76453,-1.29601),(5.636,7.464));
( (1.32179,1.92),(7.02272,3.228));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.119995,0.0),(7.16672,9.064));
( (0.119995,0.0),(8.34207,9.064));
( (0.119995,0.0),(7.16672,8.84801));
( (0.119995,0.0),(7.40456,9.272));
( (0.119995,0.0),(7.16672,8.96001));
( (0.119995,0.0),(7.16672,9.524));
( (0.894836,-1.812),(7.80728,6.96001));
( (0.720001,0.0),(7.84149,9.064));
( (0.720001,0.0),(8.34207,9.064));
( (0.720001,0.0),(7.84149,8.84801));
( (0.720001,0.0),(7.84149,8.96001));
( (1.23599,0.0),(7.39749,9.064));
( (1.23599,0.0),(7.39749,9.064));
( (1.23599,0.0),(7.39749,8.84801));
( (1.23599,0.0),(7.39749,8.96001));
( (0.600006,0.0),(8.03088,6.744));
( (0.167999,-0.156006),(8.46548,9.272));
( (0.842499,-0.216003),(7.791,9.064));
( (0.842499,-0.216003),(7.791,9.064));
( (0.842499,-0.216003),(7.791,8.84801));
( (0.842499,-0.216003),(7.791,9.272));
( (0.842499,-0.216003),(7.791,8.96001));
( (1.30862,-0.216003),(8.34549,9.064));
( (1.30862,-0.216003),(8.34549,9.064));
( (1.30862,-0.216003),(8.34549,8.84801));
( (1.30862,-0.216003),(8.34549,8.96001));
( (1.71875,0.0),(8.34207,9.064));
( (1.032,0.0),(7.25198,6.744));
( (0.943542,-1.884),(6.74672,5.112));
( (1.11032,-0.0119934),(7.18828,5.112));
( (1.5414,0.0119934),(6.74776,5.112));
( (0.295883,-0.216003),(8.34015,6.96001));
( (2.77954,-1.884),(5.41122,5.16));
( (1.69257,-0.587997),(6.97234,7.368));
( (1.56915,-0.251999),(7.3685,7.332));
( (0.280167,-0.684006),(8.47319,7.98));
( (1.62132,0.0),(8.31911,7.12801));
( (-0.232742,-1.716),(7.96288,7.464));
( (1.33704,-0.936005),(6.9994,6.96001));
( (1.18701,0.779999),(7.45158,5.98801));
( (4.13663,3.936),(5.52548,6.744));
( (2.55263,3.936),(6.91748,6.744));
( (1.10208,0.839996),(7.82219,5.35201));
( (2.44608,0.839996),(6.4782,5.35201));
( (2.03796,0.839996),(6.07008,5.35201));
( (0.119995,0.0),(7.34682,7.548));
( (0.119995,0.0),(7.34682,7.548));
( (0.295883,-0.216003),(8.34015,6.96001));
( (1.50705,2.856),(7.00909,3.336));
( (2.70955,-0.936005),(6.44427,6.96001));
( (2.05658,-0.936005),(6.44427,6.96001));
( (3.19691,1.82401),(5.09477,3.312));
( (3.17995,-0.960007),(5.43239,7.548));
( (1.28905,-0.936005),(7.48149,6.744));
( (2.61732,1.644),(5.89627,4.51199));
( (2.2142,-1.608),(4.76706,1.2));
( (1.37421,-1.608),(5.73906,1.2));
( (2.55263,3.936),(6.91748,6.744));
( (0.69397,0.839996),(7.41408,5.35201));
( (0.543304,-0.179993),(6.86584,1.164));
( (0.588928,-0.179993),(7.68463,7.464));
( (1.77861,0.768005),(6.98499,3.89999));
( (1.16064,-1.884),(5.63922,5.16));
( (2.85512,2.98801),(5.77385,7.464));
( (3.52606,5.964),(5.80368,8.064));
( (4.17169,5.964),(7.34207,8.064));
( (2.75314,5.724),(6.97714,7.84801));
( (2.63181,6.036),(7.40456,7.272));
( (2.77911,6.3),(7.20113,6.78));
( (3.17302,6.01199),(6.91736,7.308));
( (4.4969,6.09599),(5.47824,6.96001));
( (3.70491,6.09599),(6.27025,6.96001));
( (2.84311,2.98801),(6.46555,7.464));
( (4.04959,5.79601),(5.97911,7.524));
( (2.65642,-1.812),(4.13786,0.119995));
( (2.63779,2.88),(6.05215,7.464));
( (2.86368,5.964),(8.19406,8.064));
( (2.39241,-1.812),(4.17728,0.0));
( (3.14595,5.90401),(7.36995,8.028));
( (0.607056,2.856),(7.90909,3.336));
( (1.04401,0.0),(6.99263,6.168));
( (0.58017,-0.684006),(8.77319,7.98));
( (0.508179,-0.684006),(8.70119,7.98));
( (0.340179,-0.684006),(8.53319,7.98));
( (0.819473,-0.179993),(6.71071,8.064));
( (0.819473,-0.179993),(7.34207,8.064));
( (0.819473,-0.179993),(6.97714,7.84801));
( (0.819473,-0.179993),(7.40456,7.272));
( (0.819473,-0.179993),(6.71071,6.96001));
( (0.819473,-0.179993),(6.71071,7.524));
( (1.06221,-1.812),(7.28954,5.29201));
( (1.06221,-0.179993),(7.22601,8.064));
( (1.06221,-0.179993),(7.34207,8.064));
( (1.06221,-0.179993),(7.22601,7.84801));
( (1.06221,-0.179993),(7.22601,6.96001));
( (1.224,0.0),(6.06271,8.064));
( (0.119995,0.0),(7.78149,6.744));
( (1.224,0.0),(7.34207,8.064));
( (2.59264,3.108),(6.04706,6.96001));
( (1.224,0.0),(6.97714,7.84801));
( (1.224,0.0),(6.27025,6.96001));
( (0.930206,-0.179993),(7.5511,7.548));
( (0.395996,0.0),(7.40456,7.272));
( (0.647995,0.0),(7.17616,6.744));
( (0.967484,-0.960007),(7.66347,7.548));
( (0.527649,0.0),(7.98549,6.744));
( (2.60463,3.108),(6.50751,6.96001));
( (0.930206,-0.179993),(7.36659,8.064));
( (0.930206,-0.179993),(7.36659,8.064));
( (0.930206,-0.179993),(7.36659,7.84801));
( (0.930206,-0.179993),(7.40456,7.272));
( (0.930206,-0.179993),(7.36659,6.96001));
( (0.434433,-0.179993),(7.56277,5.29201));
( (1.33586,-0.179993),(6.74672,8.064));
( (1.33586,-0.179993),(7.34207,8.064));
( (1.33586,-0.179993),(6.97714,7.84801));
( (1.224,0.0),(6.06271,5.112));
( (1.33586,-0.179993),(6.74672,6.96001));
( (-0.364456,-1.884),(7.71059,8.064));
( (1.224,0.0),(6.71384,7.548));
( (1.04243,-0.960007),(7.2467,6.07201));
( (0.559418,-0.179993),(7.43077,5.29201));
( (0.660004,-0.179993),(7.66069,7.548));
( (-0.20845,-1.884),(7.45345,7.548));
( (-0.364456,-1.884),(7.71059,6.96001));
( (0.0,0.0),(0.0,0.0)) ]};;
(* End Font Description*)
  try remove_font fn; add_font fn
  with Failure ("remove_font : font unknown") -> add_font fn;;