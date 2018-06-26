(*************************************************************************)
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
Helvetica_Bold,{font_descr_filename="preloaded"; font_descr_name="Helvetica_Bold";
font_descr_width=12.0;
font_descr_height=12.0;
font_descr_descr= vect_of_list [
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.336;
3.996;
5.688;
6.672;
6.672;
10.668;
8.664;
3.336;
3.996;
3.996;
4.668;
7.008;
3.336;
3.996;
3.336;
3.336;
6.672;
6.672;
6.672;
6.672;
6.672;
6.672;
6.672;
6.672;
6.672;
6.672;
3.996;
3.996;
7.008;
7.008;
7.008;
7.332;
11.7;
8.664;
8.664;
8.664;
8.664;
8.004;
7.332;
9.336;
8.664;
3.336;
6.672;
8.664;
7.332;
9.996;
8.664;
9.336;
8.004;
9.336;
8.664;
8.004;
7.332;
8.664;
8.004;
11.328;
8.004;
8.004;
7.332;
3.996;
3.336;
3.996;
7.008;
6.672;
3.336;
6.672;
7.332;
6.672;
7.332;
6.672;
3.996;
7.332;
7.332;
3.336;
3.336;
6.672;
3.336;
10.668;
7.332;
7.332;
7.332;
7.332;
4.668;
6.672;
3.996;
7.332;
6.672;
9.336;
6.672;
6.672;
6.0;
4.668;
3.36;
4.668;
7.008;
3.336;
3.336;
8.664;
8.664;
8.664;
8.664;
8.664;
8.664;
8.664;
8.004;
8.004;
8.004;
8.004;
3.336;
3.336;
3.336;
3.336;
8.664;
8.664;
9.336;
9.336;
9.336;
9.336;
9.336;
8.664;
8.664;
8.664;
8.664;
8.004;
8.004;
7.332;
7.008;
7.008;
8.84399;
3.996;
6.672;
6.672;
2.004;
6.672;
6.672;
6.672;
6.672;
2.856;
6.0;
6.672;
3.996;
3.996;
7.332;
7.332;
8.84399;
6.672;
6.672;
6.672;
3.336;
3.36;
6.672;
4.2;
3.336;
6.0;
6.0;
6.672;
12.0;
12.0;
7.008;
7.332;
3.996;
3.996;
3.996;
3.996;
3.996;
3.996;
3.996;
3.996;
3.996;
3.996;
3.996;
3.996;
3.996;
3.996;
3.996;
3.996;
12.0;
7.008;
10.008;
10.008;
10.008;
6.672;
6.672;
6.672;
6.672;
6.672;
6.672;
6.672;
6.672;
6.672;
6.672;
6.672;
3.336;
12.0;
3.336;
4.44;
3.336;
3.336;
7.332;
7.332;
7.332;
9.336;
12.0;
4.38;
7.332;
7.332;
7.332;
7.332;
7.332;
10.668;
7.332;
7.332;
7.332;
3.336;
7.332;
6.672;
3.336;
7.332;
11.328;
7.332;
7.332;
6.672;
3.336 ]; font_descr_descr_bbox= vect_of_list [
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
( (1.08,0.0),(2.92799,8.616));
( (1.17599,5.364),(4.51199,8.616));
( (0.216003,0.0),(6.45599,8.37601));
( (0.360001,-1.38),(6.276,9.3));
( (0.335999,-0.227997),(10.332,8.52));
( (0.647995,-0.227997),(8.412,8.616));
( (0.828003,5.34),(2.508,8.616));
( (0.419998,-2.496),(3.76801,8.808));
( (0.227997,-2.496),(3.576,8.808));
( (0.324005,4.644),(4.34399,8.616));
( (0.479996,0.0),(6.528,6.07201));
( (0.768005,-2.01601),(2.56799,1.752));
( (0.324005,2.58),(3.672,4.14));
( (0.768005,0.0),(2.56799,1.752));
( (-0.395996,-0.227997),(3.73199,8.84399));
( (0.384003,-0.227997),(6.28799,8.52));
( (0.828003,0.0),(4.536,8.52));
( (0.311996,0.0),(6.132,8.52));
( (0.324005,-0.227997),(6.192,8.52));
( (0.324005,0.0),(6.312,8.52));
( (0.324005,-0.227997),(6.192,8.37601));
( (0.371994,-0.227997),(6.24001,8.52));
( (0.300003,0.0),(6.336,8.37601));
( (0.384003,-0.227997),(6.28799,8.52));
( (0.360001,-0.227997),(6.26401,8.52));
( (1.104,0.0),(2.90401,6.144));
( (1.104,-2.01601),(2.90401,6.144));
( (0.455994,-0.095993),(6.552,6.168));
( (0.479996,1.04401),(6.528,5.028));
( (0.455994,-0.095993),(6.552,6.168));
( (0.720001,0.0),(6.672,8.724));
( (1.416,-0.227997),(10.272,8.84399));
( (0.240005,0.0),(8.424,8.616));
( (0.912003,0.0),(8.028,8.616));
( (0.528,-0.227997),(8.20799,8.84399));
( (0.912003,0.0),(8.22,8.616));
( (0.912003,0.0),(7.452,8.616));
( (0.912003,0.0),(7.04401,8.616));
( (0.528,-0.227997),(8.556,8.856));
( (0.852005,0.0),(7.812,8.616));
( (0.768005,0.0),(2.56799,8.616));
( (0.264008,-0.216003),(5.808,8.616));
( (1.04401,0.0),(8.664,8.616));
( (0.912003,0.0),(6.996,8.616));
( (0.828003,0.0),(9.17999,8.616));
( (0.828003,0.0),(7.84801,8.616));
( (0.528,-0.227997),(8.808,8.84399));
( (0.912003,0.0),(7.524,8.616));
( (0.528,-0.623993),(8.84399,8.84399));
( (0.912003,0.0),(8.12399,8.616));
( (0.468002,-0.227997),(7.548,8.84399));
( (0.167999,0.0),(7.17599,8.616));
( (0.863998,-0.227997),(7.812,8.616));
( (0.227997,0.0),(7.776,8.616));
( (0.192001,0.0),(11.148,8.616));
( (0.167999,0.0),(7.836,8.616));
( (0.179993,0.0),(7.836,8.616));
( (0.300003,0.0),(7.032,8.616));
( (0.755997,-2.35201),(3.70799,8.664));
( (-0.395996,-0.227997),(3.73199,8.84399));
( (0.287994,-2.35201),(3.24001,8.664));
( (0.744003,3.87601),(6.26401,8.37601));
( (0.0,-1.5),(6.672,-0.899994));
( (0.828003,5.448),(2.508,8.724));
( (0.348007,-0.167999),(6.32401,6.552));
( (0.731995,-0.167999),(6.936,8.616));
( (0.408005,-0.167999),(6.28799,6.552));
( (0.408005,-0.167999),(6.612,8.616));
( (0.276001,-0.167999),(6.336,6.552));
( (0.119995,0.0),(3.81599,8.724));
( (0.479996,-2.604),(6.636,6.552));
( (0.779999,0.0),(6.552,8.616));
( (0.828003,0.0),(2.508,8.7));
( (0.0359955,-2.56799),(2.508,8.7));
( (0.828003,0.0),(6.744,8.616));
( (0.828003,0.0),(2.508,8.616));
( (0.768005,0.0),(9.912,6.552));
( (0.779999,0.0),(6.552,6.552));
( (0.408005,-0.167999),(6.936,6.552));
( (0.744003,-2.48399),(6.936,6.552));
( (0.408005,-2.48399),(6.62399,6.552));
( (0.768005,0.0),(4.476,6.552));
( (0.360001,-0.167999),(6.228,6.552));
( (0.119995,-0.0720062),(3.70799,8.112));
( (0.792007,-0.167999),(6.53999,6.384));
( (0.156006,0.0),(6.51601,6.384));
( (0.119995,0.0),(9.228,6.384));
( (0.179993,0.0),(6.492,6.384));
( (0.119995,-2.56799),(6.468,6.384));
( (0.240005,0.0),(5.75999,6.384));
( (0.576004,-2.35201),(4.38,8.664));
( (1.008,-0.227997),(2.35201,8.84399));
( (0.287994,-2.35201),(4.092,8.664));
( (0.731995,1.95599),(6.276,4.116));
( (0.0,0.0),(0.0,0.0));
( (0.0,0.0),(0.0,0.0));
( (0.240005,0.0),(8.424,11.0));
( (0.240005,0.0),(8.424,11.0));
( (0.240005,0.0),(8.424,11.0));
( (0.240005,0.0),(8.424,10.844));
( (0.240005,0.0),(8.424,10.748));
( (0.240005,0.0),(8.424,11.312));
( (0.528,-2.73599),(8.20799,8.84399));
( (0.912003,0.0),(7.452,11.0));
( (0.912003,0.0),(7.452,11.0));
( (0.912003,0.0),(7.452,11.0));
( (0.912003,0.0),(7.452,10.748));
( (-0.276001,0.0),(2.7,11.0));
( (0.768005,0.0),(4.272,11.0));
( (-0.119995,0.0),(4.116,11.0));
( (0.0720062,0.0),(3.924,10.748));
( (-0.0599976,0.0),(8.22,8.616));
( (0.828003,0.0),(7.84801,10.844));
( (0.528,-0.227997),(8.808,11.0));
( (0.528,-0.227997),(8.808,11.0));
( (0.528,-0.227997),(8.808,11.0));
( (0.528,-0.227997),(8.808,10.844));
( (0.528,-0.227997),(8.808,10.748));
( (0.863998,-0.227997),(7.812,11.0));
( (0.863998,-0.227997),(7.812,11.0));
( (0.863998,-0.227997),(7.812,11.0));
( (0.863998,-0.227997),(7.812,10.748));
( (0.179993,0.0),(7.836,11.0));
( (0.912003,0.0),(7.524,8.616));
( (0.792007,-2.48399),(6.53999,6.384));
( (0.479996,0.0119934),(6.53999,6.06));
( (0.479996,-0.503998),(6.528,6.576));
( (-0.132004,-0.227997),(8.98801,8.84399));
( (1.08,-2.23199),(2.92799,6.384));
( (0.408005,-1.416),(6.28799,7.536));
( (0.335999,-0.192001),(6.492,8.616));
( (-2.03999,-0.227997),(4.032,8.52));
( (-0.108002,0.0),(6.78,8.37601));
( (-0.119995,-2.52),(6.192,8.84399));
( (0.408005,-2.20799),(6.26401,8.724));
( (-0.0359955,0.912003),(6.70799,7.632));
( (0.839996,5.364),(2.01601,8.616));
( (0.768005,5.448),(5.23199,8.724));
( (1.056,0.912003),(5.616,5.808));
( (0.996002,0.912003),(3.0,5.808));
( (0.996002,0.912003),(3.0,5.808));
( (0.119995,0.0),(6.504,8.724));
( (0.119995,0.0),(6.504,8.724));
( (-0.132004,-0.227997),(8.976,8.84399));
( (0.0,2.724),(6.672,3.996));
( (0.432007,-2.052),(6.24001,8.616));
( (0.432007,-2.052),(6.24001,8.616));
( (0.695999,2.064),(2.64,4.008));
( (1.008,-0.227997),(2.35201,8.84399));
( (-0.095993,-2.29201),(6.468,8.39999));
( (0.119995,2.328),(4.08,6.28799));
( (0.828003,-1.752),(2.508,1.524));
( (0.768005,-1.752),(5.23199,1.524));
( (0.768005,5.34),(5.23199,8.616));
( (1.056,0.912003),(5.616,5.808));
( (1.104,0.0),(10.896,1.752));
( (-0.0359955,-0.227997),(12.036,8.52));
( (0.479996,1.29601),(6.528,5.028));
( (0.660004,-2.34),(6.612,6.384));
( (0.311996,3.396),(2.84399,8.52));
( (-0.276001,7.248),(2.7,9.0));
( (1.29601,7.248),(4.272,9.0));
( (-0.119995,7.248),(4.116,9.0));
( (-0.203995,7.32001),(4.2,8.84399));
( (-0.0720062,7.248),(4.06799,8.136));
( (-0.0240021,7.248),(4.02,9.0));
( (1.248,7.368),(2.75999,8.748));
( (0.0720062,7.368),(3.924,8.748));
( (0.108002,3.396),(3.888,8.52));
( (0.707993,6.81599),(3.3,9.312));
( (0.0720062,-2.73599),(2.94,0.0));
( (0.095993,3.252),(3.912,8.52));
( (0.108002,7.248),(5.832,9.0));
( (0.852005,-2.73599),(3.64799,0.0));
( (-0.119995,7.248),(4.116,9.0));
( (0.0,2.724),(12.0,3.996));
( (0.479996,0.0),(6.528,6.07201));
( (0.311996,-0.227997),(9.192,8.52));
( (0.311996,-0.227997),(9.528,8.52));
( (0.192001,-0.227997),(9.588,8.52));
( (0.348007,-0.167999),(6.32401,9.0));
( (0.348007,-0.167999),(6.32401,9.0));
( (0.348007,-0.167999),(6.32401,9.0));
( (0.348007,-0.167999),(6.32401,8.84399));
( (0.348007,-0.167999),(6.32401,8.748));
( (0.348007,-0.167999),(6.32401,9.312));
( (0.408005,-2.73599),(6.28799,6.552));
( (0.276001,-0.167999),(6.336,9.0));
( (0.276001,-0.167999),(6.336,9.0));
( (0.276001,-0.167999),(6.336,9.0));
( (0.276001,-0.167999),(6.336,8.748));
( (-0.276001,0.0),(2.7,9.0));
( (0.0599976,0.0),(11.448,8.616));
( (0.828003,0.0),(4.272,9.0));
( (0.264008,3.312),(4.164,8.84399));
( (-0.119995,0.0),(4.116,9.0));
( (0.0720062,0.0),(3.924,8.748));
( (0.408005,-0.167999),(6.936,8.84399));
( (0.779999,0.0),(6.552,8.84399));
( (-0.240005,0.0),(6.996,8.616));
( (0.395996,-0.324005),(8.92799,8.94));
( (0.444,-0.227997),(11.532,8.84399));
( (0.0720062,3.312),(4.32001,8.84399));
( (0.408005,-0.167999),(6.936,9.0));
( (0.408005,-0.167999),(6.936,9.0));
( (0.408005,-0.167999),(6.936,9.0));
( (0.408005,-0.167999),(6.936,8.84399));
( (0.408005,-0.167999),(6.936,8.748));
( (0.348007,-0.192001),(10.296,6.552));
( (0.792007,-0.167999),(6.53999,9.0));
( (0.792007,-0.167999),(6.53999,9.0));
( (0.792007,-0.167999),(6.53999,9.0));
( (0.828003,0.0),(2.508,6.384));
( (0.792007,-0.167999),(6.53999,8.748));
( (0.119995,-2.56799),(6.468,9.0));
( (-0.216003,0.0),(3.552,8.616));
( (0.264008,-0.348007),(7.06799,6.72));
( (0.408005,-0.167999),(10.944,6.552));
( (0.828003,-0.167999),(6.948,8.772));
( (0.744003,-2.496),(6.936,8.616));
( (0.119995,-2.56799),(6.468,8.748));
( (0.0,0.0),(0.0,0.0)) ]};;
(* End Font Description*)
  try remove_font fn; add_font fn
  with Failure ("remove_font : font unknown") -> add_font fn;;