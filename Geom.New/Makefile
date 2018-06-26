INCLUDE=-I ../Util -I ../Arbres
MLGRAPH=/usr/local/lib/caml-light/MLgraph
UMASK=umask 002;

all: permutations.zo complex.zo hyperbolic_geometry.zo symmetry_groups.zo \
     hyperbolic_group_3_3_4.zo circle_limit.zo poisson.zi poisson.zo \
     kang1.zi kang1.zo \
     kang2.zi kang2.zo kang3.zi kang3.zo


pictures:

.ml.zo:
	$(UMASK) camlc $(INCLUDE) -c $<

.mli.zi:
	$(UMASK) camlc $(INCLUDE) -c $<


hyperbolic_geometry.zo: complex.zo

symmetry_groups.zo: permutations.zo

hyperbolic_group_3_3_4.zo: hyperbolic_geometry.zo symmetry_groups.zo

circle_limit.zo: complex.zo hyperbolic_geometry.zo symmetry_groups.zo \
        hyperbolic_group_3_3_4.zo

poisson.zo: complex.zo hyperbolic_geometry.zo circle_limit.zo

kang1.zo: complex.zo hyperbolic_geometry.zo circle_limit.zo

kang2.zo: complex.zo hyperbolic_geometry.zo circle_limit.zo

kang3.zo: complex.zo hyperbolic_geometry.zo circle_limit.zo


clean:
	/bin/rm -f *.zi *.zo mkpictures

.SUFFIXES:
.SUFFIXES: .ml .mli .zi .zo

