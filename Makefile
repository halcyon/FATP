VERSION=0.3
LANGUAGE=V07
UMASK= umask 002;

all:
	$(UMASK) for dir in `cat DIRECTORIES`; do \
        (echo '*******' $$dir && cd $$dir && make all); done

pictures:
	$(UMASK) for dir in `cat DIRECTORIES`; do \
        (echo '*******' $$dir && cd $$dir && make pictures); done

distrib: 
	-$(UMASK) tar cf -  `cat TAR_FILES` | gzip \
                > bkprog-$(VERSION).tgz

clean:
	$(UMASK) for dir in `cat DIRECTORIES`; do \
        (echo '*******' $$dir && cd $$dir && make clean); done

test:
	$(UMASK) for dir in `cat DIRECTORIES`; do \
	echo "$$dir"; \
	(cd $$dir; echo 'include "load.ml";;' | camllight) \
	done
