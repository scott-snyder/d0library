SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Olimit 2000 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/admin/tmp$(d0library)rar/mmstomake/nodi3000
FLAVOR = SIUNIX
F77 = f77
.IGNORE:
.SUFFIXES:

opt :\
  nodi3000.a
nodi3000.a : $(d0root)/nodi3000/nodi3000.a
	@ echo nodi3000.a is up to date
$(d0root)/nodi3000/nodi3000.a:: $(SCRATCH)/nodi3000/nothing.nl
$(d0root)/nodi3000/nodi3000.a::\
  $(d0root)/nodi3000/nodi3000.a(ctemex.o)\
  $(d0root)/nodi3000/nodi3000.a(d0hpld.o)\
  $(d0root)/nodi3000/nodi3000.a(d0hplt.o)\
  $(d0root)/nodi3000/nodi3000.a(iszgex.o)\
  $(d0root)/nodi3000/nodi3000.a(mcdcex.o)\
  $(d0root)/nodi3000/nodi3000.a(muexec.o)\
  $(d0root)/nodi3000/nodi3000.a(paexec.o)\
  $(d0root)/nodi3000/nodi3000.a(pcexec.o)\
  $(d0root)/nodi3000/nodi3000.a(pdexec.o)\
  $(d0root)/nodi3000/nodi3000.a(pfexec.o)\
  $(d0root)/nodi3000/nodi3000.a(plzexe.o)\
  $(d0root)/nodi3000/nodi3000.a(ptexec.o)\
  $(d0root)/nodi3000/nodi3000.a(pvexec.o)\
  $(d0root)/nodi3000/nodi3000.a(pxhplt.o)\
  $(d0root)/nodi3000/nodi3000.a(pxmain.o)\
  $(d0root)/nodi3000/nodi3000.a(pzexec.o)
	ar $(ARFLAGS) $(d0root)/nodi3000/nodi3000.a `ls $(SCRATCH)/nodi3000/a1/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/nodi3000
$(SCRATCH)/nodi3000/nothing.nl:
	rmdirp $(SCRATCH)/nodi3000
	mkdirp $(SCRATCH)/nodi3000/a1
$(d0root)/nodi3000/nodi3000.a(ctemex.o):\
  $(d0root)/nodi3000/source/ctemex.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/ctemex.f ;\
	mv ctemex.o $(SCRATCH)/nodi3000/a1/ctemex.o ;\
	)
$(d0root)/nodi3000/nodi3000.a(d0hpld.o):\
  $(d0root)/nodi3000/source/d0hpld.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/d0hpld.f ;\
	mv d0hpld.o $(SCRATCH)/nodi3000/a1/d0hpld.o ;\
	)
$(d0root)/nodi3000/nodi3000.a(d0hplt.o):\
  $(d0root)/nodi3000/source/d0hplt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/d0hplt.f ;\
	mv d0hplt.o $(SCRATCH)/nodi3000/a1/d0hplt.o ;\
	)
$(d0root)/nodi3000/nodi3000.a(iszgex.o):\
  $(d0root)/nodi3000/source/iszgex.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/iszgex.f ;\
	mv iszgex.o $(SCRATCH)/nodi3000/a1/iszgex.o ;\
	)
$(d0root)/nodi3000/nodi3000.a(mcdcex.o):\
  $(d0root)/nodi3000/source/mcdcex.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/mcdcex.f ;\
	mv mcdcex.o $(SCRATCH)/nodi3000/a1/mcdcex.o ;\
	)
$(d0root)/nodi3000/nodi3000.a(muexec.o):\
  $(d0root)/nodi3000/source/muexec.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/muexec.f ;\
	mv muexec.o $(SCRATCH)/nodi3000/a1/muexec.o ;\
	)
$(d0root)/nodi3000/nodi3000.a(paexec.o):\
  $(d0root)/nodi3000/source/paexec.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/paexec.f ;\
	mv paexec.o $(SCRATCH)/nodi3000/a1/paexec.o ;\
	)
$(d0root)/nodi3000/nodi3000.a(pcexec.o):\
  $(d0root)/nodi3000/source/pcexec.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/pcexec.f ;\
	mv pcexec.o $(SCRATCH)/nodi3000/a1/pcexec.o ;\
	)
$(d0root)/nodi3000/nodi3000.a(pdexec.o):\
  $(d0root)/nodi3000/source/pdexec.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/pdexec.f ;\
	mv pdexec.o $(SCRATCH)/nodi3000/a1/pdexec.o ;\
	)
$(d0root)/nodi3000/nodi3000.a(pfexec.o):\
  $(d0root)/nodi3000/source/pfexec.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/pfexec.f ;\
	mv pfexec.o $(SCRATCH)/nodi3000/a1/pfexec.o ;\
	)
$(d0root)/nodi3000/nodi3000.a(plzexe.o):\
  $(d0root)/nodi3000/source/plzexe.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/plzexe.f ;\
	mv plzexe.o $(SCRATCH)/nodi3000/a1/plzexe.o ;\
	)
$(d0root)/nodi3000/nodi3000.a(ptexec.o):\
  $(d0root)/nodi3000/source/ptexec.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/ptexec.f ;\
	mv ptexec.o $(SCRATCH)/nodi3000/a1/ptexec.o ;\
	)
$(d0root)/nodi3000/nodi3000.a(pvexec.o):\
  $(d0root)/nodi3000/source/pvexec.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/pvexec.f ;\
	mv pvexec.o $(SCRATCH)/nodi3000/a1/pvexec.o ;\
	)
$(d0root)/nodi3000/nodi3000.a(pxhplt.o):\
  $(d0root)/nodi3000/source/pxhplt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/pxhplt.f ;\
	mv pxhplt.o $(SCRATCH)/nodi3000/a1/pxhplt.o ;\
	)
$(d0root)/nodi3000/nodi3000.a(pxmain.o):\
  $(d0root)/nodi3000/source/pxmain.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/pxmain.f ;\
	mv pxmain.o $(SCRATCH)/nodi3000/a1/pxmain.o ;\
	)
$(d0root)/nodi3000/nodi3000.a(pzexec.o):\
  $(d0root)/nodi3000/source/pzexec.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) nodi3000/source/pzexec.f ;\
	mv pzexec.o $(SCRATCH)/nodi3000/a1/pzexec.o ;\
	)
pre:\
  $(d0root)/nodi3000/source/ctemex.f\
  $(d0root)/nodi3000/source/d0hpld.f\
  $(d0root)/nodi3000/source/d0hplt.f\
  $(d0root)/nodi3000/source/iszgex.f\
  $(d0root)/nodi3000/source/mcdcex.f\
  $(d0root)/nodi3000/source/muexec.f\
  $(d0root)/nodi3000/source/paexec.f\
  $(d0root)/nodi3000/source/pcexec.f\
  $(d0root)/nodi3000/source/pdexec.f\
  $(d0root)/nodi3000/source/pfexec.f\
  $(d0root)/nodi3000/source/plzexe.f\
  $(d0root)/nodi3000/source/ptexec.f\
  $(d0root)/nodi3000/source/pvexec.f\
  $(d0root)/nodi3000/source/pxhplt.f\
  $(d0root)/nodi3000/source/pxmain.f\
  $(d0root)/nodi3000/source/pzexec.f
$(d0root)/nodi3000/source/ctemex.f:\
  $(d0root)/nodi3000/source/ctemex.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/ctemex.for | vmstounix > $(d0root)/nodi3000/source/ctemex.f
$(d0root)/nodi3000/source/d0hpld.f:\
  $(d0root)/nodi3000/source/d0hpld.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/d0hpld.for | vmstounix > $(d0root)/nodi3000/source/d0hpld.f
$(d0root)/nodi3000/source/d0hplt.f:\
  $(d0root)/nodi3000/source/d0hplt.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/d0hplt.for | vmstounix > $(d0root)/nodi3000/source/d0hplt.f
$(d0root)/nodi3000/source/iszgex.f:\
  $(d0root)/nodi3000/source/iszgex.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/iszgex.for | vmstounix > $(d0root)/nodi3000/source/iszgex.f
$(d0root)/nodi3000/source/mcdcex.f:\
  $(d0root)/nodi3000/source/mcdcex.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/mcdcex.for | vmstounix > $(d0root)/nodi3000/source/mcdcex.f
$(d0root)/nodi3000/source/muexec.f:\
  $(d0root)/nodi3000/source/muexec.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/muexec.for | vmstounix > $(d0root)/nodi3000/source/muexec.f
$(d0root)/nodi3000/source/paexec.f:\
  $(d0root)/nodi3000/source/paexec.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/paexec.for | vmstounix > $(d0root)/nodi3000/source/paexec.f
$(d0root)/nodi3000/source/pcexec.f:\
  $(d0root)/nodi3000/source/pcexec.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/pcexec.for | vmstounix > $(d0root)/nodi3000/source/pcexec.f
$(d0root)/nodi3000/source/pdexec.f:\
  $(d0root)/nodi3000/source/pdexec.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/pdexec.for | vmstounix > $(d0root)/nodi3000/source/pdexec.f
$(d0root)/nodi3000/source/pfexec.f:\
  $(d0root)/nodi3000/source/pfexec.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/pfexec.for | vmstounix > $(d0root)/nodi3000/source/pfexec.f
$(d0root)/nodi3000/source/plzexe.f:\
  $(d0root)/nodi3000/source/plzexe.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/plzexe.for | vmstounix > $(d0root)/nodi3000/source/plzexe.f
$(d0root)/nodi3000/source/ptexec.f:\
  $(d0root)/nodi3000/source/ptexec.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/ptexec.for | vmstounix > $(d0root)/nodi3000/source/ptexec.f
$(d0root)/nodi3000/source/pvexec.f:\
  $(d0root)/nodi3000/source/pvexec.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/pvexec.for | vmstounix > $(d0root)/nodi3000/source/pvexec.f
$(d0root)/nodi3000/source/pxhplt.f:\
  $(d0root)/nodi3000/source/pxhplt.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/pxhplt.for | vmstounix > $(d0root)/nodi3000/source/pxhplt.f
$(d0root)/nodi3000/source/pxmain.f:\
  $(d0root)/nodi3000/source/pxmain.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/pxmain.for | vmstounix > $(d0root)/nodi3000/source/pxmain.f
$(d0root)/nodi3000/source/pzexec.f:\
  $(d0root)/nodi3000/source/pzexec.for
	tasteofd0 $(FLAVOR) < $(d0root)/nodi3000/source/pzexec.for | vmstounix > $(d0root)/nodi3000/source/pzexec.f
for:
	touch $(d0root)/nodi3000/source/ctemex.for
	touch $(d0root)/nodi3000/source/d0hpld.for
	touch $(d0root)/nodi3000/source/d0hplt.for
	touch $(d0root)/nodi3000/source/iszgex.for
	touch $(d0root)/nodi3000/source/mcdcex.for
	touch $(d0root)/nodi3000/source/muexec.for
	touch $(d0root)/nodi3000/source/paexec.for
	touch $(d0root)/nodi3000/source/pcexec.for
	touch $(d0root)/nodi3000/source/pdexec.for
	touch $(d0root)/nodi3000/source/pfexec.for
	touch $(d0root)/nodi3000/source/plzexe.for
	touch $(d0root)/nodi3000/source/ptexec.for
	touch $(d0root)/nodi3000/source/pvexec.for
	touch $(d0root)/nodi3000/source/pxhplt.for
	touch $(d0root)/nodi3000/source/pxmain.for
	touch $(d0root)/nodi3000/source/pzexec.for
