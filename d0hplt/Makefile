SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Olimit 2000 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/admin/tmp$(d0library)rar/mmstomake/d0hplt
FLAVOR = SIUNIX
F77 = f77
.IGNORE:
.SUFFIXES:
debug :\
  deb_d0hplt.a
deb_d0hplt.a : $(d0root)/d0hplt/deb_d0hplt.a
	@ echo deb_d0hplt.a is up to date
$(d0root)/d0hplt/deb_d0hplt.a:: $(SCRATCH)/d0hplt/nothing.nl
$(d0root)/d0hplt/deb_d0hplt.a::\
  $(d0root)/d0hplt/deb_d0hplt.a(d0h_bsion662.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0h_file.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hclr.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hdrv.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hform.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hini.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hint.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hlab.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hldir.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hlego.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hlnm.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hopen.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hpid.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hplt_final.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hplt_setup.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hpost.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hprt.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hshw.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0hstr.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(d0husd.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(di3_start.o)\
  $(d0root)/d0hplt/deb_d0hplt.a(hplot_save.o)
	ar $(ARFLAGS) $(d0root)/d0hplt/deb_d0hplt.a `ls $(SCRATCH)/d0hplt/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/d0hplt/deb_d0hplt.a `ls $(SCRATCH)/d0hplt/a2/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/d0hplt
$(SCRATCH)/d0hplt/nothing.nl:
	rmdirp $(SCRATCH)/d0hplt
	mkdirp $(SCRATCH)/d0hplt/a1
	mkdirp $(SCRATCH)/d0hplt/a2
$(d0root)/d0hplt/deb_d0hplt.a(d0h_bsion662.o):\
  $(d0root)/d0hplt/source/d0h_bump_version.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0h_bump_version.f ;\
	mv d0h_bump_version.o $(SCRATCH)/d0hplt/a1/d0h_bsion662.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0h_file.o):\
  $(d0root)/d0hplt/source/d0h_file.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0h_file.f ;\
	mv d0h_file.o $(SCRATCH)/d0hplt/a1/d0h_file.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hclr.o):\
  $(d0root)/d0hplt/source/d0hclr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hclr.f ;\
	mv d0hclr.o $(SCRATCH)/d0hplt/a1/d0hclr.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hdrv.o):\
  $(d0root)/d0hplt/source/d0hdrv.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hdrv.f ;\
	mv d0hdrv.o $(SCRATCH)/d0hplt/a1/d0hdrv.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hform.o):\
  $(d0root)/d0hplt/source/d0hform.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hform.f ;\
	mv d0hform.o $(SCRATCH)/d0hplt/a1/d0hform.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hini.o):\
  $(d0root)/d0hplt/source/d0hini.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hini.f ;\
	mv d0hini.o $(SCRATCH)/d0hplt/a1/d0hini.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hint.o):\
  $(d0root)/d0hplt/source/d0hint.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hint.f ;\
	mv d0hint.o $(SCRATCH)/d0hplt/a1/d0hint.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hlab.o):\
  $(d0root)/d0hplt/source/d0hlab.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hlab.f ;\
	mv d0hlab.o $(SCRATCH)/d0hplt/a1/d0hlab.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hldir.o):\
  $(d0root)/d0hplt/source/d0hldir.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hldir.f ;\
	mv d0hldir.o $(SCRATCH)/d0hplt/a1/d0hldir.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hlego.o):\
  $(d0root)/d0hplt/source/d0hlego.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hlego.f ;\
	mv d0hlego.o $(SCRATCH)/d0hplt/a1/d0hlego.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hlnm.o):\
  $(d0root)/d0hplt/source/d0hlnm.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hlnm.f ;\
	mv d0hlnm.o $(SCRATCH)/d0hplt/a1/d0hlnm.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hopen.o):\
  $(d0root)/d0hplt/source/d0hopen.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hopen.f ;\
	mv d0hopen.o $(SCRATCH)/d0hplt/a1/d0hopen.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hpid.o):\
  $(d0root)/d0hplt/source/d0hpid.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hpid.f ;\
	mv d0hpid.o $(SCRATCH)/d0hplt/a1/d0hpid.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hplt_final.o):\
  $(d0root)/d0hplt/source/d0hplt_final.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hplt_final.f ;\
	mv d0hplt_final.o $(SCRATCH)/d0hplt/a1/d0hplt_final.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hplt_setup.o):\
  $(d0root)/d0hplt/source/d0hplt_setup.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hplt_setup.f ;\
	mv d0hplt_setup.o $(SCRATCH)/d0hplt/a1/d0hplt_setup.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hpost.o):\
  $(d0root)/d0hplt/source/d0hpost.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hpost.f ;\
	mv d0hpost.o $(SCRATCH)/d0hplt/a1/d0hpost.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hprt.o):\
  $(d0root)/d0hplt/source/d0hprt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hprt.f ;\
	mv d0hprt.o $(SCRATCH)/d0hplt/a1/d0hprt.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hshw.o):\
  $(d0root)/d0hplt/source/d0hshw.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hshw.f ;\
	mv d0hshw.o $(SCRATCH)/d0hplt/a1/d0hshw.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0hstr.o):\
  $(d0root)/d0hplt/source/d0hstr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0hstr.f ;\
	mv d0hstr.o $(SCRATCH)/d0hplt/a1/d0hstr.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(d0husd.o):\
  $(d0root)/d0hplt/source/d0husd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/d0husd.f ;\
	mv d0husd.o $(SCRATCH)/d0hplt/a1/d0husd.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(di3_start.o):\
  $(d0root)/d0hplt/source/di3_start.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/di3_start.f ;\
	mv di3_start.o $(SCRATCH)/d0hplt/a2/di3_start.o ;\
	)
$(d0root)/d0hplt/deb_d0hplt.a(hplot_save.o):\
  $(d0root)/d0hplt/source/hplot_save.f\
  $(d0library)/inc/exm_statistics.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) d0hplt/source/hplot_save.f ;\
	mv hplot_save.o $(SCRATCH)/d0hplt/a2/hplot_save.o ;\
	)
pre:\
  $(d0root)/d0hplt/source/d0h_bump_version.f\
  $(d0root)/d0hplt/source/d0h_file.f\
  $(d0root)/d0hplt/source/d0hclr.f\
  $(d0root)/d0hplt/source/d0hdrv.f\
  $(d0root)/d0hplt/source/d0hform.f\
  $(d0root)/d0hplt/source/d0hini.f\
  $(d0root)/d0hplt/source/d0hint.f\
  $(d0root)/d0hplt/source/d0hlab.f\
  $(d0root)/d0hplt/source/d0hldir.f\
  $(d0root)/d0hplt/source/d0hlego.f\
  $(d0root)/d0hplt/source/d0hlnm.f\
  $(d0root)/d0hplt/source/d0hopen.f\
  $(d0root)/d0hplt/source/d0hpid.f\
  $(d0root)/d0hplt/source/d0hplt_final.f\
  $(d0root)/d0hplt/source/d0hplt_setup.f\
  $(d0root)/d0hplt/source/d0hpost.f\
  $(d0root)/d0hplt/source/d0hprt.f\
  $(d0root)/d0hplt/source/d0hshw.f\
  $(d0root)/d0hplt/source/d0hstr.f\
  $(d0root)/d0hplt/source/d0husd.f\
  $(d0root)/d0hplt/source/di3_start.f\
  $(d0root)/d0hplt/source/hplot_save.f
$(d0root)/d0hplt/source/d0h_bump_version.f:\
  $(d0root)/d0hplt/source/d0h_bump_version.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0h_bump_version.for | vmstounix > $(d0root)/d0hplt/source/d0h_bump_version.f
$(d0root)/d0hplt/source/d0h_file.f:\
  $(d0root)/d0hplt/source/d0h_file.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0h_file.for | vmstounix > $(d0root)/d0hplt/source/d0h_file.f
$(d0root)/d0hplt/source/d0hclr.f:\
  $(d0root)/d0hplt/source/d0hclr.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hclr.for | vmstounix > $(d0root)/d0hplt/source/d0hclr.f
$(d0root)/d0hplt/source/d0hdrv.f:\
  $(d0root)/d0hplt/source/d0hdrv.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hdrv.for | vmstounix > $(d0root)/d0hplt/source/d0hdrv.f
$(d0root)/d0hplt/source/d0hform.f:\
  $(d0root)/d0hplt/source/d0hform.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hform.for | vmstounix > $(d0root)/d0hplt/source/d0hform.f
$(d0root)/d0hplt/source/d0hini.f:\
  $(d0root)/d0hplt/source/d0hini.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hini.for | vmstounix > $(d0root)/d0hplt/source/d0hini.f
$(d0root)/d0hplt/source/d0hint.f:\
  $(d0root)/d0hplt/source/d0hint.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hint.for | vmstounix > $(d0root)/d0hplt/source/d0hint.f
$(d0root)/d0hplt/source/d0hlab.f:\
  $(d0root)/d0hplt/source/d0hlab.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hlab.for | vmstounix > $(d0root)/d0hplt/source/d0hlab.f
$(d0root)/d0hplt/source/d0hldir.f:\
  $(d0root)/d0hplt/source/d0hldir.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hldir.for | vmstounix > $(d0root)/d0hplt/source/d0hldir.f
$(d0root)/d0hplt/source/d0hlego.f:\
  $(d0root)/d0hplt/source/d0hlego.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hlego.for | vmstounix > $(d0root)/d0hplt/source/d0hlego.f
$(d0root)/d0hplt/source/d0hlnm.f:\
  $(d0root)/d0hplt/source/d0hlnm.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hlnm.for | vmstounix > $(d0root)/d0hplt/source/d0hlnm.f
$(d0root)/d0hplt/source/d0hopen.f:\
  $(d0root)/d0hplt/source/d0hopen.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hopen.for | vmstounix > $(d0root)/d0hplt/source/d0hopen.f
$(d0root)/d0hplt/source/d0hpid.f:\
  $(d0root)/d0hplt/source/d0hpid.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hpid.for | vmstounix > $(d0root)/d0hplt/source/d0hpid.f
$(d0root)/d0hplt/source/d0hplt_final.f:\
  $(d0root)/d0hplt/source/d0hplt_final.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hplt_final.for | vmstounix > $(d0root)/d0hplt/source/d0hplt_final.f
$(d0root)/d0hplt/source/d0hplt_setup.f:\
  $(d0root)/d0hplt/source/d0hplt_setup.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hplt_setup.for | vmstounix > $(d0root)/d0hplt/source/d0hplt_setup.f
$(d0root)/d0hplt/source/d0hpost.f:\
  $(d0root)/d0hplt/source/d0hpost.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hpost.for | vmstounix > $(d0root)/d0hplt/source/d0hpost.f
$(d0root)/d0hplt/source/d0hprt.f:\
  $(d0root)/d0hplt/source/d0hprt.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hprt.for | vmstounix > $(d0root)/d0hplt/source/d0hprt.f
$(d0root)/d0hplt/source/d0hshw.f:\
  $(d0root)/d0hplt/source/d0hshw.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hshw.for | vmstounix > $(d0root)/d0hplt/source/d0hshw.f
$(d0root)/d0hplt/source/d0hstr.f:\
  $(d0root)/d0hplt/source/d0hstr.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0hstr.for | vmstounix > $(d0root)/d0hplt/source/d0hstr.f
$(d0root)/d0hplt/source/d0husd.f:\
  $(d0root)/d0hplt/source/d0husd.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/d0husd.for | vmstounix > $(d0root)/d0hplt/source/d0husd.f
$(d0root)/d0hplt/source/di3_start.f:\
  $(d0root)/d0hplt/source/di3_start.for
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/di3_start.for | vmstounix > $(d0root)/d0hplt/source/di3_start.f
$(d0root)/d0hplt/source/hplot_save.f:\
  $(d0root)/d0hplt/source/hplot_save.for\
  $(d0library)/inc/exm_statistics.inc
	tasteofd0 $(FLAVOR) < $(d0root)/d0hplt/source/hplot_save.for | vmstounix > $(d0root)/d0hplt/source/hplot_save.f
for:
	touch $(d0root)/d0hplt/source/d0h_bump_version.for
	touch $(d0root)/d0hplt/source/d0h_file.for
	touch $(d0root)/d0hplt/source/d0hclr.for
	touch $(d0root)/d0hplt/source/d0hdrv.for
	touch $(d0root)/d0hplt/source/d0hform.for
	touch $(d0root)/d0hplt/source/d0hini.for
	touch $(d0root)/d0hplt/source/d0hint.for
	touch $(d0root)/d0hplt/source/d0hlab.for
	touch $(d0root)/d0hplt/source/d0hldir.for
	touch $(d0root)/d0hplt/source/d0hlego.for
	touch $(d0root)/d0hplt/source/d0hlnm.for
	touch $(d0root)/d0hplt/source/d0hopen.for
	touch $(d0root)/d0hplt/source/d0hpid.for
	touch $(d0root)/d0hplt/source/d0hplt_final.for
	touch $(d0root)/d0hplt/source/d0hplt_setup.for
	touch $(d0root)/d0hplt/source/d0hpost.for
	touch $(d0root)/d0hplt/source/d0hprt.for
	touch $(d0root)/d0hplt/source/d0hshw.for
	touch $(d0root)/d0hplt/source/d0hstr.for
	touch $(d0root)/d0hplt/source/d0husd.for
	touch $(d0root)/d0hplt/source/di3_start.for
	touch $(d0root)/d0hplt/source/hplot_save.for
