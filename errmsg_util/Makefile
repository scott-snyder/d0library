SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Olimit 2500 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/admin/tmp/d0librar/mmstomake/errmsg_util
FLAVOR = SIUNIX
F77 = f77
.IGNORE:
.SUFFIXES:
debug :\
  deb_errmsg_util.a
deb_errmsg_util.a : $(d0root)/errmsg_util/deb_errmsg_util.a
	@ echo deb_errmsg_util.a is up to date
$(d0root)/errmsg_util/deb_errmsg_util.a:: $(SCRATCH)/errmsg_util/nothing.nl
$(d0root)/errmsg_util/deb_errmsg_util.a::\
  $(d0root)/errmsg_util/deb_errmsg_util.a(bkermg.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(elogwr.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(ermgfl.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errclr.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errdsp.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errfat.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errfnd.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errget.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errhan_e.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errini.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errins.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errint.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errlog.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errmax.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errmax_rcp.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errmsg.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errmzb.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errscr.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errsum.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(errwrn.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(ewrnwr.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(gzermg.o)\
  $(d0root)/errmsg_util/deb_errmsg_util.a(prermg.o)
	ar $(ARFLAGS) $(d0root)/errmsg_util/deb_errmsg_util.a `ls $(SCRATCH)/errmsg_util/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/errmsg_util/deb_errmsg_util.a `ls $(SCRATCH)/errmsg_util/a2/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/errmsg_util
$(SCRATCH)/errmsg_util/nothing.nl:
	rmdirp $(SCRATCH)/errmsg_util
	mkdirp $(SCRATCH)/errmsg_util/a1
	mkdirp $(SCRATCH)/errmsg_util/a2
$(d0root)/errmsg_util/deb_errmsg_util.a(bkermg.o):\
  $(d0root)/errmsg_util/source/bkermg.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izermg.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/bkermg.f ;\
	mv bkermg.o $(SCRATCH)/errmsg_util/a1/bkermg.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(elogwr.o):\
  $(d0root)/errmsg_util/source/elogwr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/elogwr.f ;\
	mv elogwr.o $(SCRATCH)/errmsg_util/a1/elogwr.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(ermgfl.o):\
  $(d0root)/errmsg_util/source/ermgfl.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/errzst.inc\
  $(d0library)/inc/errzst_c.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/ermgfl.f ;\
	mv ermgfl.o $(SCRATCH)/errmsg_util/a1/ermgfl.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errclr.o):\
  $(d0root)/errmsg_util/source/errclr.f\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errclr.f ;\
	mv errclr.o $(SCRATCH)/errmsg_util/a1/errclr.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errdsp.o):\
  $(d0root)/errmsg_util/source/errdsp.f\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errdsp.f ;\
	mv errdsp.o $(SCRATCH)/errmsg_util/a1/errdsp.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errfat.o):\
  $(d0root)/errmsg_util/source/errfat.f\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errfat.f ;\
	mv errfat.o $(SCRATCH)/errmsg_util/a1/errfat.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errfnd.o):\
  $(d0root)/errmsg_util/source/errfnd.f\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errfnd.f ;\
	mv errfnd.o $(SCRATCH)/errmsg_util/a1/errfnd.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errget.o):\
  $(d0root)/errmsg_util/source/errget.f\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errcnt.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errget.f ;\
	mv errget.o $(SCRATCH)/errmsg_util/a1/errget.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errhan_e.o):\
  $(d0root)/errmsg_util/source/errhan_e.f\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errhan_e.f ;\
	mv errhan_e.o $(SCRATCH)/errmsg_util/a1/errhan_e.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errini.o):\
  $(d0root)/errmsg_util/source/errini.f\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errini.f ;\
	mv errini.o $(SCRATCH)/errmsg_util/a1/errini.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errins.o):\
  $(d0root)/errmsg_util/source/errins.f\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errins.f ;\
	mv errins.o $(SCRATCH)/errmsg_util/a1/errins.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errint.o):\
  $(d0root)/errmsg_util/source/errint.f\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errint.f ;\
	mv errint.o $(SCRATCH)/errmsg_util/a1/errint.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errlog.o):\
  $(d0root)/errmsg_util/source/errlog.f\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errlog.f ;\
	mv errlog.o $(SCRATCH)/errmsg_util/a1/errlog.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errmax.o):\
  $(d0root)/errmsg_util/source/errmax.f\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errmax.f ;\
	mv errmax.o $(SCRATCH)/errmsg_util/a1/errmax.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errmax_rcp.o):\
  $(d0root)/errmsg_util/source/errmax_rcp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errmax_rcp.f ;\
	mv errmax_rcp.o $(SCRATCH)/errmsg_util/a1/errmax_rcp.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errmsg.o):\
  $(d0root)/errmsg_util/source/errmsg.f\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errmsg.f ;\
	mv errmsg.o $(SCRATCH)/errmsg_util/a1/errmsg.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errmzb.o):\
  $(d0root)/errmsg_util/source/errmzb.f\
  $(d0library)/inc/zebcom.inc\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errzst.inc\
  $(d0library)/inc/errzst_c.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errmzb.f ;\
	mv errmzb.o $(SCRATCH)/errmsg_util/a1/errmzb.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errscr.o):\
  $(d0root)/errmsg_util/source/errscr.f\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errscr.f ;\
	mv errscr.o $(SCRATCH)/errmsg_util/a1/errscr.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errsum.o):\
  $(d0root)/errmsg_util/source/errsum.f\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errsum.f ;\
	mv errsum.o $(SCRATCH)/errmsg_util/a1/errsum.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(errwrn.o):\
  $(d0root)/errmsg_util/source/errwrn.f\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/errwrn.f ;\
	mv errwrn.o $(SCRATCH)/errmsg_util/a1/errwrn.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(ewrnwr.o):\
  $(d0root)/errmsg_util/source/ewrnwr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/ewrnwr.f ;\
	mv ewrnwr.o $(SCRATCH)/errmsg_util/a2/ewrnwr.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(gzermg.o):\
  $(d0root)/errmsg_util/source/gzermg.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izermg.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/gzermg.f ;\
	mv gzermg.o $(SCRATCH)/errmsg_util/a2/gzermg.o ;\
	)
$(d0root)/errmsg_util/deb_errmsg_util.a(prermg.o):\
  $(d0root)/errmsg_util/source/prermg.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izermg.link\
  $(d0library)/inc/errzst.inc\
  $(d0library)/inc/errzst_c.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) errmsg_util/source/prermg.f ;\
	mv prermg.o $(SCRATCH)/errmsg_util/a2/prermg.o ;\
	)
pre:\
  $(d0root)/errmsg_util/source/bkermg.f\
  $(d0root)/errmsg_util/source/elogwr.f\
  $(d0root)/errmsg_util/source/ermgfl.f\
  $(d0root)/errmsg_util/source/errclr.f\
  $(d0root)/errmsg_util/source/errdsp.f\
  $(d0root)/errmsg_util/source/errfat.f\
  $(d0root)/errmsg_util/source/errfnd.f\
  $(d0root)/errmsg_util/source/errget.f\
  $(d0root)/errmsg_util/source/errhan_e.f\
  $(d0root)/errmsg_util/source/errini.f\
  $(d0root)/errmsg_util/source/errins.f\
  $(d0root)/errmsg_util/source/errint.f\
  $(d0root)/errmsg_util/source/errlog.f\
  $(d0root)/errmsg_util/source/errmax.f\
  $(d0root)/errmsg_util/source/errmax_rcp.f\
  $(d0root)/errmsg_util/source/errmsg.f\
  $(d0root)/errmsg_util/source/errmzb.f\
  $(d0root)/errmsg_util/source/errscr.f\
  $(d0root)/errmsg_util/source/errsum.f\
  $(d0root)/errmsg_util/source/errwrn.f\
  $(d0root)/errmsg_util/source/ewrnwr.f\
  $(d0root)/errmsg_util/source/gzermg.f\
  $(d0root)/errmsg_util/source/prermg.f
$(d0root)/errmsg_util/source/bkermg.f:\
  $(d0root)/errmsg_util/source/bkermg.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izermg.link
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/bkermg.for | vmstounix > $(d0root)/errmsg_util/source/bkermg.f
$(d0root)/errmsg_util/source/elogwr.f:\
  $(d0root)/errmsg_util/source/elogwr.for
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/elogwr.for | vmstounix > $(d0root)/errmsg_util/source/elogwr.f
$(d0root)/errmsg_util/source/ermgfl.f:\
  $(d0root)/errmsg_util/source/ermgfl.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/errzst.inc\
  $(d0library)/inc/errzst_c.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/ermgfl.for | vmstounix > $(d0root)/errmsg_util/source/ermgfl.f
$(d0root)/errmsg_util/source/errclr.f:\
  $(d0root)/errmsg_util/source/errclr.for\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errclr.for | vmstounix > $(d0root)/errmsg_util/source/errclr.f
$(d0root)/errmsg_util/source/errdsp.f:\
  $(d0root)/errmsg_util/source/errdsp.for\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errdsp.for | vmstounix > $(d0root)/errmsg_util/source/errdsp.f
$(d0root)/errmsg_util/source/errfat.f:\
  $(d0root)/errmsg_util/source/errfat.for\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errfat.for | vmstounix > $(d0root)/errmsg_util/source/errfat.f
$(d0root)/errmsg_util/source/errfnd.f:\
  $(d0root)/errmsg_util/source/errfnd.for\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errfnd.for | vmstounix > $(d0root)/errmsg_util/source/errfnd.f
$(d0root)/errmsg_util/source/errget.f:\
  $(d0root)/errmsg_util/source/errget.for\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errcnt.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errget.for | vmstounix > $(d0root)/errmsg_util/source/errget.f
$(d0root)/errmsg_util/source/errhan_e.f:\
  $(d0root)/errmsg_util/source/errhan_e.for\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errhan_e.for | vmstounix > $(d0root)/errmsg_util/source/errhan_e.f
$(d0root)/errmsg_util/source/errini.f:\
  $(d0root)/errmsg_util/source/errini.for\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errini.for | vmstounix > $(d0root)/errmsg_util/source/errini.f
$(d0root)/errmsg_util/source/errins.f:\
  $(d0root)/errmsg_util/source/errins.for\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errins.for | vmstounix > $(d0root)/errmsg_util/source/errins.f
$(d0root)/errmsg_util/source/errint.f:\
  $(d0root)/errmsg_util/source/errint.for\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errint.for | vmstounix > $(d0root)/errmsg_util/source/errint.f
$(d0root)/errmsg_util/source/errlog.f:\
  $(d0root)/errmsg_util/source/errlog.for\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errlog.for | vmstounix > $(d0root)/errmsg_util/source/errlog.f
$(d0root)/errmsg_util/source/errmax.f:\
  $(d0root)/errmsg_util/source/errmax.for\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errmax.for | vmstounix > $(d0root)/errmsg_util/source/errmax.f
$(d0root)/errmsg_util/source/errmax_rcp.f:\
  $(d0root)/errmsg_util/source/errmax_rcp.for
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errmax_rcp.for | vmstounix > $(d0root)/errmsg_util/source/errmax_rcp.f
$(d0root)/errmsg_util/source/errmsg.f:\
  $(d0root)/errmsg_util/source/errmsg.for\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errmsg.for | vmstounix > $(d0root)/errmsg_util/source/errmsg.f
$(d0root)/errmsg_util/source/errmzb.f:\
  $(d0root)/errmsg_util/source/errmzb.for\
  $(d0library)/inc/zebcom.inc\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errzst.inc\
  $(d0library)/inc/errzst_c.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errmzb.for | vmstounix > $(d0root)/errmsg_util/source/errmzb.f
$(d0root)/errmsg_util/source/errscr.f:\
  $(d0root)/errmsg_util/source/errscr.for\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errscr.for | vmstounix > $(d0root)/errmsg_util/source/errscr.f
$(d0root)/errmsg_util/source/errsum.f:\
  $(d0root)/errmsg_util/source/errsum.for\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errsum.for | vmstounix > $(d0root)/errmsg_util/source/errsum.f
$(d0root)/errmsg_util/source/errwrn.f:\
  $(d0root)/errmsg_util/source/errwrn.for\
  $(d0root)/errmsg_util/params/errmsg.params\
  $(d0library)/inc/errctl.inc\
  $(d0library)/inc/errcnt.inc\
  $(d0library)/inc/errids.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/errwrn.for | vmstounix > $(d0root)/errmsg_util/source/errwrn.f
$(d0root)/errmsg_util/source/ewrnwr.f:\
  $(d0root)/errmsg_util/source/ewrnwr.for
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/ewrnwr.for | vmstounix > $(d0root)/errmsg_util/source/ewrnwr.f
$(d0root)/errmsg_util/source/gzermg.f:\
  $(d0root)/errmsg_util/source/gzermg.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izermg.link
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/gzermg.for | vmstounix > $(d0root)/errmsg_util/source/gzermg.f
$(d0root)/errmsg_util/source/prermg.f:\
  $(d0root)/errmsg_util/source/prermg.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izermg.link\
  $(d0library)/inc/errzst.inc\
  $(d0library)/inc/errzst_c.inc
	tasteofd0 $(FLAVOR) < $(d0root)/errmsg_util/source/prermg.for | vmstounix > $(d0root)/errmsg_util/source/prermg.f
for:
	touch $(d0root)/errmsg_util/source/bkermg.for
	touch $(d0root)/errmsg_util/source/elogwr.for
	touch $(d0root)/errmsg_util/source/ermgfl.for
	touch $(d0root)/errmsg_util/source/errclr.for
	touch $(d0root)/errmsg_util/source/errdsp.for
	touch $(d0root)/errmsg_util/source/errfat.for
	touch $(d0root)/errmsg_util/source/errfnd.for
	touch $(d0root)/errmsg_util/source/errget.for
	touch $(d0root)/errmsg_util/source/errhan_e.for
	touch $(d0root)/errmsg_util/source/errini.for
	touch $(d0root)/errmsg_util/source/errins.for
	touch $(d0root)/errmsg_util/source/errint.for
	touch $(d0root)/errmsg_util/source/errlog.for
	touch $(d0root)/errmsg_util/source/errmax.for
	touch $(d0root)/errmsg_util/source/errmax_rcp.for
	touch $(d0root)/errmsg_util/source/errmsg.for
	touch $(d0root)/errmsg_util/source/errmzb.for
	touch $(d0root)/errmsg_util/source/errscr.for
	touch $(d0root)/errmsg_util/source/errsum.for
	touch $(d0root)/errmsg_util/source/errwrn.for
	touch $(d0root)/errmsg_util/source/ewrnwr.for
	touch $(d0root)/errmsg_util/source/gzermg.for
	touch $(d0root)/errmsg_util/source/prermg.for
