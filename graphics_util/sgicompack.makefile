SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Olimit 2500 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/admin/tmp/d0librar/userlib/2979/sgicompack
FLAVOR = SIUNIX
CC = cc
LEX = lex
F77 = f77
.IGNORE:
.SUFFIXES:
debug :\
  deb_sgicompack.a
opt :\
  sgicompack.a
deb_sgicompack.a : $(d0root)/graphics_util/deb_sgicompack.a
	@ echo deb_sgicompack.a is up to date
$(d0root)/graphics_util/deb_sgicompack.a:: $(SCRATCH)/nothing.nl
$(d0root)/graphics_util/deb_sgicompack.a::\
  $(d0root)/graphics_util/deb_sgicompack.a(ding_bell.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(makemeni.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(makemenu.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(mvlogi.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(mvstrn.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(myl.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(rdstrn.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_canmen.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_diabox.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_dtems827.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_eximen.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_getpar.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_getpar77.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_hlproc.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_intast.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_intmen.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_menadd.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_mendef.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_mennew.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_menset.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_menudo.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_menuex.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_menuop.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_mrams815.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_outmsg.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_pfget.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_pflabl.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_pfnum.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_pfwait.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_ptems154.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_setsta.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_splsta.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_spltit.o)\
  $(d0root)/graphics_util/deb_sgicompack.a(sgi_stamsg.o)
	ar $(ARFLAGS) $(d0root)/graphics_util/deb_sgicompack.a `ls $(SCRATCH)/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/deb_sgicompack.a `ls $(SCRATCH)/a2/*.o 2> /dev/null`
	rmdirp $(SCRATCH)
sgicompack.a : $(d0root)/graphics_util/sgicompack.a
	@ echo sgicompack.a is up to date
$(d0root)/graphics_util/sgicompack.a:: $(SCRATCH)/nothing.nl
$(d0root)/graphics_util/sgicompack.a::\
  $(d0root)/graphics_util/sgicompack.a(ding_bell.o)\
  $(d0root)/graphics_util/sgicompack.a(makemeni.o)\
  $(d0root)/graphics_util/sgicompack.a(makemenu.o)\
  $(d0root)/graphics_util/sgicompack.a(mvlogi.o)\
  $(d0root)/graphics_util/sgicompack.a(mvstrn.o)\
  $(d0root)/graphics_util/sgicompack.a(myl.o)\
  $(d0root)/graphics_util/sgicompack.a(rdstrn.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_canmen.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_diabox.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_dtems827.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_eximen.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_getpar.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_getpar77.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_hlproc.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_intast.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_intmen.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_menadd.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_mendef.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_mennew.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_menset.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_menudo.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_menuex.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_menuop.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_mrams815.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_outmsg.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_pfget.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_pflabl.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_pfnum.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_pfwait.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_ptems154.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_setsta.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_splsta.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_spltit.o)\
  $(d0root)/graphics_util/sgicompack.a(sgi_stamsg.o)
	ar $(ARFLAGS) $(d0root)/graphics_util/sgicompack.a `ls $(SCRATCH)/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/graphics_util/sgicompack.a `ls $(SCRATCH)/a2/*.o 2> /dev/null`
	rmdirp $(SCRATCH)
$(SCRATCH)/nothing.nl:
	rmdirp $(SCRATCH)
	mkdirp $(SCRATCH)/a1
	mkdirp $(SCRATCH)/a2
$(d0root)/graphics_util/deb_sgicompack.a(ding_bell.o):\
  $(d0root)/graphics_util/sgicompack/ding_bell.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/ding_bell.f ;\
	mv ding_bell.o $(SCRATCH)/a1/ding_bell.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(makemeni.o):\
  $(d0root)/graphics_util/sgicompack/makemeni.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/makemeni.f ;\
	mv makemeni.o $(SCRATCH)/a1/makemeni.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(makemenu.o):\
  $(d0root)/graphics_util/sgicompack/makemenu.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/makemenu.f ;\
	mv makemenu.o $(SCRATCH)/a1/makemenu.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(mvlogi.o):\
  $(d0root)/graphics_util/sgicompack/mvlogi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/mvlogi.f ;\
	mv mvlogi.o $(SCRATCH)/a1/mvlogi.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(mvstrn.o):\
  $(d0root)/graphics_util/sgicompack/mvstrn.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/mvstrn.f ;\
	mv mvstrn.o $(SCRATCH)/a1/mvstrn.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(myl.o):\
  $(d0root)/graphics_util/sgicompack/myl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/myl.f ;\
	mv myl.o $(SCRATCH)/a1/myl.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(rdstrn.o):\
  $(d0root)/graphics_util/sgicompack/rdstrn.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/rdstrn.f ;\
	mv rdstrn.o $(SCRATCH)/a1/rdstrn.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_canmen.o):\
  $(d0root)/graphics_util/sgicompack/sgi_canmen.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_canmen.f ;\
	mv sgi_canmen.o $(SCRATCH)/a1/sgi_canmen.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_diabox.o):\
  $(d0root)/graphics_util/sgicompack/sgi_diabox.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_diabox.f ;\
	mv sgi_diabox.o $(SCRATCH)/a1/sgi_diabox.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_dtems827.o):\
  $(d0root)/graphics_util/sgicompack/sgi_display_items.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_display_items.f ;\
	mv sgi_display_items.o $(SCRATCH)/a1/sgi_dtems827.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_eximen.o):\
  $(d0root)/graphics_util/sgicompack/sgi_eximen.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_eximen.f ;\
	mv sgi_eximen.o $(SCRATCH)/a1/sgi_eximen.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_getpar.o):\
  $(d0root)/graphics_util/sgicompack/sgi_getpar.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/compack/source/commsg.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_getpar.f ;\
	mv sgi_getpar.o $(SCRATCH)/a1/sgi_getpar.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_getpar77.o):\
  $(d0root)/graphics_util/sgicompack/sgi_getpar77.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/compack/source/commsg.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_getpar77.f ;\
	mv sgi_getpar77.o $(SCRATCH)/a1/sgi_getpar77.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_hlproc.o):\
  $(d0root)/graphics_util/sgicompack/sgi_hlproc.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_hlproc.f ;\
	mv sgi_hlproc.o $(SCRATCH)/a1/sgi_hlproc.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_intast.o):\
  $(d0root)/graphics_util/sgicompack/sgi_intast.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_intast.f ;\
	mv sgi_intast.o $(SCRATCH)/a1/sgi_intast.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_intmen.o):\
  $(d0root)/graphics_util/sgicompack/sgi_intmen.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_intmen.f ;\
	mv sgi_intmen.o $(SCRATCH)/a1/sgi_intmen.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_menadd.o):\
  $(d0root)/graphics_util/sgicompack/sgi_menadd.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_menadd.f ;\
	mv sgi_menadd.o $(SCRATCH)/a1/sgi_menadd.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_mendef.o):\
  $(d0root)/graphics_util/sgicompack/sgi_mendef.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_mendef.f ;\
	mv sgi_mendef.o $(SCRATCH)/a1/sgi_mendef.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_mennew.o):\
  $(d0root)/graphics_util/sgicompack/sgi_mennew.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_mennew.f ;\
	mv sgi_mennew.o $(SCRATCH)/a1/sgi_mennew.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_menset.o):\
  $(d0root)/graphics_util/sgicompack/sgi_menset.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_menset.f ;\
	mv sgi_menset.o $(SCRATCH)/a1/sgi_menset.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_menudo.o):\
  $(d0root)/graphics_util/sgicompack/sgi_menudo.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_menudo.f ;\
	mv sgi_menudo.o $(SCRATCH)/a2/sgi_menudo.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_menuex.o):\
  $(d0root)/graphics_util/sgicompack/sgi_menuex.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_menuex.f ;\
	mv sgi_menuex.o $(SCRATCH)/a2/sgi_menuex.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_menuop.o):\
  $(d0root)/graphics_util/sgicompack/sgi_menuop.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_menuop.f ;\
	mv sgi_menuop.o $(SCRATCH)/a2/sgi_menuop.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_mrams815.o):\
  $(d0root)/graphics_util/sgicompack/sgi_modify_params.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_modify_params.f ;\
	mv sgi_modify_params.o $(SCRATCH)/a2/sgi_mrams815.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_outmsg.o):\
  $(d0root)/graphics_util/sgicompack/sgi_outmsg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_outmsg.f ;\
	mv sgi_outmsg.o $(SCRATCH)/a2/sgi_outmsg.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_pfget.o):\
  $(d0root)/graphics_util/sgicompack/sgi_pfget.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_pfget.f ;\
	mv sgi_pfget.o $(SCRATCH)/a2/sgi_pfget.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_pflabl.o):\
  $(d0root)/graphics_util/sgicompack/sgi_pflabl.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_pflabl.f ;\
	mv sgi_pflabl.o $(SCRATCH)/a2/sgi_pflabl.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_pfnum.o):\
  $(d0root)/graphics_util/sgicompack/sgi_pfnum.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_pfnum.f ;\
	mv sgi_pfnum.o $(SCRATCH)/a2/sgi_pfnum.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_pfwait.o):\
  $(d0root)/graphics_util/sgicompack/sgi_pfwait.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_pfwait.f ;\
	mv sgi_pfwait.o $(SCRATCH)/a2/sgi_pfwait.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_ptems154.o):\
  $(d0root)/graphics_util/sgicompack/sgi_px_display_items.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_px_display_items.f ;\
	mv sgi_px_display_items.o $(SCRATCH)/a2/sgi_ptems154.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_setsta.o):\
  $(d0root)/graphics_util/sgicompack/sgi_setsta.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_setsta.f ;\
	mv sgi_setsta.o $(SCRATCH)/a2/sgi_setsta.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_splsta.o):\
  $(d0root)/graphics_util/sgicompack/sgi_splsta.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_splsta.f ;\
	mv sgi_splsta.o $(SCRATCH)/a2/sgi_splsta.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_spltit.o):\
  $(d0root)/graphics_util/sgicompack/sgi_spltit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_spltit.f ;\
	mv sgi_spltit.o $(SCRATCH)/a2/sgi_spltit.o ;\
	)
$(d0root)/graphics_util/deb_sgicompack.a(sgi_stamsg.o):\
  $(d0root)/graphics_util/sgicompack/sgi_stamsg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) graphics_util/sgicompack/sgi_stamsg.f ;\
	mv sgi_stamsg.o $(SCRATCH)/a2/sgi_stamsg.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(ding_bell.o):\
  $(d0root)/graphics_util/sgicompack/ding_bell.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/ding_bell.f ;\
	mv ding_bell.o $(SCRATCH)/a1/ding_bell.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(makemeni.o):\
  $(d0root)/graphics_util/sgicompack/makemeni.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/makemeni.f ;\
	mv makemeni.o $(SCRATCH)/a1/makemeni.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(makemenu.o):\
  $(d0root)/graphics_util/sgicompack/makemenu.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/makemenu.f ;\
	mv makemenu.o $(SCRATCH)/a1/makemenu.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(mvlogi.o):\
  $(d0root)/graphics_util/sgicompack/mvlogi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/mvlogi.f ;\
	mv mvlogi.o $(SCRATCH)/a1/mvlogi.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(mvstrn.o):\
  $(d0root)/graphics_util/sgicompack/mvstrn.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/mvstrn.f ;\
	mv mvstrn.o $(SCRATCH)/a1/mvstrn.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(myl.o):\
  $(d0root)/graphics_util/sgicompack/myl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/myl.f ;\
	mv myl.o $(SCRATCH)/a1/myl.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(rdstrn.o):\
  $(d0root)/graphics_util/sgicompack/rdstrn.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/rdstrn.f ;\
	mv rdstrn.o $(SCRATCH)/a1/rdstrn.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_canmen.o):\
  $(d0root)/graphics_util/sgicompack/sgi_canmen.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_canmen.f ;\
	mv sgi_canmen.o $(SCRATCH)/a1/sgi_canmen.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_diabox.o):\
  $(d0root)/graphics_util/sgicompack/sgi_diabox.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_diabox.f ;\
	mv sgi_diabox.o $(SCRATCH)/a1/sgi_diabox.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_dtems827.o):\
  $(d0root)/graphics_util/sgicompack/sgi_display_items.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_display_items.f ;\
	mv sgi_display_items.o $(SCRATCH)/a1/sgi_dtems827.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_eximen.o):\
  $(d0root)/graphics_util/sgicompack/sgi_eximen.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_eximen.f ;\
	mv sgi_eximen.o $(SCRATCH)/a1/sgi_eximen.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_getpar.o):\
  $(d0root)/graphics_util/sgicompack/sgi_getpar.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/compack/source/commsg.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_getpar.f ;\
	mv sgi_getpar.o $(SCRATCH)/a1/sgi_getpar.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_getpar77.o):\
  $(d0root)/graphics_util/sgicompack/sgi_getpar77.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/compack/source/commsg.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_getpar77.f ;\
	mv sgi_getpar77.o $(SCRATCH)/a1/sgi_getpar77.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_hlproc.o):\
  $(d0root)/graphics_util/sgicompack/sgi_hlproc.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_hlproc.f ;\
	mv sgi_hlproc.o $(SCRATCH)/a1/sgi_hlproc.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_intast.o):\
  $(d0root)/graphics_util/sgicompack/sgi_intast.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_intast.f ;\
	mv sgi_intast.o $(SCRATCH)/a1/sgi_intast.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_intmen.o):\
  $(d0root)/graphics_util/sgicompack/sgi_intmen.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_intmen.f ;\
	mv sgi_intmen.o $(SCRATCH)/a1/sgi_intmen.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_menadd.o):\
  $(d0root)/graphics_util/sgicompack/sgi_menadd.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_menadd.f ;\
	mv sgi_menadd.o $(SCRATCH)/a1/sgi_menadd.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_mendef.o):\
  $(d0root)/graphics_util/sgicompack/sgi_mendef.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_mendef.f ;\
	mv sgi_mendef.o $(SCRATCH)/a1/sgi_mendef.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_mennew.o):\
  $(d0root)/graphics_util/sgicompack/sgi_mennew.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_mennew.f ;\
	mv sgi_mennew.o $(SCRATCH)/a1/sgi_mennew.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_menset.o):\
  $(d0root)/graphics_util/sgicompack/sgi_menset.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_menset.f ;\
	mv sgi_menset.o $(SCRATCH)/a1/sgi_menset.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_menudo.o):\
  $(d0root)/graphics_util/sgicompack/sgi_menudo.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_menudo.f ;\
	mv sgi_menudo.o $(SCRATCH)/a2/sgi_menudo.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_menuex.o):\
  $(d0root)/graphics_util/sgicompack/sgi_menuex.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_menuex.f ;\
	mv sgi_menuex.o $(SCRATCH)/a2/sgi_menuex.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_menuop.o):\
  $(d0root)/graphics_util/sgicompack/sgi_menuop.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_menuop.f ;\
	mv sgi_menuop.o $(SCRATCH)/a2/sgi_menuop.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_mrams815.o):\
  $(d0root)/graphics_util/sgicompack/sgi_modify_params.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_modify_params.f ;\
	mv sgi_modify_params.o $(SCRATCH)/a2/sgi_mrams815.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_outmsg.o):\
  $(d0root)/graphics_util/sgicompack/sgi_outmsg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_outmsg.f ;\
	mv sgi_outmsg.o $(SCRATCH)/a2/sgi_outmsg.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_pfget.o):\
  $(d0root)/graphics_util/sgicompack/sgi_pfget.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_pfget.f ;\
	mv sgi_pfget.o $(SCRATCH)/a2/sgi_pfget.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_pflabl.o):\
  $(d0root)/graphics_util/sgicompack/sgi_pflabl.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_pflabl.f ;\
	mv sgi_pflabl.o $(SCRATCH)/a2/sgi_pflabl.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_pfnum.o):\
  $(d0root)/graphics_util/sgicompack/sgi_pfnum.f\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_pfnum.f ;\
	mv sgi_pfnum.o $(SCRATCH)/a2/sgi_pfnum.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_pfwait.o):\
  $(d0root)/graphics_util/sgicompack/sgi_pfwait.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_pfwait.f ;\
	mv sgi_pfwait.o $(SCRATCH)/a2/sgi_pfwait.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_ptems154.o):\
  $(d0root)/graphics_util/sgicompack/sgi_px_display_items.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_px_display_items.f ;\
	mv sgi_px_display_items.o $(SCRATCH)/a2/sgi_ptems154.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_setsta.o):\
  $(d0root)/graphics_util/sgicompack/sgi_setsta.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_setsta.f ;\
	mv sgi_setsta.o $(SCRATCH)/a2/sgi_setsta.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_splsta.o):\
  $(d0root)/graphics_util/sgicompack/sgi_splsta.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_splsta.f ;\
	mv sgi_splsta.o $(SCRATCH)/a2/sgi_splsta.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_spltit.o):\
  $(d0root)/graphics_util/sgicompack/sgi_spltit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_spltit.f ;\
	mv sgi_spltit.o $(SCRATCH)/a2/sgi_spltit.o ;\
	)
$(d0root)/graphics_util/sgicompack.a(sgi_stamsg.o):\
  $(d0root)/graphics_util/sgicompack/sgi_stamsg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) graphics_util/sgicompack/sgi_stamsg.f ;\
	mv sgi_stamsg.o $(SCRATCH)/a2/sgi_stamsg.o ;\
	)
pre:\
  $(d0root)/graphics_util/sgicompack/ding_bell.f\
  $(d0root)/graphics_util/sgicompack/makemeni.f\
  $(d0root)/graphics_util/sgicompack/makemenu.f\
  $(d0root)/graphics_util/sgicompack/mvlogi.f\
  $(d0root)/graphics_util/sgicompack/mvstrn.f\
  $(d0root)/graphics_util/sgicompack/myl.f\
  $(d0root)/graphics_util/sgicompack/rdstrn.f\
  $(d0root)/graphics_util/sgicompack/sgi_canmen.f\
  $(d0root)/graphics_util/sgicompack/sgi_diabox.f\
  $(d0root)/graphics_util/sgicompack/sgi_display_items.f\
  $(d0root)/graphics_util/sgicompack/sgi_eximen.f\
  $(d0root)/graphics_util/sgicompack/sgi_getpar.f\
  $(d0root)/graphics_util/sgicompack/sgi_getpar77.f\
  $(d0root)/graphics_util/sgicompack/sgi_hlproc.f\
  $(d0root)/graphics_util/sgicompack/sgi_intast.f\
  $(d0root)/graphics_util/sgicompack/sgi_intmen.f\
  $(d0root)/graphics_util/sgicompack/sgi_menadd.f\
  $(d0root)/graphics_util/sgicompack/sgi_mendef.f\
  $(d0root)/graphics_util/sgicompack/sgi_mennew.f\
  $(d0root)/graphics_util/sgicompack/sgi_menset.f\
  $(d0root)/graphics_util/sgicompack/sgi_menudo.f\
  $(d0root)/graphics_util/sgicompack/sgi_menuex.f\
  $(d0root)/graphics_util/sgicompack/sgi_menuop.f\
  $(d0root)/graphics_util/sgicompack/sgi_modify_params.f\
  $(d0root)/graphics_util/sgicompack/sgi_outmsg.f\
  $(d0root)/graphics_util/sgicompack/sgi_pfget.f\
  $(d0root)/graphics_util/sgicompack/sgi_pflabl.f\
  $(d0root)/graphics_util/sgicompack/sgi_pfnum.f\
  $(d0root)/graphics_util/sgicompack/sgi_pfwait.f\
  $(d0root)/graphics_util/sgicompack/sgi_px_display_items.f\
  $(d0root)/graphics_util/sgicompack/sgi_setsta.f\
  $(d0root)/graphics_util/sgicompack/sgi_splsta.f\
  $(d0root)/graphics_util/sgicompack/sgi_spltit.f\
  $(d0root)/graphics_util/sgicompack/sgi_stamsg.f
$(d0root)/graphics_util/sgicompack/ding_bell.f:\
  $(d0root)/graphics_util/sgicompack/ding_bell.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/ding_bell.for | vmstounix > $(d0root)/graphics_util/sgicompack/ding_bell.f
$(d0root)/graphics_util/sgicompack/makemeni.f:\
  $(d0root)/graphics_util/sgicompack/makemeni.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/makemeni.for | vmstounix > $(d0root)/graphics_util/sgicompack/makemeni.f
$(d0root)/graphics_util/sgicompack/makemenu.f:\
  $(d0root)/graphics_util/sgicompack/makemenu.for\
  $(d0root)/graphics_util/sgicompack/mainpack.inc\
  $(d0root)/graphics_util/sgidi3/di3gl.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/makemenu.for | vmstounix > $(d0root)/graphics_util/sgicompack/makemenu.f
$(d0root)/graphics_util/sgicompack/mvlogi.f:\
  $(d0root)/graphics_util/sgicompack/mvlogi.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/mvlogi.for | vmstounix > $(d0root)/graphics_util/sgicompack/mvlogi.f
$(d0root)/graphics_util/sgicompack/mvstrn.f:\
  $(d0root)/graphics_util/sgicompack/mvstrn.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/mvstrn.for | vmstounix > $(d0root)/graphics_util/sgicompack/mvstrn.f
$(d0root)/graphics_util/sgicompack/myl.f:\
  $(d0root)/graphics_util/sgicompack/myl.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/myl.for | vmstounix > $(d0root)/graphics_util/sgicompack/myl.f
$(d0root)/graphics_util/sgicompack/rdstrn.f:\
  $(d0root)/graphics_util/sgicompack/rdstrn.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/rdstrn.for | vmstounix > $(d0root)/graphics_util/sgicompack/rdstrn.f
$(d0root)/graphics_util/sgicompack/sgi_canmen.f:\
  $(d0root)/graphics_util/sgicompack/sgi_canmen.for\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_canmen.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_canmen.f
$(d0root)/graphics_util/sgicompack/sgi_diabox.f:\
  $(d0root)/graphics_util/sgicompack/sgi_diabox.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_diabox.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_diabox.f
$(d0root)/graphics_util/sgicompack/sgi_display_items.f:\
  $(d0root)/graphics_util/sgicompack/sgi_display_items.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_display_items.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_display_items.f
$(d0root)/graphics_util/sgicompack/sgi_eximen.f:\
  $(d0root)/graphics_util/sgicompack/sgi_eximen.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_eximen.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_eximen.f
$(d0root)/graphics_util/sgicompack/sgi_getpar.f:\
  $(d0root)/graphics_util/sgicompack/sgi_getpar.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/compack/source/commsg.def
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_getpar.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_getpar.f
$(d0root)/graphics_util/sgicompack/sgi_getpar77.f:\
  $(d0root)/graphics_util/sgicompack/sgi_getpar77.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/compack/source/commsg.def
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_getpar77.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_getpar77.f
$(d0root)/graphics_util/sgicompack/sgi_hlproc.f:\
  $(d0root)/graphics_util/sgicompack/sgi_hlproc.for\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_hlproc.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_hlproc.f
$(d0root)/graphics_util/sgicompack/sgi_intast.f:\
  $(d0root)/graphics_util/sgicompack/sgi_intast.for\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_intast.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_intast.f
$(d0root)/graphics_util/sgicompack/sgi_intmen.f:\
  $(d0root)/graphics_util/sgicompack/sgi_intmen.for\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_intmen.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_intmen.f
$(d0root)/graphics_util/sgicompack/sgi_menadd.f:\
  $(d0root)/graphics_util/sgicompack/sgi_menadd.for\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_menadd.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_menadd.f
$(d0root)/graphics_util/sgicompack/sgi_mendef.f:\
  $(d0root)/graphics_util/sgicompack/sgi_mendef.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_mendef.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_mendef.f
$(d0root)/graphics_util/sgicompack/sgi_mennew.f:\
  $(d0root)/graphics_util/sgicompack/sgi_mennew.for\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_mennew.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_mennew.f
$(d0root)/graphics_util/sgicompack/sgi_menset.f:\
  $(d0root)/graphics_util/sgicompack/sgi_menset.for\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_menset.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_menset.f
$(d0root)/graphics_util/sgicompack/sgi_menudo.f:\
  $(d0root)/graphics_util/sgicompack/sgi_menudo.for\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_menudo.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_menudo.f
$(d0root)/graphics_util/sgicompack/sgi_menuex.f:\
  $(d0root)/graphics_util/sgicompack/sgi_menuex.for\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_menuex.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_menuex.f
$(d0root)/graphics_util/sgicompack/sgi_menuop.f:\
  $(d0root)/graphics_util/sgicompack/sgi_menuop.for\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_menuop.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_menuop.f
$(d0root)/graphics_util/sgicompack/sgi_modify_params.f:\
  $(d0root)/graphics_util/sgicompack/sgi_modify_params.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_modify_params.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_modify_params.f
$(d0root)/graphics_util/sgicompack/sgi_outmsg.f:\
  $(d0root)/graphics_util/sgicompack/sgi_outmsg.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_outmsg.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_outmsg.f
$(d0root)/graphics_util/sgicompack/sgi_pfget.f:\
  $(d0root)/graphics_util/sgicompack/sgi_pfget.for\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_pfget.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_pfget.f
$(d0root)/graphics_util/sgicompack/sgi_pflabl.f:\
  $(d0root)/graphics_util/sgicompack/sgi_pflabl.for\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_pflabl.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_pflabl.f
$(d0root)/graphics_util/sgicompack/sgi_pfnum.f:\
  $(d0root)/graphics_util/sgicompack/sgi_pfnum.for\
  $(d0root)/graphics_util/sgicompack/mainpack.inc
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_pfnum.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_pfnum.f
$(d0root)/graphics_util/sgicompack/sgi_pfwait.f:\
  $(d0root)/graphics_util/sgicompack/sgi_pfwait.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_pfwait.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_pfwait.f
$(d0root)/graphics_util/sgicompack/sgi_px_display_items.f:\
  $(d0root)/graphics_util/sgicompack/sgi_px_display_items.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_px_display_items.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_px_display_items.f
$(d0root)/graphics_util/sgicompack/sgi_setsta.f:\
  $(d0root)/graphics_util/sgicompack/sgi_setsta.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_setsta.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_setsta.f
$(d0root)/graphics_util/sgicompack/sgi_splsta.f:\
  $(d0root)/graphics_util/sgicompack/sgi_splsta.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_splsta.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_splsta.f
$(d0root)/graphics_util/sgicompack/sgi_spltit.f:\
  $(d0root)/graphics_util/sgicompack/sgi_spltit.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_spltit.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_spltit.f
$(d0root)/graphics_util/sgicompack/sgi_stamsg.f:\
  $(d0root)/graphics_util/sgicompack/sgi_stamsg.for
	tasteofd0 $(FLAVOR) < $(d0root)/graphics_util/sgicompack/sgi_stamsg.for | vmstounix > $(d0root)/graphics_util/sgicompack/sgi_stamsg.f
for:
	touch $(d0root)/graphics_util/sgicompack/ding_bell.for
	touch $(d0root)/graphics_util/sgicompack/makemeni.for
	touch $(d0root)/graphics_util/sgicompack/makemenu.for
	touch $(d0root)/graphics_util/sgicompack/mvlogi.for
	touch $(d0root)/graphics_util/sgicompack/mvstrn.for
	touch $(d0root)/graphics_util/sgicompack/myl.for
	touch $(d0root)/graphics_util/sgicompack/rdstrn.for
	touch $(d0root)/graphics_util/sgicompack/sgi_canmen.for
	touch $(d0root)/graphics_util/sgicompack/sgi_diabox.for
	touch $(d0root)/graphics_util/sgicompack/sgi_display_items.for
	touch $(d0root)/graphics_util/sgicompack/sgi_eximen.for
	touch $(d0root)/graphics_util/sgicompack/sgi_getpar.for
	touch $(d0root)/graphics_util/sgicompack/sgi_getpar77.for
	touch $(d0root)/graphics_util/sgicompack/sgi_hlproc.for
	touch $(d0root)/graphics_util/sgicompack/sgi_intast.for
	touch $(d0root)/graphics_util/sgicompack/sgi_intmen.for
	touch $(d0root)/graphics_util/sgicompack/sgi_menadd.for
	touch $(d0root)/graphics_util/sgicompack/sgi_mendef.for
	touch $(d0root)/graphics_util/sgicompack/sgi_mennew.for
	touch $(d0root)/graphics_util/sgicompack/sgi_menset.for
	touch $(d0root)/graphics_util/sgicompack/sgi_menudo.for
	touch $(d0root)/graphics_util/sgicompack/sgi_menuex.for
	touch $(d0root)/graphics_util/sgicompack/sgi_menuop.for
	touch $(d0root)/graphics_util/sgicompack/sgi_modify_params.for
	touch $(d0root)/graphics_util/sgicompack/sgi_outmsg.for
	touch $(d0root)/graphics_util/sgicompack/sgi_pfget.for
	touch $(d0root)/graphics_util/sgicompack/sgi_pflabl.for
	touch $(d0root)/graphics_util/sgicompack/sgi_pfnum.for
	touch $(d0root)/graphics_util/sgicompack/sgi_pfwait.for
	touch $(d0root)/graphics_util/sgicompack/sgi_px_display_items.for
	touch $(d0root)/graphics_util/sgicompack/sgi_setsta.for
	touch $(d0root)/graphics_util/sgicompack/sgi_splsta.for
	touch $(d0root)/graphics_util/sgicompack/sgi_spltit.for
	touch $(d0root)/graphics_util/sgicompack/sgi_stamsg.for
