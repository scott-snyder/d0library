SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0gamma)/unix/source -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0gamma)/unix/source -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Olimit 2500 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/admin/tmp/d0librar/mmstomake/lcompack
FLAVOR = SIUNIX
F77 = f77
.IGNORE:
.SUFFIXES:

opt :\
  compack.a
compack.a : $(d0root)/lcompack/compack.a
	@ echo compack.a is up to date
$(d0root)/lcompack/compack.a:: $(SCRATCH)/compack/nothing.nl
$(d0root)/lcompack/compack.a::\
  $(d0root)/lcompack/compack.a(abomen.o)\
  $(d0root)/lcompack/compack.a(additm.o)\
  $(d0root)/lcompack/compack.a(canmen.o)\
  $(d0root)/lcompack/compack.a(chcols.o)\
  $(d0root)/lcompack/compack.a(chkist.o)\
  $(d0root)/lcompack/compack.a(chspac.o)\
  $(d0root)/lcompack/compack.a(conhex.o)\
  $(d0root)/lcompack/compack.a(conint.o)\
  $(d0root)/lcompack/compack.a(conrea.o)\
  $(d0root)/lcompack/compack.a(contry.o)\
  $(d0root)/lcompack/compack.a(curlin.o)\
  $(d0root)/lcompack/compack.a(defcol.o)\
  $(d0root)/lcompack/compack.a(defspa.o)\
  $(d0root)/lcompack/compack.a(disabl.o)\
  $(d0root)/lcompack/compack.a(dmpmsg.o)\
  $(d0root)/lcompack/compack.a(doproc.o)\
  $(d0root)/lcompack/compack.a(enddmp.o)\
  $(d0root)/lcompack/compack.a(endlog.o)\
  $(d0root)/lcompack/compack.a(endspl.o)\
  $(d0root)/lcompack/compack.a(endsta.o)\
  $(d0root)/lcompack/compack.a(evecom.o)\
  $(d0root)/lcompack/compack.a(evesav.o)\
  $(d0root)/lcompack/compack.a(eximen.o)\
  $(d0root)/lcompack/compack.a(exiusr.o)\
  $(d0root)/lcompack/compack.a(fillst.o)\
  $(d0root)/lcompack/compack.a(fndfil.o)\
  $(d0root)/lcompack/compack.a(getarr.o)\
  $(d0root)/lcompack/compack.a(getdis.o)\
  $(d0root)/lcompack/compack.a(getdis77.o)\
  $(d0root)/lcompack/compack.a(getopt.o)\
  $(d0root)/lcompack/compack.a(getpar.o)\
  $(d0root)/lcompack/compack.a(getpar77.o)\
  $(d0root)/lcompack/compack.a(gline0.o)\
  $(d0root)/lcompack/compack.a(gnosmg.o)\
  $(d0root)/lcompack/compack.a(gotop.o)\
  $(d0root)/lcompack/compack.a(hlproc.o)\
  $(d0root)/lcompack/compack.a(inpcmd.o)\
  $(d0root)/lcompack/compack.a(inptrm.o)\
  $(d0root)/lcompack/compack.a(intast.o)\
  $(d0root)/lcompack/compack.a(intmen.o)\
  $(d0root)/lcompack/compack.a(intmsg.o)\
  $(d0root)/lcompack/compack.a(libera.o)\
  $(d0root)/lcompack/compack.a(librep.o)\
  $(d0root)/lcompack/compack.a(linchk.o)\
  $(d0root)/lcompack/compack.a(lines0.o)\
  $(d0root)/lcompack/compack.a(lspawn.o)\
  $(d0root)/lcompack/compack.a(menadd.o)\
  $(d0root)/lcompack/compack.a(menctr.o)\
  $(d0root)/lcompack/compack.a(mendef.o)\
  $(d0root)/lcompack/compack.a(mendis.o)\
  $(d0root)/lcompack/compack.a(menexg.o)\
  $(d0root)/lcompack/compack.a(mennew.o)\
  $(d0root)/lcompack/compack.a(menset.o)\
  $(d0root)/lcompack/compack.a(menudo.o)\
  $(d0root)/lcompack/compack.a(menuex.o)\
  $(d0root)/lcompack/compack.a(movint.o)\
  $(d0root)/lcompack/compack.a(movstr.o)\
  $(d0root)/lcompack/compack.a(msgscr.o)\
  $(d0root)/lcompack/compack.a(newpar.o)\
  $(d0root)/lcompack/compack.a(nulhdl.o)\
  $(d0root)/lcompack/compack.a(oline0.o)\
  $(d0root)/lcompack/compack.a(outmsg.o)\
  $(d0root)/lcompack/compack.a(outsav.o)\
  $(d0root)/lcompack/compack.a(pareal.o)\
  $(d0root)/lcompack/compack.a(parint.o)\
  $(d0root)/lcompack/compack.a(parlog.o)\
  $(d0root)/lcompack/compack.a(pfnum.o)\
  $(d0root)/lcompack/compack.a(pfwait.o)\
  $(d0root)/lcompack/compack.a(prscom.o)\
  $(d0root)/lcompack/compack.a(putmsg.o)\
  $(d0root)/lcompack/compack.a(rdset.o)\
  $(d0root)/lcompack/compack.a(readme.o)\
  $(d0root)/lcompack/compack.a(reasav.o)\
  $(d0root)/lcompack/compack.a(reaset.o)\
  $(d0root)/lcompack/compack.a(runcom.o)\
  $(d0root)/lcompack/compack.a(runlog.o)\
  $(d0root)/lcompack/compack.a(runset.o)\
  $(d0root)/lcompack/compack.a(setchk.o)\
  $(d0root)/lcompack/compack.a(setcom.o)\
  $(d0root)/lcompack/compack.a(setdir.o)\
  $(d0root)/lcompack/compack.a(setdmp.o)\
  $(d0root)/lcompack/compack.a(setflg.o)\
  $(d0root)/lcompack/compack.a(setmod.o)\
  $(d0root)/lcompack/compack.a(setsta.o)\
  $(d0root)/lcompack/compack.a(spawit.o)\
  $(d0root)/lcompack/compack.a(splsta.o)\
  $(d0root)/lcompack/compack.a(spltit.o)\
  $(d0root)/lcompack/compack.a(stamsg.o)\
  $(d0root)/lcompack/compack.a(strcop.o)\
  $(d0root)/lcompack/compack.a(strdcp.o)\
  $(d0root)/lcompack/compack.a(strfet.o)\
  $(d0root)/lcompack/compack.a(strsto.o)\
  $(d0root)/lcompack/compack.a(sysdsp.o)\
  $(d0root)/lcompack/compack.a(tabdis.o)\
  $(d0root)/lcompack/compack.a(tabdisc.o)\
  $(d0root)/lcompack/compack.a(tabdisn.o)\
  $(d0root)/lcompack/compack.a(tline0.o)\
  $(d0root)/lcompack/compack.a(traint.o)\
  $(d0root)/lcompack/compack.a(trarea.o)\
  $(d0root)/lcompack/compack.a(waibit.o)\
  $(d0root)/lcompack/compack.a(waitit.o)
	ar $(ARFLAGS) $(d0root)/lcompack/compack.a `ls $(SCRATCH)/compack/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/lcompack/compack.a `ls $(SCRATCH)/compack/a2/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/lcompack/compack.a `ls $(SCRATCH)/compack/a3/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/lcompack/compack.a `ls $(SCRATCH)/compack/a4/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/lcompack/compack.a `ls $(SCRATCH)/compack/a5/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/lcompack/compack.a `ls $(SCRATCH)/compack/a6/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/compack
$(SCRATCH)/compack/nothing.nl:
	rmdirp $(SCRATCH)/compack
	mkdirp $(SCRATCH)/compack/a1
	mkdirp $(SCRATCH)/compack/a2
	mkdirp $(SCRATCH)/compack/a3
	mkdirp $(SCRATCH)/compack/a4
	mkdirp $(SCRATCH)/compack/a5
	mkdirp $(SCRATCH)/compack/a6
$(d0root)/lcompack/compack.a(abomen.o):\
  $(d0root)/lcompack/source/abomen.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/abomen.f ;\
	mv abomen.o $(SCRATCH)/compack/a1/abomen.o ;\
	)
$(d0root)/lcompack/compack.a(additm.o):\
  $(d0root)/lcompack/source/additm.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/additm.f ;\
	mv additm.o $(SCRATCH)/compack/a1/additm.o ;\
	)
$(d0root)/lcompack/compack.a(canmen.o):\
  $(d0root)/lcompack/source/canmen.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/canmen.f ;\
	mv canmen.o $(SCRATCH)/compack/a1/canmen.o ;\
	)
$(d0root)/lcompack/compack.a(chcols.o):\
  $(d0root)/lcompack/source/chcols.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/chcols.f ;\
	mv chcols.o $(SCRATCH)/compack/a1/chcols.o ;\
	)
$(d0root)/lcompack/compack.a(chkist.o):\
  $(d0root)/lcompack/source/chkist.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/chkist.f ;\
	mv chkist.o $(SCRATCH)/compack/a1/chkist.o ;\
	)
$(d0root)/lcompack/compack.a(chspac.o):\
  $(d0root)/lcompack/source/chspac.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/chspac.f ;\
	mv chspac.o $(SCRATCH)/compack/a1/chspac.o ;\
	)
$(d0root)/lcompack/compack.a(conhex.o):\
  $(d0root)/lcompack/source/conhex.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/conhex.f ;\
	mv conhex.o $(SCRATCH)/compack/a1/conhex.o ;\
	)
$(d0root)/lcompack/compack.a(conint.o):\
  $(d0root)/lcompack/source/conint.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/conint.f ;\
	mv conint.o $(SCRATCH)/compack/a1/conint.o ;\
	)
$(d0root)/lcompack/compack.a(conrea.o):\
  $(d0root)/lcompack/source/conrea.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/conrea.f ;\
	mv conrea.o $(SCRATCH)/compack/a1/conrea.o ;\
	)
$(d0root)/lcompack/compack.a(contry.o):\
  $(d0root)/lcompack/source/contry.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/contry.f ;\
	mv contry.o $(SCRATCH)/compack/a1/contry.o ;\
	)
$(d0root)/lcompack/compack.a(curlin.o):\
  $(d0root)/lcompack/source/curlin.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/curlin.f ;\
	mv curlin.o $(SCRATCH)/compack/a1/curlin.o ;\
	)
$(d0root)/lcompack/compack.a(defcol.o):\
  $(d0root)/lcompack/source/defcol.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/defcol.f ;\
	mv defcol.o $(SCRATCH)/compack/a1/defcol.o ;\
	)
$(d0root)/lcompack/compack.a(defspa.o):\
  $(d0root)/lcompack/source/defspa.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/defspa.f ;\
	mv defspa.o $(SCRATCH)/compack/a1/defspa.o ;\
	)
$(d0root)/lcompack/compack.a(disabl.o):\
  $(d0root)/lcompack/source/disabl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/disabl.f ;\
	mv disabl.o $(SCRATCH)/compack/a1/disabl.o ;\
	)
$(d0root)/lcompack/compack.a(dmpmsg.o):\
  $(d0root)/lcompack/source/dmpmsg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/dmpmsg.f ;\
	mv dmpmsg.o $(SCRATCH)/compack/a1/dmpmsg.o ;\
	)
$(d0root)/lcompack/compack.a(doproc.o):\
  $(d0root)/lcompack/source/doproc.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/doproc.f ;\
	mv doproc.o $(SCRATCH)/compack/a1/doproc.o ;\
	)
$(d0root)/lcompack/compack.a(enddmp.o):\
  $(d0root)/lcompack/source/enddmp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/enddmp.f ;\
	mv enddmp.o $(SCRATCH)/compack/a1/enddmp.o ;\
	)
$(d0root)/lcompack/compack.a(endlog.o):\
  $(d0root)/lcompack/source/endlog.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/endlog.f ;\
	mv endlog.o $(SCRATCH)/compack/a1/endlog.o ;\
	)
$(d0root)/lcompack/compack.a(endspl.o):\
  $(d0root)/lcompack/source/endspl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/endspl.f ;\
	mv endspl.o $(SCRATCH)/compack/a1/endspl.o ;\
	)
$(d0root)/lcompack/compack.a(endsta.o):\
  $(d0root)/lcompack/source/endsta.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/endsta.f ;\
	mv endsta.o $(SCRATCH)/compack/a1/endsta.o ;\
	)
$(d0root)/lcompack/compack.a(evecom.o):\
  $(d0root)/lcompack/source/evecom.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/evecom.f ;\
	mv evecom.o $(SCRATCH)/compack/a2/evecom.o ;\
	)
$(d0root)/lcompack/compack.a(evesav.o):\
  $(d0root)/lcompack/source/evesav.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/evesav.f ;\
	mv evesav.o $(SCRATCH)/compack/a2/evesav.o ;\
	)
$(d0root)/lcompack/compack.a(eximen.o):\
  $(d0root)/lcompack/source/eximen.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/eximen.f ;\
	mv eximen.o $(SCRATCH)/compack/a2/eximen.o ;\
	)
$(d0root)/lcompack/compack.a(exiusr.o):\
  $(d0root)/lcompack/source/exiusr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/exiusr.f ;\
	mv exiusr.o $(SCRATCH)/compack/a2/exiusr.o ;\
	)
$(d0root)/lcompack/compack.a(fillst.o):\
  $(d0root)/lcompack/source/fillst.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/fillst.f ;\
	mv fillst.o $(SCRATCH)/compack/a2/fillst.o ;\
	)
$(d0root)/lcompack/compack.a(fndfil.o):\
  $(d0root)/lcompack/source/fndfil.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/fndfil.f ;\
	mv fndfil.o $(SCRATCH)/compack/a2/fndfil.o ;\
	)
$(d0root)/lcompack/compack.a(getarr.o):\
  $(d0root)/lcompack/source/getarr.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/getarr.f ;\
	mv getarr.o $(SCRATCH)/compack/a2/getarr.o ;\
	)
$(d0root)/lcompack/compack.a(getdis.o):\
  $(d0root)/lcompack/source/getdis.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/getdis.f ;\
	mv getdis.o $(SCRATCH)/compack/a2/getdis.o ;\
	)
$(d0root)/lcompack/compack.a(getdis77.o):\
  $(d0root)/lcompack/source/getdis77.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/getdis77.f ;\
	mv getdis77.o $(SCRATCH)/compack/a2/getdis77.o ;\
	)
$(d0root)/lcompack/compack.a(getopt.o):\
  $(d0root)/lcompack/source/getopt.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/getopt.f ;\
	mv getopt.o $(SCRATCH)/compack/a2/getopt.o ;\
	)
$(d0root)/lcompack/compack.a(getpar.o):\
  $(d0root)/lcompack/source/getpar.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/lcompack/source/commsg.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/getpar.f ;\
	mv getpar.o $(SCRATCH)/compack/a2/getpar.o ;\
	)
$(d0root)/lcompack/compack.a(getpar77.o):\
  $(d0root)/lcompack/source/getpar77.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/lcompack/source/commsg.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/getpar77.f ;\
	mv getpar77.o $(SCRATCH)/compack/a2/getpar77.o ;\
	)
$(d0root)/lcompack/compack.a(gline0.o):\
  $(d0root)/lcompack/source/gline0.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/gline0.f ;\
	mv gline0.o $(SCRATCH)/compack/a2/gline0.o ;\
	)
$(d0root)/lcompack/compack.a(gnosmg.o):\
  $(d0root)/lcompack/source/gnosmg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/gnosmg.f ;\
	mv gnosmg.o $(SCRATCH)/compack/a2/gnosmg.o ;\
	)
$(d0root)/lcompack/compack.a(gotop.o):\
  $(d0root)/lcompack/source/gotop.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/gotop.f ;\
	mv gotop.o $(SCRATCH)/compack/a2/gotop.o ;\
	)
$(d0root)/lcompack/compack.a(hlproc.o):\
  $(d0root)/lcompack/source/hlproc.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/hlproc.f ;\
	mv hlproc.o $(SCRATCH)/compack/a2/hlproc.o ;\
	)
$(d0root)/lcompack/compack.a(inpcmd.o):\
  $(d0root)/lcompack/source/inpcmd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/inpcmd.f ;\
	mv inpcmd.o $(SCRATCH)/compack/a2/inpcmd.o ;\
	)
$(d0root)/lcompack/compack.a(inptrm.o):\
  $(d0root)/lcompack/source/inptrm.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/inptrm.f ;\
	mv inptrm.o $(SCRATCH)/compack/a2/inptrm.o ;\
	)
$(d0root)/lcompack/compack.a(intast.o):\
  $(d0root)/lcompack/source/intast.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/intast.f ;\
	mv intast.o $(SCRATCH)/compack/a2/intast.o ;\
	)
$(d0root)/lcompack/compack.a(intmen.o):\
  $(d0root)/lcompack/source/intmen.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/intmen.f ;\
	mv intmen.o $(SCRATCH)/compack/a2/intmen.o ;\
	)
$(d0root)/lcompack/compack.a(intmsg.o):\
  $(d0root)/lcompack/source/intmsg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/intmsg.f ;\
	mv intmsg.o $(SCRATCH)/compack/a3/intmsg.o ;\
	)
$(d0root)/lcompack/compack.a(libera.o):\
  $(d0root)/lcompack/source/libera.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/libera.f ;\
	mv libera.o $(SCRATCH)/compack/a3/libera.o ;\
	)
$(d0root)/lcompack/compack.a(librep.o):\
  $(d0root)/lcompack/source/librep.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/librep.f ;\
	mv librep.o $(SCRATCH)/compack/a3/librep.o ;\
	)
$(d0root)/lcompack/compack.a(linchk.o):\
  $(d0root)/lcompack/source/linchk.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/linchk.f ;\
	mv linchk.o $(SCRATCH)/compack/a3/linchk.o ;\
	)
$(d0root)/lcompack/compack.a(lines0.o):\
  $(d0root)/lcompack/source/lines0.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/lines0.f ;\
	mv lines0.o $(SCRATCH)/compack/a3/lines0.o ;\
	)
$(d0root)/lcompack/compack.a(lspawn.o):\
  $(d0root)/lcompack/source/lspawn.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/lspawn.f ;\
	mv lspawn.o $(SCRATCH)/compack/a3/lspawn.o ;\
	)
$(d0root)/lcompack/compack.a(menadd.o):\
  $(d0root)/lcompack/source/menadd.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/menadd.f ;\
	mv menadd.o $(SCRATCH)/compack/a3/menadd.o ;\
	)
$(d0root)/lcompack/compack.a(menctr.o):\
  $(d0root)/lcompack/source/menctr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/menctr.f ;\
	mv menctr.o $(SCRATCH)/compack/a3/menctr.o ;\
	)
$(d0root)/lcompack/compack.a(mendef.o):\
  $(d0root)/lcompack/source/mendef.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/mendef.f ;\
	mv mendef.o $(SCRATCH)/compack/a3/mendef.o ;\
	)
$(d0root)/lcompack/compack.a(mendis.o):\
  $(d0root)/lcompack/source/mendis.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/mendis.f ;\
	mv mendis.o $(SCRATCH)/compack/a3/mendis.o ;\
	)
$(d0root)/lcompack/compack.a(menexg.o):\
  $(d0root)/lcompack/source/menexg.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/menexg.f ;\
	mv menexg.o $(SCRATCH)/compack/a3/menexg.o ;\
	)
$(d0root)/lcompack/compack.a(mennew.o):\
  $(d0root)/lcompack/source/mennew.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/mennew.f ;\
	mv mennew.o $(SCRATCH)/compack/a3/mennew.o ;\
	)
$(d0root)/lcompack/compack.a(menset.o):\
  $(d0root)/lcompack/source/menset.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/menset.f ;\
	mv menset.o $(SCRATCH)/compack/a3/menset.o ;\
	)
$(d0root)/lcompack/compack.a(menudo.o):\
  $(d0root)/lcompack/source/menudo.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/menudo.f ;\
	mv menudo.o $(SCRATCH)/compack/a3/menudo.o ;\
	)
$(d0root)/lcompack/compack.a(menuex.o):\
  $(d0root)/lcompack/source/menuex.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/menuex.f ;\
	mv menuex.o $(SCRATCH)/compack/a3/menuex.o ;\
	)
$(d0root)/lcompack/compack.a(movint.o):\
  $(d0root)/lcompack/source/movint.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/movint.f ;\
	mv movint.o $(SCRATCH)/compack/a3/movint.o ;\
	)
$(d0root)/lcompack/compack.a(movstr.o):\
  $(d0root)/lcompack/source/movstr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/movstr.f ;\
	mv movstr.o $(SCRATCH)/compack/a3/movstr.o ;\
	)
$(d0root)/lcompack/compack.a(msgscr.o):\
  $(d0root)/lcompack/source/msgscr.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/msgscr.f ;\
	mv msgscr.o $(SCRATCH)/compack/a3/msgscr.o ;\
	)
$(d0root)/lcompack/compack.a(newpar.o):\
  $(d0root)/lcompack/source/newpar.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/newpar.f ;\
	mv newpar.o $(SCRATCH)/compack/a3/newpar.o ;\
	)
$(d0root)/lcompack/compack.a(nulhdl.o):\
  $(d0root)/lcompack/source/nulhdl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/nulhdl.f ;\
	mv nulhdl.o $(SCRATCH)/compack/a3/nulhdl.o ;\
	)
$(d0root)/lcompack/compack.a(oline0.o):\
  $(d0root)/lcompack/source/oline0.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/oline0.f ;\
	mv oline0.o $(SCRATCH)/compack/a4/oline0.o ;\
	)
$(d0root)/lcompack/compack.a(outmsg.o):\
  $(d0root)/lcompack/source/outmsg.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/outmsg.f ;\
	mv outmsg.o $(SCRATCH)/compack/a4/outmsg.o ;\
	)
$(d0root)/lcompack/compack.a(outsav.o):\
  $(d0root)/lcompack/source/outsav.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/outsav.f ;\
	mv outsav.o $(SCRATCH)/compack/a4/outsav.o ;\
	)
$(d0root)/lcompack/compack.a(pareal.o):\
  $(d0root)/lcompack/source/pareal.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/pareal.f ;\
	mv pareal.o $(SCRATCH)/compack/a4/pareal.o ;\
	)
$(d0root)/lcompack/compack.a(parint.o):\
  $(d0root)/lcompack/source/parint.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/parint.f ;\
	mv parint.o $(SCRATCH)/compack/a4/parint.o ;\
	)
$(d0root)/lcompack/compack.a(parlog.o):\
  $(d0root)/lcompack/source/parlog.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/parlog.f ;\
	mv parlog.o $(SCRATCH)/compack/a4/parlog.o ;\
	)
$(d0root)/lcompack/compack.a(pfnum.o):\
  $(d0root)/lcompack/source/pfnum.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/pfnum.f ;\
	mv pfnum.o $(SCRATCH)/compack/a4/pfnum.o ;\
	)
$(d0root)/lcompack/compack.a(pfwait.o):\
  $(d0root)/lcompack/source/pfwait.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/pfwait.f ;\
	mv pfwait.o $(SCRATCH)/compack/a4/pfwait.o ;\
	)
$(d0root)/lcompack/compack.a(prscom.o):\
  $(d0root)/lcompack/source/prscom.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/prscom.f ;\
	mv prscom.o $(SCRATCH)/compack/a4/prscom.o ;\
	)
$(d0root)/lcompack/compack.a(putmsg.o):\
  $(d0root)/lcompack/source/putmsg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/putmsg.f ;\
	mv putmsg.o $(SCRATCH)/compack/a4/putmsg.o ;\
	)
$(d0root)/lcompack/compack.a(rdset.o):\
  $(d0root)/lcompack/source/rdset.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/rdset.f ;\
	mv rdset.o $(SCRATCH)/compack/a4/rdset.o ;\
	)
$(d0root)/lcompack/compack.a(readme.o):\
  $(d0root)/lcompack/source/readme.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/readme.f ;\
	mv readme.o $(SCRATCH)/compack/a4/readme.o ;\
	)
$(d0root)/lcompack/compack.a(reasav.o):\
  $(d0root)/lcompack/source/reasav.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/reasav.f ;\
	mv reasav.o $(SCRATCH)/compack/a4/reasav.o ;\
	)
$(d0root)/lcompack/compack.a(reaset.o):\
  $(d0root)/lcompack/source/reaset.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/smgdef.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/reaset.f ;\
	mv reaset.o $(SCRATCH)/compack/a4/reaset.o ;\
	)
$(d0root)/lcompack/compack.a(runcom.o):\
  $(d0root)/lcompack/source/runcom.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/runcom.f ;\
	mv runcom.o $(SCRATCH)/compack/a4/runcom.o ;\
	)
$(d0root)/lcompack/compack.a(runlog.o):\
  $(d0root)/lcompack/source/runlog.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/runlog.f ;\
	mv runlog.o $(SCRATCH)/compack/a4/runlog.o ;\
	)
$(d0root)/lcompack/compack.a(runset.o):\
  $(d0root)/lcompack/source/runset.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/runset.f ;\
	mv runset.o $(SCRATCH)/compack/a4/runset.o ;\
	)
$(d0root)/lcompack/compack.a(setchk.o):\
  $(d0root)/lcompack/source/setchk.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/ssdef.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/setchk.f ;\
	mv setchk.o $(SCRATCH)/compack/a4/setchk.o ;\
	)
$(d0root)/lcompack/compack.a(setcom.o):\
  $(d0root)/lcompack/source/setcom.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/setcom.f ;\
	mv setcom.o $(SCRATCH)/compack/a4/setcom.o ;\
	)
$(d0root)/lcompack/compack.a(setdir.o):\
  $(d0root)/lcompack/source/setdir.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/setdir.f ;\
	mv setdir.o $(SCRATCH)/compack/a4/setdir.o ;\
	)
$(d0root)/lcompack/compack.a(setdmp.o):\
  $(d0root)/lcompack/source/setdmp.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/setdmp.f ;\
	mv setdmp.o $(SCRATCH)/compack/a5/setdmp.o ;\
	)
$(d0root)/lcompack/compack.a(setflg.o):\
  $(d0root)/lcompack/source/setflg.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/setflg.f ;\
	mv setflg.o $(SCRATCH)/compack/a5/setflg.o ;\
	)
$(d0root)/lcompack/compack.a(setmod.o):\
  $(d0root)/lcompack/source/setmod.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/setmod.f ;\
	mv setmod.o $(SCRATCH)/compack/a5/setmod.o ;\
	)
$(d0root)/lcompack/compack.a(setsta.o):\
  $(d0root)/lcompack/source/setsta.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/setsta.f ;\
	mv setsta.o $(SCRATCH)/compack/a5/setsta.o ;\
	)
$(d0root)/lcompack/compack.a(spawit.o):\
  $(d0root)/lcompack/source/spawit.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/spawit.f ;\
	mv spawit.o $(SCRATCH)/compack/a5/spawit.o ;\
	)
$(d0root)/lcompack/compack.a(splsta.o):\
  $(d0root)/lcompack/source/splsta.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/splsta.f ;\
	mv splsta.o $(SCRATCH)/compack/a5/splsta.o ;\
	)
$(d0root)/lcompack/compack.a(spltit.o):\
  $(d0root)/lcompack/source/spltit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/spltit.f ;\
	mv spltit.o $(SCRATCH)/compack/a5/spltit.o ;\
	)
$(d0root)/lcompack/compack.a(stamsg.o):\
  $(d0root)/lcompack/source/stamsg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/stamsg.f ;\
	mv stamsg.o $(SCRATCH)/compack/a5/stamsg.o ;\
	)
$(d0root)/lcompack/compack.a(strcop.o):\
  $(d0root)/lcompack/source/strcop.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/strcop.f ;\
	mv strcop.o $(SCRATCH)/compack/a5/strcop.o ;\
	)
$(d0root)/lcompack/compack.a(strdcp.o):\
  $(d0root)/lcompack/source/strdcp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/strdcp.f ;\
	mv strdcp.o $(SCRATCH)/compack/a5/strdcp.o ;\
	)
$(d0root)/lcompack/compack.a(strfet.o):\
  $(d0root)/lcompack/source/strfet.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/strfet.f ;\
	mv strfet.o $(SCRATCH)/compack/a5/strfet.o ;\
	)
$(d0root)/lcompack/compack.a(strsto.o):\
  $(d0root)/lcompack/source/strsto.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/strsto.f ;\
	mv strsto.o $(SCRATCH)/compack/a5/strsto.o ;\
	)
$(d0root)/lcompack/compack.a(sysdsp.o):\
  $(d0root)/lcompack/source/sysdsp.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/sysdsp.f ;\
	mv sysdsp.o $(SCRATCH)/compack/a5/sysdsp.o ;\
	)
$(d0root)/lcompack/compack.a(tabdis.o):\
  $(d0root)/lcompack/source/tabdis.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/tabdis.f ;\
	mv tabdis.o $(SCRATCH)/compack/a5/tabdis.o ;\
	)
$(d0root)/lcompack/compack.a(tabdisc.o):\
  $(d0root)/lcompack/source/tabdisc.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/tabdisc.f ;\
	mv tabdisc.o $(SCRATCH)/compack/a5/tabdisc.o ;\
	)
$(d0root)/lcompack/compack.a(tabdisn.o):\
  $(d0root)/lcompack/source/tabdisn.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/tabdisn.f ;\
	mv tabdisn.o $(SCRATCH)/compack/a5/tabdisn.o ;\
	)
$(d0root)/lcompack/compack.a(tline0.o):\
  $(d0root)/lcompack/source/tline0.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/tline0.f ;\
	mv tline0.o $(SCRATCH)/compack/a5/tline0.o ;\
	)
$(d0root)/lcompack/compack.a(traint.o):\
  $(d0root)/lcompack/source/traint.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/traint.f ;\
	mv traint.o $(SCRATCH)/compack/a5/traint.o ;\
	)
$(d0root)/lcompack/compack.a(trarea.o):\
  $(d0root)/lcompack/source/trarea.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/trarea.f ;\
	mv trarea.o $(SCRATCH)/compack/a5/trarea.o ;\
	)
$(d0root)/lcompack/compack.a(waibit.o):\
  $(d0root)/lcompack/source/waibit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/waibit.f ;\
	mv waibit.o $(SCRATCH)/compack/a5/waibit.o ;\
	)
$(d0root)/lcompack/compack.a(waitit.o):\
  $(d0root)/lcompack/source/waitit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) lcompack/source/waitit.f ;\
	mv waitit.o $(SCRATCH)/compack/a6/waitit.o ;\
	)
pre:\
  $(d0root)/lcompack/source/abomen.f\
  $(d0root)/lcompack/source/additm.f\
  $(d0root)/lcompack/source/canmen.f\
  $(d0root)/lcompack/source/chcols.f\
  $(d0root)/lcompack/source/chkist.f\
  $(d0root)/lcompack/source/chspac.f\
  $(d0root)/lcompack/source/conhex.f\
  $(d0root)/lcompack/source/conint.f\
  $(d0root)/lcompack/source/conrea.f\
  $(d0root)/lcompack/source/contry.f\
  $(d0root)/lcompack/source/curlin.f\
  $(d0root)/lcompack/source/defcol.f\
  $(d0root)/lcompack/source/defspa.f\
  $(d0root)/lcompack/source/disabl.f\
  $(d0root)/lcompack/source/dmpmsg.f\
  $(d0root)/lcompack/source/doproc.f\
  $(d0root)/lcompack/source/enddmp.f\
  $(d0root)/lcompack/source/endlog.f\
  $(d0root)/lcompack/source/endspl.f\
  $(d0root)/lcompack/source/endsta.f\
  $(d0root)/lcompack/source/evecom.f\
  $(d0root)/lcompack/source/evesav.f\
  $(d0root)/lcompack/source/eximen.f\
  $(d0root)/lcompack/source/exiusr.f\
  $(d0root)/lcompack/source/fillst.f\
  $(d0root)/lcompack/source/fndfil.f\
  $(d0root)/lcompack/source/getarr.f\
  $(d0root)/lcompack/source/getdis.f\
  $(d0root)/lcompack/source/getdis77.f\
  $(d0root)/lcompack/source/getopt.f\
  $(d0root)/lcompack/source/getpar.f\
  $(d0root)/lcompack/source/getpar77.f\
  $(d0root)/lcompack/source/gline0.f\
  $(d0root)/lcompack/source/gnosmg.f\
  $(d0root)/lcompack/source/gotop.f\
  $(d0root)/lcompack/source/hlproc.f\
  $(d0root)/lcompack/source/inpcmd.f\
  $(d0root)/lcompack/source/inptrm.f\
  $(d0root)/lcompack/source/intast.f\
  $(d0root)/lcompack/source/intmen.f\
  $(d0root)/lcompack/source/intmsg.f\
  $(d0root)/lcompack/source/libera.f\
  $(d0root)/lcompack/source/librep.f\
  $(d0root)/lcompack/source/linchk.f\
  $(d0root)/lcompack/source/lines0.f\
  $(d0root)/lcompack/source/lspawn.f\
  $(d0root)/lcompack/source/menadd.f\
  $(d0root)/lcompack/source/menctr.f\
  $(d0root)/lcompack/source/mendef.f\
  $(d0root)/lcompack/source/mendis.f\
  $(d0root)/lcompack/source/menexg.f\
  $(d0root)/lcompack/source/mennew.f\
  $(d0root)/lcompack/source/menset.f\
  $(d0root)/lcompack/source/menudo.f\
  $(d0root)/lcompack/source/menuex.f\
  $(d0root)/lcompack/source/movint.f\
  $(d0root)/lcompack/source/movstr.f\
  $(d0root)/lcompack/source/msgscr.f\
  $(d0root)/lcompack/source/newpar.f\
  $(d0root)/lcompack/source/nulhdl.f\
  $(d0root)/lcompack/source/oline0.f\
  $(d0root)/lcompack/source/outmsg.f\
  $(d0root)/lcompack/source/outsav.f\
  $(d0root)/lcompack/source/pareal.f\
  $(d0root)/lcompack/source/parint.f\
  $(d0root)/lcompack/source/parlog.f\
  $(d0root)/lcompack/source/pfnum.f\
  $(d0root)/lcompack/source/pfwait.f\
  $(d0root)/lcompack/source/prscom.f\
  $(d0root)/lcompack/source/putmsg.f\
  $(d0root)/lcompack/source/rdset.f\
  $(d0root)/lcompack/source/readme.f\
  $(d0root)/lcompack/source/reasav.f\
  $(d0root)/lcompack/source/reaset.f\
  $(d0root)/lcompack/source/runcom.f\
  $(d0root)/lcompack/source/runlog.f\
  $(d0root)/lcompack/source/runset.f\
  $(d0root)/lcompack/source/setchk.f\
  $(d0root)/lcompack/source/setcom.f\
  $(d0root)/lcompack/source/setdir.f\
  $(d0root)/lcompack/source/setdmp.f\
  $(d0root)/lcompack/source/setflg.f\
  $(d0root)/lcompack/source/setmod.f\
  $(d0root)/lcompack/source/setsta.f\
  $(d0root)/lcompack/source/spawit.f\
  $(d0root)/lcompack/source/splsta.f\
  $(d0root)/lcompack/source/spltit.f\
  $(d0root)/lcompack/source/stamsg.f\
  $(d0root)/lcompack/source/strcop.f\
  $(d0root)/lcompack/source/strdcp.f\
  $(d0root)/lcompack/source/strfet.f\
  $(d0root)/lcompack/source/strsto.f\
  $(d0root)/lcompack/source/sysdsp.f\
  $(d0root)/lcompack/source/tabdis.f\
  $(d0root)/lcompack/source/tabdisc.f\
  $(d0root)/lcompack/source/tabdisn.f\
  $(d0root)/lcompack/source/tline0.f\
  $(d0root)/lcompack/source/traint.f\
  $(d0root)/lcompack/source/trarea.f\
  $(d0root)/lcompack/source/waibit.f\
  $(d0root)/lcompack/source/waitit.f
$(d0root)/lcompack/source/abomen.f:\
  $(d0root)/lcompack/source/abomen.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/abomen.for | vmstounix > $(d0root)/lcompack/source/abomen.f
$(d0root)/lcompack/source/additm.f:\
  $(d0root)/lcompack/source/additm.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/additm.for | vmstounix > $(d0root)/lcompack/source/additm.f
$(d0root)/lcompack/source/canmen.f:\
  $(d0root)/lcompack/source/canmen.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/canmen.for | vmstounix > $(d0root)/lcompack/source/canmen.f
$(d0root)/lcompack/source/chcols.f:\
  $(d0root)/lcompack/source/chcols.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/chcols.for | vmstounix > $(d0root)/lcompack/source/chcols.f
$(d0root)/lcompack/source/chkist.f:\
  $(d0root)/lcompack/source/chkist.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/chkist.for | vmstounix > $(d0root)/lcompack/source/chkist.f
$(d0root)/lcompack/source/chspac.f:\
  $(d0root)/lcompack/source/chspac.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/chspac.for | vmstounix > $(d0root)/lcompack/source/chspac.f
$(d0root)/lcompack/source/conhex.f:\
  $(d0root)/lcompack/source/conhex.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/conhex.for | vmstounix > $(d0root)/lcompack/source/conhex.f
$(d0root)/lcompack/source/conint.f:\
  $(d0root)/lcompack/source/conint.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/conint.for | vmstounix > $(d0root)/lcompack/source/conint.f
$(d0root)/lcompack/source/conrea.f:\
  $(d0root)/lcompack/source/conrea.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/conrea.for | vmstounix > $(d0root)/lcompack/source/conrea.f
$(d0root)/lcompack/source/contry.f:\
  $(d0root)/lcompack/source/contry.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/contry.for | vmstounix > $(d0root)/lcompack/source/contry.f
$(d0root)/lcompack/source/curlin.f:\
  $(d0root)/lcompack/source/curlin.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/curlin.for | vmstounix > $(d0root)/lcompack/source/curlin.f
$(d0root)/lcompack/source/defcol.f:\
  $(d0root)/lcompack/source/defcol.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/defcol.for | vmstounix > $(d0root)/lcompack/source/defcol.f
$(d0root)/lcompack/source/defspa.f:\
  $(d0root)/lcompack/source/defspa.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/defspa.for | vmstounix > $(d0root)/lcompack/source/defspa.f
$(d0root)/lcompack/source/disabl.f:\
  $(d0root)/lcompack/source/disabl.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/disabl.for | vmstounix > $(d0root)/lcompack/source/disabl.f
$(d0root)/lcompack/source/dmpmsg.f:\
  $(d0root)/lcompack/source/dmpmsg.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/dmpmsg.for | vmstounix > $(d0root)/lcompack/source/dmpmsg.f
$(d0root)/lcompack/source/doproc.f:\
  $(d0root)/lcompack/source/doproc.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/doproc.for | vmstounix > $(d0root)/lcompack/source/doproc.f
$(d0root)/lcompack/source/enddmp.f:\
  $(d0root)/lcompack/source/enddmp.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/enddmp.for | vmstounix > $(d0root)/lcompack/source/enddmp.f
$(d0root)/lcompack/source/endlog.f:\
  $(d0root)/lcompack/source/endlog.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/endlog.for | vmstounix > $(d0root)/lcompack/source/endlog.f
$(d0root)/lcompack/source/endspl.f:\
  $(d0root)/lcompack/source/endspl.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/endspl.for | vmstounix > $(d0root)/lcompack/source/endspl.f
$(d0root)/lcompack/source/endsta.f:\
  $(d0root)/lcompack/source/endsta.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/endsta.for | vmstounix > $(d0root)/lcompack/source/endsta.f
$(d0root)/lcompack/source/evecom.f:\
  $(d0root)/lcompack/source/evecom.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/evecom.for | vmstounix > $(d0root)/lcompack/source/evecom.f
$(d0root)/lcompack/source/evesav.f:\
  $(d0root)/lcompack/source/evesav.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/evesav.for | vmstounix > $(d0root)/lcompack/source/evesav.f
$(d0root)/lcompack/source/eximen.f:\
  $(d0root)/lcompack/source/eximen.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/eximen.for | vmstounix > $(d0root)/lcompack/source/eximen.f
$(d0root)/lcompack/source/exiusr.f:\
  $(d0root)/lcompack/source/exiusr.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/exiusr.for | vmstounix > $(d0root)/lcompack/source/exiusr.f
$(d0root)/lcompack/source/fillst.f:\
  $(d0root)/lcompack/source/fillst.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/fillst.for | vmstounix > $(d0root)/lcompack/source/fillst.f
$(d0root)/lcompack/source/fndfil.f:\
  $(d0root)/lcompack/source/fndfil.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/fndfil.for | vmstounix > $(d0root)/lcompack/source/fndfil.f
$(d0root)/lcompack/source/getarr.f:\
  $(d0root)/lcompack/source/getarr.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/getarr.for | vmstounix > $(d0root)/lcompack/source/getarr.f
$(d0root)/lcompack/source/getdis.f:\
  $(d0root)/lcompack/source/getdis.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/getdis.for | vmstounix > $(d0root)/lcompack/source/getdis.f
$(d0root)/lcompack/source/getdis77.f:\
  $(d0root)/lcompack/source/getdis77.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/getdis77.for | vmstounix > $(d0root)/lcompack/source/getdis77.f
$(d0root)/lcompack/source/getopt.f:\
  $(d0root)/lcompack/source/getopt.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/getopt.for | vmstounix > $(d0root)/lcompack/source/getopt.f
$(d0root)/lcompack/source/getpar.f:\
  $(d0root)/lcompack/source/getpar.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/lcompack/source/commsg.def
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/getpar.for | vmstounix > $(d0root)/lcompack/source/getpar.f
$(d0root)/lcompack/source/getpar77.f:\
  $(d0root)/lcompack/source/getpar77.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/lcompack/source/commsg.def
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/getpar77.for | vmstounix > $(d0root)/lcompack/source/getpar77.f
$(d0root)/lcompack/source/gline0.f:\
  $(d0root)/lcompack/source/gline0.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/gline0.for | vmstounix > $(d0root)/lcompack/source/gline0.f
$(d0root)/lcompack/source/gnosmg.f:\
  $(d0root)/lcompack/source/gnosmg.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/gnosmg.for | vmstounix > $(d0root)/lcompack/source/gnosmg.f
$(d0root)/lcompack/source/gotop.f:\
  $(d0root)/lcompack/source/gotop.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/gotop.for | vmstounix > $(d0root)/lcompack/source/gotop.f
$(d0root)/lcompack/source/hlproc.f:\
  $(d0root)/lcompack/source/hlproc.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/hlproc.for | vmstounix > $(d0root)/lcompack/source/hlproc.f
$(d0root)/lcompack/source/inpcmd.f:\
  $(d0root)/lcompack/source/inpcmd.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/inpcmd.for | vmstounix > $(d0root)/lcompack/source/inpcmd.f
$(d0root)/lcompack/source/inptrm.f:\
  $(d0root)/lcompack/source/inptrm.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/inptrm.for | vmstounix > $(d0root)/lcompack/source/inptrm.f
$(d0root)/lcompack/source/intast.f:\
  $(d0root)/lcompack/source/intast.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/intast.for | vmstounix > $(d0root)/lcompack/source/intast.f
$(d0root)/lcompack/source/intmen.f:\
  $(d0root)/lcompack/source/intmen.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/intmen.for | vmstounix > $(d0root)/lcompack/source/intmen.f
$(d0root)/lcompack/source/intmsg.f:\
  $(d0root)/lcompack/source/intmsg.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/intmsg.for | vmstounix > $(d0root)/lcompack/source/intmsg.f
$(d0root)/lcompack/source/libera.f:\
  $(d0root)/lcompack/source/libera.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/libera.for | vmstounix > $(d0root)/lcompack/source/libera.f
$(d0root)/lcompack/source/librep.f:\
  $(d0root)/lcompack/source/librep.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/librep.for | vmstounix > $(d0root)/lcompack/source/librep.f
$(d0root)/lcompack/source/linchk.f:\
  $(d0root)/lcompack/source/linchk.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/linchk.for | vmstounix > $(d0root)/lcompack/source/linchk.f
$(d0root)/lcompack/source/lines0.f:\
  $(d0root)/lcompack/source/lines0.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/lines0.for | vmstounix > $(d0root)/lcompack/source/lines0.f
$(d0root)/lcompack/source/lspawn.f:\
  $(d0root)/lcompack/source/lspawn.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/lspawn.for | vmstounix > $(d0root)/lcompack/source/lspawn.f
$(d0root)/lcompack/source/menadd.f:\
  $(d0root)/lcompack/source/menadd.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/menadd.for | vmstounix > $(d0root)/lcompack/source/menadd.f
$(d0root)/lcompack/source/menctr.f:\
  $(d0root)/lcompack/source/menctr.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/menctr.for | vmstounix > $(d0root)/lcompack/source/menctr.f
$(d0root)/lcompack/source/mendef.f:\
  $(d0root)/lcompack/source/mendef.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/mendef.for | vmstounix > $(d0root)/lcompack/source/mendef.f
$(d0root)/lcompack/source/mendis.f:\
  $(d0root)/lcompack/source/mendis.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/mendis.for | vmstounix > $(d0root)/lcompack/source/mendis.f
$(d0root)/lcompack/source/menexg.f:\
  $(d0root)/lcompack/source/menexg.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/menexg.for | vmstounix > $(d0root)/lcompack/source/menexg.f
$(d0root)/lcompack/source/mennew.f:\
  $(d0root)/lcompack/source/mennew.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/mennew.for | vmstounix > $(d0root)/lcompack/source/mennew.f
$(d0root)/lcompack/source/menset.f:\
  $(d0root)/lcompack/source/menset.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/menset.for | vmstounix > $(d0root)/lcompack/source/menset.f
$(d0root)/lcompack/source/menudo.f:\
  $(d0root)/lcompack/source/menudo.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/menudo.for | vmstounix > $(d0root)/lcompack/source/menudo.f
$(d0root)/lcompack/source/menuex.f:\
  $(d0root)/lcompack/source/menuex.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/menuex.for | vmstounix > $(d0root)/lcompack/source/menuex.f
$(d0root)/lcompack/source/movint.f:\
  $(d0root)/lcompack/source/movint.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/movint.for | vmstounix > $(d0root)/lcompack/source/movint.f
$(d0root)/lcompack/source/movstr.f:\
  $(d0root)/lcompack/source/movstr.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/movstr.for | vmstounix > $(d0root)/lcompack/source/movstr.f
$(d0root)/lcompack/source/msgscr.f:\
  $(d0root)/lcompack/source/msgscr.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/msgscr.for | vmstounix > $(d0root)/lcompack/source/msgscr.f
$(d0root)/lcompack/source/newpar.f:\
  $(d0root)/lcompack/source/newpar.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/newpar.for | vmstounix > $(d0root)/lcompack/source/newpar.f
$(d0root)/lcompack/source/nulhdl.f:\
  $(d0root)/lcompack/source/nulhdl.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/nulhdl.for | vmstounix > $(d0root)/lcompack/source/nulhdl.f
$(d0root)/lcompack/source/oline0.f:\
  $(d0root)/lcompack/source/oline0.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/oline0.for | vmstounix > $(d0root)/lcompack/source/oline0.f
$(d0root)/lcompack/source/outmsg.f:\
  $(d0root)/lcompack/source/outmsg.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/outmsg.for | vmstounix > $(d0root)/lcompack/source/outmsg.f
$(d0root)/lcompack/source/outsav.f:\
  $(d0root)/lcompack/source/outsav.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/outsav.for | vmstounix > $(d0root)/lcompack/source/outsav.f
$(d0root)/lcompack/source/pareal.f:\
  $(d0root)/lcompack/source/pareal.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/pareal.for | vmstounix > $(d0root)/lcompack/source/pareal.f
$(d0root)/lcompack/source/parint.f:\
  $(d0root)/lcompack/source/parint.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/parint.for | vmstounix > $(d0root)/lcompack/source/parint.f
$(d0root)/lcompack/source/parlog.f:\
  $(d0root)/lcompack/source/parlog.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/parlog.for | vmstounix > $(d0root)/lcompack/source/parlog.f
$(d0root)/lcompack/source/pfnum.f:\
  $(d0root)/lcompack/source/pfnum.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/pfnum.for | vmstounix > $(d0root)/lcompack/source/pfnum.f
$(d0root)/lcompack/source/pfwait.f:\
  $(d0root)/lcompack/source/pfwait.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/pfwait.for | vmstounix > $(d0root)/lcompack/source/pfwait.f
$(d0root)/lcompack/source/prscom.f:\
  $(d0root)/lcompack/source/prscom.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/prscom.for | vmstounix > $(d0root)/lcompack/source/prscom.f
$(d0root)/lcompack/source/putmsg.f:\
  $(d0root)/lcompack/source/putmsg.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/putmsg.for | vmstounix > $(d0root)/lcompack/source/putmsg.f
$(d0root)/lcompack/source/rdset.f:\
  $(d0root)/lcompack/source/rdset.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/rdset.for | vmstounix > $(d0root)/lcompack/source/rdset.f
$(d0root)/lcompack/source/readme.f:\
  $(d0root)/lcompack/source/readme.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/readme.for | vmstounix > $(d0root)/lcompack/source/readme.f
$(d0root)/lcompack/source/reasav.f:\
  $(d0root)/lcompack/source/reasav.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/reasav.for | vmstounix > $(d0root)/lcompack/source/reasav.f
$(d0root)/lcompack/source/reaset.f:\
  $(d0root)/lcompack/source/reaset.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/smgdef.def
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/reaset.for | vmstounix > $(d0root)/lcompack/source/reaset.f
$(d0root)/lcompack/source/runcom.f:\
  $(d0root)/lcompack/source/runcom.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/runcom.for | vmstounix > $(d0root)/lcompack/source/runcom.f
$(d0root)/lcompack/source/runlog.f:\
  $(d0root)/lcompack/source/runlog.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/runlog.for | vmstounix > $(d0root)/lcompack/source/runlog.f
$(d0root)/lcompack/source/runset.f:\
  $(d0root)/lcompack/source/runset.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/runset.for | vmstounix > $(d0root)/lcompack/source/runset.f
$(d0root)/lcompack/source/setchk.f:\
  $(d0root)/lcompack/source/setchk.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/ssdef.def
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/setchk.for | vmstounix > $(d0root)/lcompack/source/setchk.f
$(d0root)/lcompack/source/setcom.f:\
  $(d0root)/lcompack/source/setcom.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/setcom.for | vmstounix > $(d0root)/lcompack/source/setcom.f
$(d0root)/lcompack/source/setdir.f:\
  $(d0root)/lcompack/source/setdir.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/setdir.for | vmstounix > $(d0root)/lcompack/source/setdir.f
$(d0root)/lcompack/source/setdmp.f:\
  $(d0root)/lcompack/source/setdmp.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/setdmp.for | vmstounix > $(d0root)/lcompack/source/setdmp.f
$(d0root)/lcompack/source/setflg.f:\
  $(d0root)/lcompack/source/setflg.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/setflg.for | vmstounix > $(d0root)/lcompack/source/setflg.f
$(d0root)/lcompack/source/setmod.f:\
  $(d0root)/lcompack/source/setmod.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/setmod.for | vmstounix > $(d0root)/lcompack/source/setmod.f
$(d0root)/lcompack/source/setsta.f:\
  $(d0root)/lcompack/source/setsta.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/setsta.for | vmstounix > $(d0root)/lcompack/source/setsta.f
$(d0root)/lcompack/source/spawit.f:\
  $(d0root)/lcompack/source/spawit.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/spawit.for | vmstounix > $(d0root)/lcompack/source/spawit.f
$(d0root)/lcompack/source/splsta.f:\
  $(d0root)/lcompack/source/splsta.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/splsta.for | vmstounix > $(d0root)/lcompack/source/splsta.f
$(d0root)/lcompack/source/spltit.f:\
  $(d0root)/lcompack/source/spltit.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/spltit.for | vmstounix > $(d0root)/lcompack/source/spltit.f
$(d0root)/lcompack/source/stamsg.f:\
  $(d0root)/lcompack/source/stamsg.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/stamsg.for | vmstounix > $(d0root)/lcompack/source/stamsg.f
$(d0root)/lcompack/source/strcop.f:\
  $(d0root)/lcompack/source/strcop.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/strcop.for | vmstounix > $(d0root)/lcompack/source/strcop.f
$(d0root)/lcompack/source/strdcp.f:\
  $(d0root)/lcompack/source/strdcp.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/strdcp.for | vmstounix > $(d0root)/lcompack/source/strdcp.f
$(d0root)/lcompack/source/strfet.f:\
  $(d0root)/lcompack/source/strfet.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/strfet.for | vmstounix > $(d0root)/lcompack/source/strfet.f
$(d0root)/lcompack/source/strsto.f:\
  $(d0root)/lcompack/source/strsto.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/strsto.for | vmstounix > $(d0root)/lcompack/source/strsto.f
$(d0root)/lcompack/source/sysdsp.f:\
  $(d0root)/lcompack/source/sysdsp.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/sysdsp.for | vmstounix > $(d0root)/lcompack/source/sysdsp.f
$(d0root)/lcompack/source/tabdis.f:\
  $(d0root)/lcompack/source/tabdis.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/tabdis.for | vmstounix > $(d0root)/lcompack/source/tabdis.f
$(d0root)/lcompack/source/tabdisc.f:\
  $(d0root)/lcompack/source/tabdisc.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/tabdisc.for | vmstounix > $(d0root)/lcompack/source/tabdisc.f
$(d0root)/lcompack/source/tabdisn.f:\
  $(d0root)/lcompack/source/tabdisn.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/tabdisn.for | vmstounix > $(d0root)/lcompack/source/tabdisn.f
$(d0root)/lcompack/source/tline0.f:\
  $(d0root)/lcompack/source/tline0.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/tline0.for | vmstounix > $(d0root)/lcompack/source/tline0.f
$(d0root)/lcompack/source/traint.f:\
  $(d0root)/lcompack/source/traint.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/traint.for | vmstounix > $(d0root)/lcompack/source/traint.f
$(d0root)/lcompack/source/trarea.f:\
  $(d0root)/lcompack/source/trarea.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/trarea.for | vmstounix > $(d0root)/lcompack/source/trarea.f
$(d0root)/lcompack/source/waibit.f:\
  $(d0root)/lcompack/source/waibit.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/waibit.for | vmstounix > $(d0root)/lcompack/source/waibit.f
$(d0root)/lcompack/source/waitit.f:\
  $(d0root)/lcompack/source/waitit.for
	tasteofd0 $(FLAVOR) < $(d0root)/lcompack/source/waitit.for | vmstounix > $(d0root)/lcompack/source/waitit.f
for:
	touch $(d0root)/lcompack/source/abomen.for
	touch $(d0root)/lcompack/source/additm.for
	touch $(d0root)/lcompack/source/canmen.for
	touch $(d0root)/lcompack/source/chcols.for
	touch $(d0root)/lcompack/source/chkist.for
	touch $(d0root)/lcompack/source/chspac.for
	touch $(d0root)/lcompack/source/conhex.for
	touch $(d0root)/lcompack/source/conint.for
	touch $(d0root)/lcompack/source/conrea.for
	touch $(d0root)/lcompack/source/contry.for
	touch $(d0root)/lcompack/source/curlin.for
	touch $(d0root)/lcompack/source/defcol.for
	touch $(d0root)/lcompack/source/defspa.for
	touch $(d0root)/lcompack/source/disabl.for
	touch $(d0root)/lcompack/source/dmpmsg.for
	touch $(d0root)/lcompack/source/doproc.for
	touch $(d0root)/lcompack/source/enddmp.for
	touch $(d0root)/lcompack/source/endlog.for
	touch $(d0root)/lcompack/source/endspl.for
	touch $(d0root)/lcompack/source/endsta.for
	touch $(d0root)/lcompack/source/evecom.for
	touch $(d0root)/lcompack/source/evesav.for
	touch $(d0root)/lcompack/source/eximen.for
	touch $(d0root)/lcompack/source/exiusr.for
	touch $(d0root)/lcompack/source/fillst.for
	touch $(d0root)/lcompack/source/fndfil.for
	touch $(d0root)/lcompack/source/getarr.for
	touch $(d0root)/lcompack/source/getdis.for
	touch $(d0root)/lcompack/source/getdis77.for
	touch $(d0root)/lcompack/source/getopt.for
	touch $(d0root)/lcompack/source/getpar.for
	touch $(d0root)/lcompack/source/getpar77.for
	touch $(d0root)/lcompack/source/gline0.for
	touch $(d0root)/lcompack/source/gnosmg.for
	touch $(d0root)/lcompack/source/gotop.for
	touch $(d0root)/lcompack/source/hlproc.for
	touch $(d0root)/lcompack/source/inpcmd.for
	touch $(d0root)/lcompack/source/inptrm.for
	touch $(d0root)/lcompack/source/intast.for
	touch $(d0root)/lcompack/source/intmen.for
	touch $(d0root)/lcompack/source/intmsg.for
	touch $(d0root)/lcompack/source/libera.for
	touch $(d0root)/lcompack/source/librep.for
	touch $(d0root)/lcompack/source/linchk.for
	touch $(d0root)/lcompack/source/lines0.for
	touch $(d0root)/lcompack/source/lspawn.for
	touch $(d0root)/lcompack/source/menadd.for
	touch $(d0root)/lcompack/source/menctr.for
	touch $(d0root)/lcompack/source/mendef.for
	touch $(d0root)/lcompack/source/mendis.for
	touch $(d0root)/lcompack/source/menexg.for
	touch $(d0root)/lcompack/source/mennew.for
	touch $(d0root)/lcompack/source/menset.for
	touch $(d0root)/lcompack/source/menudo.for
	touch $(d0root)/lcompack/source/menuex.for
	touch $(d0root)/lcompack/source/movint.for
	touch $(d0root)/lcompack/source/movstr.for
	touch $(d0root)/lcompack/source/msgscr.for
	touch $(d0root)/lcompack/source/newpar.for
	touch $(d0root)/lcompack/source/nulhdl.for
	touch $(d0root)/lcompack/source/oline0.for
	touch $(d0root)/lcompack/source/outmsg.for
	touch $(d0root)/lcompack/source/outsav.for
	touch $(d0root)/lcompack/source/pareal.for
	touch $(d0root)/lcompack/source/parint.for
	touch $(d0root)/lcompack/source/parlog.for
	touch $(d0root)/lcompack/source/pfnum.for
	touch $(d0root)/lcompack/source/pfwait.for
	touch $(d0root)/lcompack/source/prscom.for
	touch $(d0root)/lcompack/source/putmsg.for
	touch $(d0root)/lcompack/source/rdset.for
	touch $(d0root)/lcompack/source/readme.for
	touch $(d0root)/lcompack/source/reasav.for
	touch $(d0root)/lcompack/source/reaset.for
	touch $(d0root)/lcompack/source/runcom.for
	touch $(d0root)/lcompack/source/runlog.for
	touch $(d0root)/lcompack/source/runset.for
	touch $(d0root)/lcompack/source/setchk.for
	touch $(d0root)/lcompack/source/setcom.for
	touch $(d0root)/lcompack/source/setdir.for
	touch $(d0root)/lcompack/source/setdmp.for
	touch $(d0root)/lcompack/source/setflg.for
	touch $(d0root)/lcompack/source/setmod.for
	touch $(d0root)/lcompack/source/setsta.for
	touch $(d0root)/lcompack/source/spawit.for
	touch $(d0root)/lcompack/source/splsta.for
	touch $(d0root)/lcompack/source/spltit.for
	touch $(d0root)/lcompack/source/stamsg.for
	touch $(d0root)/lcompack/source/strcop.for
	touch $(d0root)/lcompack/source/strdcp.for
	touch $(d0root)/lcompack/source/strfet.for
	touch $(d0root)/lcompack/source/strsto.for
	touch $(d0root)/lcompack/source/sysdsp.for
	touch $(d0root)/lcompack/source/tabdis.for
	touch $(d0root)/lcompack/source/tabdisc.for
	touch $(d0root)/lcompack/source/tabdisn.for
	touch $(d0root)/lcompack/source/tline0.for
	touch $(d0root)/lcompack/source/traint.for
	touch $(d0root)/lcompack/source/trarea.for
	touch $(d0root)/lcompack/source/waibit.for
	touch $(d0root)/lcompack/source/waitit.for
