SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Olimit 2500 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/admin/tmp/d0librar/mmstomake/compack
FLAVOR = SIUNIX
F77 = f77
.IGNORE:
.SUFFIXES:

opt :\
  compack.a
compack.a : $(d0root)/compack/compack.a
	@ echo compack.a is up to date
$(d0root)/compack/compack.a:: $(SCRATCH)/compack/nothing.nl
$(d0root)/compack/compack.a::\
  $(d0root)/compack/compack.a(abomen.o)\
  $(d0root)/compack/compack.a(additm.o)\
  $(d0root)/compack/compack.a(askhst.o)\
  $(d0root)/compack/compack.a(askins.o)\
  $(d0root)/compack/compack.a(asklst.o)\
  $(d0root)/compack/compack.a(asknum.o)\
  $(d0root)/compack/compack.a(askrls.o)\
  $(d0root)/compack/compack.a(askrng.o)\
  $(d0root)/compack/compack.a(askval.o)\
  $(d0root)/compack/compack.a(askyno.o)\
  $(d0root)/compack/compack.a(begbat.o)\
  $(d0root)/compack/compack.a(broadc.o)\
  $(d0root)/compack/compack.a(broast.o)\
  $(d0root)/compack/compack.a(canmen.o)\
  $(d0root)/compack/compack.a(chcols.o)\
  $(d0root)/compack/compack.a(chkcom.o)\
  $(d0root)/compack/compack.a(chkist.o)\
  $(d0root)/compack/compack.a(chspac.o)\
  $(d0root)/compack/compack.a(cmpk_gconfi.o)\
  $(d0root)/compack/compack.a(confrm.o)\
  $(d0root)/compack/compack.a(conhex.o)\
  $(d0root)/compack/compack.a(conint.o)\
  $(d0root)/compack/compack.a(conrea.o)\
  $(d0root)/compack/compack.a(contry.o)\
  $(d0root)/compack/compack.a(ctrast.o)\
  $(d0root)/compack/compack.a(curlin.o)\
  $(d0root)/compack/compack.a(curonf.o)\
  $(d0root)/compack/compack.a(curso1.o)\
  $(d0root)/compack/compack.a(cursor.o)\
  $(d0root)/compack/compack.a(curtim.o)\
  $(d0root)/compack/compack.a(defcol.o)\
  $(d0root)/compack/compack.a(defspa.o)\
  $(d0root)/compack/compack.a(diabox.o)\
  $(d0root)/compack/compack.a(disabl.o)\
  $(d0root)/compack/compack.a(displtems409.o)\
  $(d0root)/compack/compack.a(displines402.o)\
  $(d0root)/compack/compack.a(dmpmsg.o)\
  $(d0root)/compack/compack.a(doproc.o)\
  $(d0root)/compack/compack.a(drgrid.o)\
  $(d0root)/compack/compack.a(drrect.o)\
  $(d0root)/compack/compack.a(dumhdl.o)\
  $(d0root)/compack/compack.a(endclr.o)\
  $(d0root)/compack/compack.a(enddmp.o)\
  $(d0root)/compack/compack.a(endlog.o)\
  $(d0root)/compack/compack.a(endspl.o)\
  $(d0root)/compack/compack.a(endsta.o)\
  $(d0root)/compack/compack.a(erase.o)\
  $(d0root)/compack/compack.a(evecom.o)\
  $(d0root)/compack/compack.a(evefil.o)\
  $(d0root)/compack/compack.a(evesav.o)\
  $(d0root)/compack/compack.a(eximen.o)\
  $(d0root)/compack/compack.a(exiusr.o)\
  $(d0root)/compack/compack.a(fillst.o)\
  $(d0root)/compack/compack.a(fndfil.o)\
  $(d0root)/compack/compack.a(fulmod.o)\
  $(d0root)/compack/compack.a(gcomnd.o)\
  $(d0root)/compack/compack.a(gconfi.o)\
  $(d0root)/compack/compack.a(getarr.o)\
  $(d0root)/compack/compack.a(getdev.o)\
  $(d0root)/compack/compack.a(getdis.o)\
  $(d0root)/compack/compack.a(getdis77.o)\
  $(d0root)/compack/compack.a(gethlp.o)\
  $(d0root)/compack/compack.a(getopt.o)\
  $(d0root)/compack/compack.a(getpar.o)\
  $(d0root)/compack/compack.a(getpar77.o)\
  $(d0root)/compack/compack.a(getscr.o)\
  $(d0root)/compack/compack.a(getspl.o)\
  $(d0root)/compack/compack.a(getsta.o)\
  $(d0root)/compack/compack.a(getstr.o)\
  $(d0root)/compack/compack.a(getsyi.o)\
  $(d0root)/compack/compack.a(getwta.o)\
  $(d0root)/compack/compack.a(gfulsc.o)\
  $(d0root)/compack/compack.a(gincom.o)\
  $(d0root)/compack/compack.a(gjourn.o)\
  $(d0root)/compack/compack.a(gline0.o)\
  $(d0root)/compack/compack.a(gline1.o)\
  $(d0root)/compack/compack.a(gline2.o)\
  $(d0root)/compack/compack.a(glines.o)\
  $(d0root)/compack/compack.a(gnosmg.o)\
  $(d0root)/compack/compack.a(gotop.o)\
  $(d0root)/compack/compack.a(gpar_inpt827.o)\
  $(d0root)/compack/compack.a(greads.o)\
  $(d0root)/compack/compack.a(gsplit.o)\
  $(d0root)/compack/compack.a(gstatu.o)\
  $(d0root)/compack/compack.a(guserm.o)\
  $(d0root)/compack/compack.a(helpme.o)\
  $(d0root)/compack/compack.a(hlpget.o)\
  $(d0root)/compack/compack.a(hlproc.o)\
  $(d0root)/compack/compack.a(inpcmd.o)\
  $(d0root)/compack/compack.a(inptrm.o)\
  $(d0root)/compack/compack.a(intast.o)\
  $(d0root)/compack/compack.a(intmen.o)\
  $(d0root)/compack/compack.a(intmsg.o)\
  $(d0root)/compack/compack.a(keyast.o)\
  $(d0root)/compack/compack.a(keydef.o)\
  $(d0root)/compack/compack.a(keyptr.o)\
  $(d0root)/compack/compack.a(lenint.o)\
  $(d0root)/compack/compack.a(libbig.o)\
  $(d0root)/compack/compack.a(libbof.o)\
  $(d0root)/compack/compack.a(libbon.o)\
  $(d0root)/compack/compack.a(libcar.o)\
  $(d0root)/compack/compack.a(libcr1.o)\
  $(d0root)/compack/compack.a(libctm.o)\
  $(d0root)/compack/compack.a(libcur.o)\
  $(d0root)/compack/compack.a(libefc.o)\
  $(d0root)/compack/compack.a(libemv.o)\
  $(d0root)/compack/compack.a(libera.o)\
  $(d0root)/compack/compack.a(liberl.o)\
  $(d0root)/compack/compack.a(liberp.o)\
  $(d0root)/compack/compack.a(libget.o)\
  $(d0root)/compack/compack.a(libhmv.o)\
  $(d0root)/compack/compack.a(libind.o)\
  $(d0root)/compack/compack.a(libkey.o)\
  $(d0root)/compack/compack.a(liblin.o)\
  $(d0root)/compack/compack.a(libnxt.o)\
  $(d0root)/compack/compack.a(libpmv.o)\
  $(d0root)/compack/compack.a(libpst.o)\
  $(d0root)/compack/compack.a(libput.o)\
  $(d0root)/compack/compack.a(librep.o)\
  $(d0root)/compack/compack.a(libres.o)\
  $(d0root)/compack/compack.a(libsav.o)\
  $(d0root)/compack/compack.a(libscr.o)\
  $(d0root)/compack/compack.a(libsna.o)\
  $(d0root)/compack/compack.a(libsta.o)\
  $(d0root)/compack/compack.a(libstm.o)\
  $(d0root)/compack/compack.a(libunp.o)\
  $(d0root)/compack/compack.a(libuns.o)\
  $(d0root)/compack/compack.a(linchk.o)\
  $(d0root)/compack/compack.a(lines.o)\
  $(d0root)/compack/compack.a(lines0.o)\
  $(d0root)/compack/compack.a(lines1.o)\
  $(d0root)/compack/compack.a(linget.o)\
  $(d0root)/compack/compack.a(lisbox.o)\
  $(d0root)/compack/compack.a(lspawn.o)\
  $(d0root)/compack/compack.a(mclref.o)\
  $(d0root)/compack/compack.a(menadd.o)\
  $(d0root)/compack/compack.a(menctr.o)\
  $(d0root)/compack/compack.a(mendef.o)\
  $(d0root)/compack/compack.a(mendis.o)\
  $(d0root)/compack/compack.a(menexg.o)\
  $(d0root)/compack/compack.a(menmai.o)\
  $(d0root)/compack/compack.a(menmod.o)\
  $(d0root)/compack/compack.a(mennew.o)\
  $(d0root)/compack/compack.a(menset.o)\
  $(d0root)/compack/compack.a(mensub.o)\
  $(d0root)/compack/compack.a(menudo.o)\
  $(d0root)/compack/compack.a(menuef.o)\
  $(d0root)/compack/compack.a(menuex.o)\
  $(d0root)/compack/compack.a(menuop.o)\
  $(d0root)/compack/compack.a(modiframs397.o)\
  $(d0root)/compack/compack.a(movint.o)\
  $(d0root)/compack/compack.a(movprt.o)\
  $(d0root)/compack/compack.a(movstr.o)\
  $(d0root)/compack/compack.a(msetef.o)\
  $(d0root)/compack/compack.a(msgscr.o)\
  $(d0root)/compack/compack.a(mtimer.o)\
  $(d0root)/compack/compack.a(newpar.o)\
  $(d0root)/compack/compack.a(normal.o)\
  $(d0root)/compack/compack.a(nulhdl.o)\
  $(d0root)/compack/compack.a(oline0.o)\
  $(d0root)/compack/compack.a(oline1.o)\
  $(d0root)/compack/compack.a(olines.o)\
  $(d0root)/compack/compack.a(ordset.o)\
  $(d0root)/compack/compack.a(oreads.o)\
  $(d0root)/compack/compack.a(outmsg.o)\
  $(d0root)/compack/compack.a(outsav.o)\
  $(d0root)/compack/compack.a(page.o)\
  $(d0root)/compack/compack.a(pareal.o)\
  $(d0root)/compack/compack.a(parint.o)\
  $(d0root)/compack/compack.a(parlog.o)\
  $(d0root)/compack/compack.a(parstr.o)\
  $(d0root)/compack/compack.a(pfget.o)\
  $(d0root)/compack/compack.a(pflabl.o)\
  $(d0root)/compack/compack.a(pfnum.o)\
  $(d0root)/compack/compack.a(pfwait.o)\
  $(d0root)/compack/compack.a(prscom.o)\
  $(d0root)/compack/compack.a(prtpbd.o)\
  $(d0root)/compack/compack.a(putmsg.o)\
  $(d0root)/compack/compack.a(qioast.o)\
  $(d0root)/compack/compack.a(rdlog.o)\
  $(d0root)/compack/compack.a(rdset.o)\
  $(d0root)/compack/compack.a(readme.o)\
  $(d0root)/compack/compack.a(readpf.o)\
  $(d0root)/compack/compack.a(reasav.o)\
  $(d0root)/compack/compack.a(reaset.o)\
  $(d0root)/compack/compack.a(relmsg.o)\
  $(d0root)/compack/compack.a(repscr.o)\
  $(d0root)/compack/compack.a(restat.o)\
  $(d0root)/compack/compack.a(runaux.o)\
  $(d0root)/compack/compack.a(runcom.o)\
  $(d0root)/compack/compack.a(runlog.o)\
  $(d0root)/compack/compack.a(runset.o)\
  $(d0root)/compack/compack.a(savscr.o)\
  $(d0root)/compack/compack.a(scrolines.o)\
  $(d0root)/compack/compack.a(set_sreen728.o)\
  $(d0root)/compack/compack.a(setchk.o)\
  $(d0root)/compack/compack.a(setcom.o)\
  $(d0root)/compack/compack.a(setdir.o)\
  $(d0root)/compack/compack.a(setdmp.o)\
  $(d0root)/compack/compack.a(setflg.o)\
  $(d0root)/compack/compack.a(setlin.o)\
  $(d0root)/compack/compack.a(setmod.o)\
  $(d0root)/compack/compack.a(setqio.o)\
  $(d0root)/compack/compack.a(setsta.o)\
  $(d0root)/compack/compack.a(spawit.o)\
  $(d0root)/compack/compack.a(splsta.o)\
  $(d0root)/compack/compack.a(spltit.o)\
  $(d0root)/compack/compack.a(stadis.o)\
  $(d0root)/compack/compack.a(stamsg.o)\
  $(d0root)/compack/compack.a(strap1.o)\
  $(d0root)/compack/compack.a(strapp.o)\
  $(d0root)/compack/compack.a(strcop.o)\
  $(d0root)/compack/compack.a(strdcp.o)\
  $(d0root)/compack/compack.a(strfet.o)\
  $(d0root)/compack/compack.a(strfre.o)\
  $(d0root)/compack/compack.a(strsto.o)\
  $(d0root)/compack/compack.a(sysdsp.o)\
  $(d0root)/compack/compack.a(tabdis.o)\
  $(d0root)/compack/compack.a(tabdisc.o)\
  $(d0root)/compack/compack.a(tabdisn.o)\
  $(d0root)/compack/compack.a(timast.o)\
  $(d0root)/compack/compack.a(timdis.o)\
  $(d0root)/compack/compack.a(tline0.o)\
  $(d0root)/compack/compack.a(tline1.o)\
  $(d0root)/compack/compack.a(tlines.o)\
  $(d0root)/compack/compack.a(traint.o)\
  $(d0root)/compack/compack.a(tralog.o)\
  $(d0root)/compack/compack.a(trarea.o)\
  $(d0root)/compack/compack.a(typage.o)\
  $(d0root)/compack/compack.a(usrchk.o)\
  $(d0root)/compack/compack.a(waibit.o)\
  $(d0root)/compack/compack.a(waitit.o)\
  $(d0root)/compack/compack.a(windis.o)
	ar $(ARFLAGS) $(d0root)/compack/compack.a `ls $(SCRATCH)/compack/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/compack/compack.a `ls $(SCRATCH)/compack/a2/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/compack/compack.a `ls $(SCRATCH)/compack/a3/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/compack/compack.a `ls $(SCRATCH)/compack/a4/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/compack/compack.a `ls $(SCRATCH)/compack/a5/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/compack/compack.a `ls $(SCRATCH)/compack/a6/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/compack/compack.a `ls $(SCRATCH)/compack/a7/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/compack/compack.a `ls $(SCRATCH)/compack/a8/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/compack/compack.a `ls $(SCRATCH)/compack/a9/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/compack/compack.a `ls $(SCRATCH)/compack/a10/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/compack/compack.a `ls $(SCRATCH)/compack/a11/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/compack/compack.a `ls $(SCRATCH)/compack/a12/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/compack
$(SCRATCH)/compack/nothing.nl:
	rmdirp $(SCRATCH)/compack
	mkdirp $(SCRATCH)/compack/a1
	mkdirp $(SCRATCH)/compack/a2
	mkdirp $(SCRATCH)/compack/a3
	mkdirp $(SCRATCH)/compack/a4
	mkdirp $(SCRATCH)/compack/a5
	mkdirp $(SCRATCH)/compack/a6
	mkdirp $(SCRATCH)/compack/a7
	mkdirp $(SCRATCH)/compack/a8
	mkdirp $(SCRATCH)/compack/a9
	mkdirp $(SCRATCH)/compack/a10
	mkdirp $(SCRATCH)/compack/a11
	mkdirp $(SCRATCH)/compack/a12
$(d0root)/compack/compack.a(abomen.o):\
  $(d0root)/compack/source/abomen.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/abomen.f ;\
	mv abomen.o $(SCRATCH)/compack/a1/abomen.o ;\
	)
$(d0root)/compack/compack.a(additm.o):\
  $(d0root)/compack/source/additm.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/additm.f ;\
	mv additm.o $(SCRATCH)/compack/a1/additm.o ;\
	)
$(d0root)/compack/compack.a(askhst.o):\
  $(d0root)/compack/source/askhst.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/askhst.f ;\
	mv askhst.o $(SCRATCH)/compack/a1/askhst.o ;\
	)
$(d0root)/compack/compack.a(askins.o):\
  $(d0root)/compack/source/askins.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/askins.f ;\
	mv askins.o $(SCRATCH)/compack/a1/askins.o ;\
	)
$(d0root)/compack/compack.a(asklst.o):\
  $(d0root)/compack/source/asklst.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/asklst.f ;\
	mv asklst.o $(SCRATCH)/compack/a1/asklst.o ;\
	)
$(d0root)/compack/compack.a(asknum.o):\
  $(d0root)/compack/source/asknum.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/asknum.f ;\
	mv asknum.o $(SCRATCH)/compack/a1/asknum.o ;\
	)
$(d0root)/compack/compack.a(askrls.o):\
  $(d0root)/compack/source/askrls.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/askrls.f ;\
	mv askrls.o $(SCRATCH)/compack/a1/askrls.o ;\
	)
$(d0root)/compack/compack.a(askrng.o):\
  $(d0root)/compack/source/askrng.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/askrng.f ;\
	mv askrng.o $(SCRATCH)/compack/a1/askrng.o ;\
	)
$(d0root)/compack/compack.a(askval.o):\
  $(d0root)/compack/source/askval.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/askval.f ;\
	mv askval.o $(SCRATCH)/compack/a1/askval.o ;\
	)
$(d0root)/compack/compack.a(askyno.o):\
  $(d0root)/compack/source/askyno.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/askyno.f ;\
	mv askyno.o $(SCRATCH)/compack/a1/askyno.o ;\
	)
$(d0root)/compack/compack.a(begbat.o):\
  $(d0root)/compack/source/begbat.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/begbat.f ;\
	mv begbat.o $(SCRATCH)/compack/a1/begbat.o ;\
	)
$(d0root)/compack/compack.a(broadc.o):\
  $(d0root)/compack/source/broadc.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/broadc.f ;\
	mv broadc.o $(SCRATCH)/compack/a1/broadc.o ;\
	)
$(d0root)/compack/compack.a(broast.o):\
  $(d0root)/compack/source/broast.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/broast.f ;\
	mv broast.o $(SCRATCH)/compack/a1/broast.o ;\
	)
$(d0root)/compack/compack.a(canmen.o):\
  $(d0root)/compack/source/canmen.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/canmen.f ;\
	mv canmen.o $(SCRATCH)/compack/a1/canmen.o ;\
	)
$(d0root)/compack/compack.a(chcols.o):\
  $(d0root)/compack/source/chcols.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/chcols.f ;\
	mv chcols.o $(SCRATCH)/compack/a1/chcols.o ;\
	)
$(d0root)/compack/compack.a(chkcom.o):\
  $(d0root)/compack/source/chkcom.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgdef.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/chkcom.f ;\
	mv chkcom.o $(SCRATCH)/compack/a1/chkcom.o ;\
	)
$(d0root)/compack/compack.a(chkist.o):\
  $(d0root)/compack/source/chkist.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/chkist.f ;\
	mv chkist.o $(SCRATCH)/compack/a1/chkist.o ;\
	)
$(d0root)/compack/compack.a(chspac.o):\
  $(d0root)/compack/source/chspac.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/chspac.f ;\
	mv chspac.o $(SCRATCH)/compack/a1/chspac.o ;\
	)
$(d0root)/compack/compack.a(cmpk_gconfi.o):\
  $(d0root)/compack/source/cmpk_gconfi.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/cmpk_gconfi.f ;\
	mv cmpk_gconfi.o $(SCRATCH)/compack/a1/cmpk_gconfi.o ;\
	)
$(d0root)/compack/compack.a(confrm.o):\
  $(d0root)/compack/source/confrm.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/keycom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/confrm.f ;\
	mv confrm.o $(SCRATCH)/compack/a1/confrm.o ;\
	)
$(d0root)/compack/compack.a(conhex.o):\
  $(d0root)/compack/source/conhex.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/conhex.f ;\
	mv conhex.o $(SCRATCH)/compack/a2/conhex.o ;\
	)
$(d0root)/compack/compack.a(conint.o):\
  $(d0root)/compack/source/conint.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/conint.f ;\
	mv conint.o $(SCRATCH)/compack/a2/conint.o ;\
	)
$(d0root)/compack/compack.a(conrea.o):\
  $(d0root)/compack/source/conrea.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/conrea.f ;\
	mv conrea.o $(SCRATCH)/compack/a2/conrea.o ;\
	)
$(d0root)/compack/compack.a(contry.o):\
  $(d0root)/compack/source/contry.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/contry.f ;\
	mv contry.o $(SCRATCH)/compack/a2/contry.o ;\
	)
$(d0root)/compack/compack.a(ctrast.o):\
  $(d0root)/compack/source/ctrast.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/ctrast.f ;\
	mv ctrast.o $(SCRATCH)/compack/a2/ctrast.o ;\
	)
$(d0root)/compack/compack.a(curlin.o):\
  $(d0root)/compack/source/curlin.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/curlin.f ;\
	mv curlin.o $(SCRATCH)/compack/a2/curlin.o ;\
	)
$(d0root)/compack/compack.a(curonf.o):\
  $(d0root)/compack/source/curonf.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/curonf.f ;\
	mv curonf.o $(SCRATCH)/compack/a2/curonf.o ;\
	)
$(d0root)/compack/compack.a(curso1.o):\
  $(d0root)/compack/source/curso1.f\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/smgdef.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/curso1.f ;\
	mv curso1.o $(SCRATCH)/compack/a2/curso1.o ;\
	)
$(d0root)/compack/compack.a(cursor.o):\
  $(d0root)/compack/source/cursor.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/cursor.f ;\
	mv cursor.o $(SCRATCH)/compack/a2/cursor.o ;\
	)
$(d0root)/compack/compack.a(curtim.o):\
  $(d0root)/compack/source/curtim.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/curtim.f ;\
	mv curtim.o $(SCRATCH)/compack/a2/curtim.o ;\
	)
$(d0root)/compack/compack.a(defcol.o):\
  $(d0root)/compack/source/defcol.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/defcol.f ;\
	mv defcol.o $(SCRATCH)/compack/a2/defcol.o ;\
	)
$(d0root)/compack/compack.a(defspa.o):\
  $(d0root)/compack/source/defspa.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/defspa.f ;\
	mv defspa.o $(SCRATCH)/compack/a2/defspa.o ;\
	)
$(d0root)/compack/compack.a(diabox.o):\
  $(d0root)/compack/source/diabox.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/keycom.inc\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/trmdef.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/diabox.f ;\
	mv diabox.o $(SCRATCH)/compack/a2/diabox.o ;\
	)
$(d0root)/compack/compack.a(disabl.o):\
  $(d0root)/compack/source/disabl.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/disabl.f ;\
	mv disabl.o $(SCRATCH)/compack/a2/disabl.o ;\
	)
$(d0root)/compack/compack.a(displtems409.o):\
  $(d0root)/compack/source/display_items.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/display_items.f ;\
	mv display_items.o $(SCRATCH)/compack/a2/displtems409.o ;\
	)
$(d0root)/compack/compack.a(displines402.o):\
  $(d0root)/compack/source/display_lines.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/display_lines.f ;\
	mv display_lines.o $(SCRATCH)/compack/a2/displines402.o ;\
	)
$(d0root)/compack/compack.a(dmpmsg.o):\
  $(d0root)/compack/source/dmpmsg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/dmpmsg.f ;\
	mv dmpmsg.o $(SCRATCH)/compack/a2/dmpmsg.o ;\
	)
$(d0root)/compack/compack.a(doproc.o):\
  $(d0root)/compack/source/doproc.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/doproc.f ;\
	mv doproc.o $(SCRATCH)/compack/a2/doproc.o ;\
	)
$(d0root)/compack/compack.a(drgrid.o):\
  $(d0root)/compack/source/drgrid.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/drgrid.f ;\
	mv drgrid.o $(SCRATCH)/compack/a2/drgrid.o ;\
	)
$(d0root)/compack/compack.a(drrect.o):\
  $(d0root)/compack/source/drrect.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/drrect.f ;\
	mv drrect.o $(SCRATCH)/compack/a2/drrect.o ;\
	)
$(d0root)/compack/compack.a(dumhdl.o):\
  $(d0root)/compack/source/dumhdl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/dumhdl.f ;\
	mv dumhdl.o $(SCRATCH)/compack/a3/dumhdl.o ;\
	)
$(d0root)/compack/compack.a(endclr.o):\
  $(d0root)/compack/source/endclr.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/endclr.f ;\
	mv endclr.o $(SCRATCH)/compack/a3/endclr.o ;\
	)
$(d0root)/compack/compack.a(enddmp.o):\
  $(d0root)/compack/source/enddmp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/enddmp.f ;\
	mv enddmp.o $(SCRATCH)/compack/a3/enddmp.o ;\
	)
$(d0root)/compack/compack.a(endlog.o):\
  $(d0root)/compack/source/endlog.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/endlog.f ;\
	mv endlog.o $(SCRATCH)/compack/a3/endlog.o ;\
	)
$(d0root)/compack/compack.a(endspl.o):\
  $(d0root)/compack/source/endspl.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/endspl.f ;\
	mv endspl.o $(SCRATCH)/compack/a3/endspl.o ;\
	)
$(d0root)/compack/compack.a(endsta.o):\
  $(d0root)/compack/source/endsta.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/endsta.f ;\
	mv endsta.o $(SCRATCH)/compack/a3/endsta.o ;\
	)
$(d0root)/compack/compack.a(erase.o):\
  $(d0root)/compack/source/erase.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/erase.f ;\
	mv erase.o $(SCRATCH)/compack/a3/erase.o ;\
	)
$(d0root)/compack/compack.a(evecom.o):\
  $(d0root)/compack/source/evecom.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/evecom.f ;\
	mv evecom.o $(SCRATCH)/compack/a3/evecom.o ;\
	)
$(d0root)/compack/compack.a(evefil.o):\
  $(d0root)/compack/source/evefil.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/evefil.f ;\
	mv evefil.o $(SCRATCH)/compack/a3/evefil.o ;\
	)
$(d0root)/compack/compack.a(evesav.o):\
  $(d0root)/compack/source/evesav.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/evesav.f ;\
	mv evesav.o $(SCRATCH)/compack/a3/evesav.o ;\
	)
$(d0root)/compack/compack.a(eximen.o):\
  $(d0root)/compack/source/eximen.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/eximen.f ;\
	mv eximen.o $(SCRATCH)/compack/a3/eximen.o ;\
	)
$(d0root)/compack/compack.a(exiusr.o):\
  $(d0root)/compack/source/exiusr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/exiusr.f ;\
	mv exiusr.o $(SCRATCH)/compack/a3/exiusr.o ;\
	)
$(d0root)/compack/compack.a(fillst.o):\
  $(d0root)/compack/source/fillst.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/fillst.f ;\
	mv fillst.o $(SCRATCH)/compack/a3/fillst.o ;\
	)
$(d0root)/compack/compack.a(fndfil.o):\
  $(d0root)/compack/source/fndfil.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/fndfil.f ;\
	mv fndfil.o $(SCRATCH)/compack/a3/fndfil.o ;\
	)
$(d0root)/compack/compack.a(fulmod.o):\
  $(d0root)/compack/source/fulmod.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/fulmod.f ;\
	mv fulmod.o $(SCRATCH)/compack/a3/fulmod.o ;\
	)
$(d0root)/compack/compack.a(gcomnd.o):\
  $(d0root)/compack/source/gcomnd.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/gcomnd.f ;\
	mv gcomnd.o $(SCRATCH)/compack/a3/gcomnd.o ;\
	)
$(d0root)/compack/compack.a(gconfi.o):\
  $(d0root)/compack/source/gconfi.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/gconfi.f ;\
	mv gconfi.o $(SCRATCH)/compack/a3/gconfi.o ;\
	)
$(d0root)/compack/compack.a(getarr.o):\
  $(d0root)/compack/source/getarr.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/getarr.f ;\
	mv getarr.o $(SCRATCH)/compack/a3/getarr.o ;\
	)
$(d0root)/compack/compack.a(getdev.o):\
  $(d0root)/compack/source/getdev.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/getdev.f ;\
	mv getdev.o $(SCRATCH)/compack/a3/getdev.o ;\
	)
$(d0root)/compack/compack.a(getdis.o):\
  $(d0root)/compack/source/getdis.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/getdis.f ;\
	mv getdis.o $(SCRATCH)/compack/a3/getdis.o ;\
	)
$(d0root)/compack/compack.a(getdis77.o):\
  $(d0root)/compack/source/getdis77.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/getdis77.f ;\
	mv getdis77.o $(SCRATCH)/compack/a4/getdis77.o ;\
	)
$(d0root)/compack/compack.a(gethlp.o):\
  $(d0root)/compack/source/gethlp.f\
  $(d0library)/inc/smgdef.def\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/gethlp.f ;\
	mv gethlp.o $(SCRATCH)/compack/a4/gethlp.o ;\
	)
$(d0root)/compack/compack.a(getopt.o):\
  $(d0root)/compack/source/getopt.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/getopt.f ;\
	mv getopt.o $(SCRATCH)/compack/a4/getopt.o ;\
	)
$(d0root)/compack/compack.a(getpar.o):\
  $(d0root)/compack/source/getpar.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/getpar.f ;\
	mv getpar.o $(SCRATCH)/compack/a4/getpar.o ;\
	)
$(d0root)/compack/compack.a(getpar77.o):\
  $(d0root)/compack/source/getpar77.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/getpar77.f ;\
	mv getpar77.o $(SCRATCH)/compack/a4/getpar77.o ;\
	)
$(d0root)/compack/compack.a(getscr.o):\
  $(d0root)/compack/source/getscr.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/getscr.f ;\
	mv getscr.o $(SCRATCH)/compack/a4/getscr.o ;\
	)
$(d0root)/compack/compack.a(getspl.o):\
  $(d0root)/compack/source/getspl.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/getspl.f ;\
	mv getspl.o $(SCRATCH)/compack/a4/getspl.o ;\
	)
$(d0root)/compack/compack.a(getsta.o):\
  $(d0root)/compack/source/getsta.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/getsta.f ;\
	mv getsta.o $(SCRATCH)/compack/a4/getsta.o ;\
	)
$(d0root)/compack/compack.a(getstr.o):\
  $(d0root)/compack/source/getstr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/getstr.f ;\
	mv getstr.o $(SCRATCH)/compack/a4/getstr.o ;\
	)
$(d0root)/compack/compack.a(getsyi.o):\
  $(d0root)/compack/source/getsyi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/getsyi.f ;\
	mv getsyi.o $(SCRATCH)/compack/a4/getsyi.o ;\
	)
$(d0root)/compack/compack.a(getwta.o):\
  $(d0root)/compack/source/getwta.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/getwta.f ;\
	mv getwta.o $(SCRATCH)/compack/a4/getwta.o ;\
	)
$(d0root)/compack/compack.a(gfulsc.o):\
  $(d0root)/compack/source/gfulsc.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/gfulsc.f ;\
	mv gfulsc.o $(SCRATCH)/compack/a4/gfulsc.o ;\
	)
$(d0root)/compack/compack.a(gincom.o):\
  $(d0root)/compack/source/gincom.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/gincom.f ;\
	mv gincom.o $(SCRATCH)/compack/a4/gincom.o ;\
	)
$(d0root)/compack/compack.a(gjourn.o):\
  $(d0root)/compack/source/gjourn.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/gjourn.f ;\
	mv gjourn.o $(SCRATCH)/compack/a4/gjourn.o ;\
	)
$(d0root)/compack/compack.a(gline0.o):\
  $(d0root)/compack/source/gline0.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/gline0.f ;\
	mv gline0.o $(SCRATCH)/compack/a4/gline0.o ;\
	)
$(d0root)/compack/compack.a(gline1.o):\
  $(d0root)/compack/source/gline1.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/gline1.f ;\
	mv gline1.o $(SCRATCH)/compack/a4/gline1.o ;\
	)
$(d0root)/compack/compack.a(gline2.o):\
  $(d0root)/compack/source/gline2.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/gline2.f ;\
	mv gline2.o $(SCRATCH)/compack/a4/gline2.o ;\
	)
$(d0root)/compack/compack.a(glines.o):\
  $(d0root)/compack/source/glines.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/glines.f ;\
	mv glines.o $(SCRATCH)/compack/a4/glines.o ;\
	)
$(d0root)/compack/compack.a(gnosmg.o):\
  $(d0root)/compack/source/gnosmg.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/gnosmg.f ;\
	mv gnosmg.o $(SCRATCH)/compack/a4/gnosmg.o ;\
	)
$(d0root)/compack/compack.a(gotop.o):\
  $(d0root)/compack/source/gotop.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/gotop.f ;\
	mv gotop.o $(SCRATCH)/compack/a4/gotop.o ;\
	)
$(d0root)/compack/compack.a(gpar_inpt827.o):\
  $(d0root)/compack/source/gpar_lindspl_inpt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/gpar_lindspl_inpt.f ;\
	mv gpar_lindspl_inpt.o $(SCRATCH)/compack/a5/gpar_inpt827.o ;\
	)
$(d0root)/compack/compack.a(greads.o):\
  $(d0root)/compack/source/greads.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/trmdef.def\
  $(d0library)/inc/smgdef.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/greads.f ;\
	mv greads.o $(SCRATCH)/compack/a5/greads.o ;\
	)
$(d0root)/compack/compack.a(gsplit.o):\
  $(d0root)/compack/source/gsplit.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/gsplit.f ;\
	mv gsplit.o $(SCRATCH)/compack/a5/gsplit.o ;\
	)
$(d0root)/compack/compack.a(gstatu.o):\
  $(d0root)/compack/source/gstatu.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/gstatu.f ;\
	mv gstatu.o $(SCRATCH)/compack/a5/gstatu.o ;\
	)
$(d0root)/compack/compack.a(guserm.o):\
  $(d0root)/compack/source/guserm.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/guserm.f ;\
	mv guserm.o $(SCRATCH)/compack/a5/guserm.o ;\
	)
$(d0root)/compack/compack.a(helpme.o):\
  $(d0root)/compack/source/helpme.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/helpme.f ;\
	mv helpme.o $(SCRATCH)/compack/a5/helpme.o ;\
	)
$(d0root)/compack/compack.a(hlpget.o):\
  $(d0root)/compack/source/hlpget.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/hlpget.f ;\
	mv hlpget.o $(SCRATCH)/compack/a5/hlpget.o ;\
	)
$(d0root)/compack/compack.a(hlproc.o):\
  $(d0root)/compack/source/hlproc.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/hlproc.f ;\
	mv hlproc.o $(SCRATCH)/compack/a5/hlproc.o ;\
	)
$(d0root)/compack/compack.a(inpcmd.o):\
  $(d0root)/compack/source/inpcmd.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/inpcmd.f ;\
	mv inpcmd.o $(SCRATCH)/compack/a5/inpcmd.o ;\
	)
$(d0root)/compack/compack.a(inptrm.o):\
  $(d0root)/compack/source/inptrm.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/inptrm.f ;\
	mv inptrm.o $(SCRATCH)/compack/a5/inptrm.o ;\
	)
$(d0root)/compack/compack.a(intast.o):\
  $(d0root)/compack/source/intast.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/intast.f ;\
	mv intast.o $(SCRATCH)/compack/a5/intast.o ;\
	)
$(d0root)/compack/compack.a(intmen.o):\
  $(d0root)/compack/source/intmen.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/intmen.f ;\
	mv intmen.o $(SCRATCH)/compack/a5/intmen.o ;\
	)
$(d0root)/compack/compack.a(intmsg.o):\
  $(d0root)/compack/source/intmsg.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/intmsg.f ;\
	mv intmsg.o $(SCRATCH)/compack/a5/intmsg.o ;\
	)
$(d0root)/compack/compack.a(keyast.o):\
  $(d0root)/compack/source/keyast.f\
  $(d0library)/inc/keycom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/keyast.f ;\
	mv keyast.o $(SCRATCH)/compack/a5/keyast.o ;\
	)
$(d0root)/compack/compack.a(keydef.o):\
  $(d0root)/compack/source/keydef.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/trmdef.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/keydef.f ;\
	mv keydef.o $(SCRATCH)/compack/a5/keydef.o ;\
	)
$(d0root)/compack/compack.a(keyptr.o):\
  $(d0root)/compack/source/keyptr.f\
  $(d0library)/inc/keycom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/keyptr.f ;\
	mv keyptr.o $(SCRATCH)/compack/a5/keyptr.o ;\
	)
$(d0root)/compack/compack.a(lenint.o):\
  $(d0root)/compack/source/lenint.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/lenint.f ;\
	mv lenint.o $(SCRATCH)/compack/a5/lenint.o ;\
	)
$(d0root)/compack/compack.a(libbig.o):\
  $(d0root)/compack/source/libbig.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libbig.f ;\
	mv libbig.o $(SCRATCH)/compack/a5/libbig.o ;\
	)
$(d0root)/compack/compack.a(libbof.o):\
  $(d0root)/compack/source/libbof.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libbof.f ;\
	mv libbof.o $(SCRATCH)/compack/a5/libbof.o ;\
	)
$(d0root)/compack/compack.a(libbon.o):\
  $(d0root)/compack/source/libbon.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libbon.f ;\
	mv libbon.o $(SCRATCH)/compack/a5/libbon.o ;\
	)
$(d0root)/compack/compack.a(libcar.o):\
  $(d0root)/compack/source/libcar.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libcar.f ;\
	mv libcar.o $(SCRATCH)/compack/a6/libcar.o ;\
	)
$(d0root)/compack/compack.a(libcr1.o):\
  $(d0root)/compack/source/libcr1.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libcr1.f ;\
	mv libcr1.o $(SCRATCH)/compack/a6/libcr1.o ;\
	)
$(d0root)/compack/compack.a(libctm.o):\
  $(d0root)/compack/source/libctm.f\
  $(d0library)/inc/keycom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libctm.f ;\
	mv libctm.o $(SCRATCH)/compack/a6/libctm.o ;\
	)
$(d0root)/compack/compack.a(libcur.o):\
  $(d0root)/compack/source/libcur.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libcur.f ;\
	mv libcur.o $(SCRATCH)/compack/a6/libcur.o ;\
	)
$(d0root)/compack/compack.a(libefc.o):\
  $(d0root)/compack/source/libefc.f\
  $(d0library)/inc/keycom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libefc.f ;\
	mv libefc.o $(SCRATCH)/compack/a6/libefc.o ;\
	)
$(d0root)/compack/compack.a(libemv.o):\
  $(d0root)/compack/source/libemv.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libemv.f ;\
	mv libemv.o $(SCRATCH)/compack/a6/libemv.o ;\
	)
$(d0root)/compack/compack.a(libera.o):\
  $(d0root)/compack/source/libera.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libera.f ;\
	mv libera.o $(SCRATCH)/compack/a6/libera.o ;\
	)
$(d0root)/compack/compack.a(liberl.o):\
  $(d0root)/compack/source/liberl.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/liberl.f ;\
	mv liberl.o $(SCRATCH)/compack/a6/liberl.o ;\
	)
$(d0root)/compack/compack.a(liberp.o):\
  $(d0root)/compack/source/liberp.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/liberp.f ;\
	mv liberp.o $(SCRATCH)/compack/a6/liberp.o ;\
	)
$(d0root)/compack/compack.a(libget.o):\
  $(d0root)/compack/source/libget.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libget.f ;\
	mv libget.o $(SCRATCH)/compack/a6/libget.o ;\
	)
$(d0root)/compack/compack.a(libhmv.o):\
  $(d0root)/compack/source/libhmv.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libhmv.f ;\
	mv libhmv.o $(SCRATCH)/compack/a6/libhmv.o ;\
	)
$(d0root)/compack/compack.a(libind.o):\
  $(d0root)/compack/source/libind.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libind.f ;\
	mv libind.o $(SCRATCH)/compack/a6/libind.o ;\
	)
$(d0root)/compack/compack.a(libkey.o):\
  $(d0root)/compack/source/libkey.f\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/keycom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libkey.f ;\
	mv libkey.o $(SCRATCH)/compack/a6/libkey.o ;\
	)
$(d0root)/compack/compack.a(liblin.o):\
  $(d0root)/compack/source/liblin.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/liblin.f ;\
	mv liblin.o $(SCRATCH)/compack/a6/liblin.o ;\
	)
$(d0root)/compack/compack.a(libnxt.o):\
  $(d0root)/compack/source/libnxt.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libnxt.f ;\
	mv libnxt.o $(SCRATCH)/compack/a6/libnxt.o ;\
	)
$(d0root)/compack/compack.a(libpmv.o):\
  $(d0root)/compack/source/libpmv.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libpmv.f ;\
	mv libpmv.o $(SCRATCH)/compack/a6/libpmv.o ;\
	)
$(d0root)/compack/compack.a(libpst.o):\
  $(d0root)/compack/source/libpst.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libpst.f ;\
	mv libpst.o $(SCRATCH)/compack/a6/libpst.o ;\
	)
$(d0root)/compack/compack.a(libput.o):\
  $(d0root)/compack/source/libput.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libput.f ;\
	mv libput.o $(SCRATCH)/compack/a6/libput.o ;\
	)
$(d0root)/compack/compack.a(librep.o):\
  $(d0root)/compack/source/librep.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/librep.f ;\
	mv librep.o $(SCRATCH)/compack/a6/librep.o ;\
	)
$(d0root)/compack/compack.a(libres.o):\
  $(d0root)/compack/source/libres.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libres.f ;\
	mv libres.o $(SCRATCH)/compack/a6/libres.o ;\
	)
$(d0root)/compack/compack.a(libsav.o):\
  $(d0root)/compack/source/libsav.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libsav.f ;\
	mv libsav.o $(SCRATCH)/compack/a7/libsav.o ;\
	)
$(d0root)/compack/compack.a(libscr.o):\
  $(d0root)/compack/source/libscr.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libscr.f ;\
	mv libscr.o $(SCRATCH)/compack/a7/libscr.o ;\
	)
$(d0root)/compack/compack.a(libsna.o):\
  $(d0root)/compack/source/libsna.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libsna.f ;\
	mv libsna.o $(SCRATCH)/compack/a7/libsna.o ;\
	)
$(d0root)/compack/compack.a(libsta.o):\
  $(d0root)/compack/source/libsta.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libsta.f ;\
	mv libsta.o $(SCRATCH)/compack/a7/libsta.o ;\
	)
$(d0root)/compack/compack.a(libstm.o):\
  $(d0root)/compack/source/libstm.f\
  $(d0library)/inc/keycom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libstm.f ;\
	mv libstm.o $(SCRATCH)/compack/a7/libstm.o ;\
	)
$(d0root)/compack/compack.a(libunp.o):\
  $(d0root)/compack/source/libunp.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libunp.f ;\
	mv libunp.o $(SCRATCH)/compack/a7/libunp.o ;\
	)
$(d0root)/compack/compack.a(libuns.o):\
  $(d0root)/compack/source/libuns.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/libuns.f ;\
	mv libuns.o $(SCRATCH)/compack/a7/libuns.o ;\
	)
$(d0root)/compack/compack.a(linchk.o):\
  $(d0root)/compack/source/linchk.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/linchk.f ;\
	mv linchk.o $(SCRATCH)/compack/a7/linchk.o ;\
	)
$(d0root)/compack/compack.a(lines.o):\
  $(d0root)/compack/source/lines.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/lines.f ;\
	mv lines.o $(SCRATCH)/compack/a7/lines.o ;\
	)
$(d0root)/compack/compack.a(lines0.o):\
  $(d0root)/compack/source/lines0.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/lines0.f ;\
	mv lines0.o $(SCRATCH)/compack/a7/lines0.o ;\
	)
$(d0root)/compack/compack.a(lines1.o):\
  $(d0root)/compack/source/lines1.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/lines1.f ;\
	mv lines1.o $(SCRATCH)/compack/a7/lines1.o ;\
	)
$(d0root)/compack/compack.a(linget.o):\
  $(d0root)/compack/source/linget.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/linget.f ;\
	mv linget.o $(SCRATCH)/compack/a7/linget.o ;\
	)
$(d0root)/compack/compack.a(lisbox.o):\
  $(d0root)/compack/source/lisbox.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/keycom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/lisbox.f ;\
	mv lisbox.o $(SCRATCH)/compack/a7/lisbox.o ;\
	)
$(d0root)/compack/compack.a(lspawn.o):\
  $(d0root)/compack/source/lspawn.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/lspawn.f ;\
	mv lspawn.o $(SCRATCH)/compack/a7/lspawn.o ;\
	)
$(d0root)/compack/compack.a(mclref.o):\
  $(d0root)/compack/source/mclref.f\
  $(d0library)/inc/keycom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/mclref.f ;\
	mv mclref.o $(SCRATCH)/compack/a7/mclref.o ;\
	)
$(d0root)/compack/compack.a(menadd.o):\
  $(d0root)/compack/source/menadd.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/menadd.f ;\
	mv menadd.o $(SCRATCH)/compack/a7/menadd.o ;\
	)
$(d0root)/compack/compack.a(menctr.o):\
  $(d0root)/compack/source/menctr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/menctr.f ;\
	mv menctr.o $(SCRATCH)/compack/a7/menctr.o ;\
	)
$(d0root)/compack/compack.a(mendef.o):\
  $(d0root)/compack/source/mendef.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/mendef.f ;\
	mv mendef.o $(SCRATCH)/compack/a7/mendef.o ;\
	)
$(d0root)/compack/compack.a(mendis.o):\
  $(d0root)/compack/source/mendis.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/mendis.f ;\
	mv mendis.o $(SCRATCH)/compack/a7/mendis.o ;\
	)
$(d0root)/compack/compack.a(menexg.o):\
  $(d0root)/compack/source/menexg.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/menexg.f ;\
	mv menexg.o $(SCRATCH)/compack/a7/menexg.o ;\
	)
$(d0root)/compack/compack.a(menmai.o):\
  $(d0root)/compack/source/menmai.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/menmai.f ;\
	mv menmai.o $(SCRATCH)/compack/a8/menmai.o ;\
	)
$(d0root)/compack/compack.a(menmod.o):\
  $(d0root)/compack/source/menmod.f\
  $(d0library)/inc/keycom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/menmod.f ;\
	mv menmod.o $(SCRATCH)/compack/a8/menmod.o ;\
	)
$(d0root)/compack/compack.a(mennew.o):\
  $(d0root)/compack/source/mennew.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/mennew.f ;\
	mv mennew.o $(SCRATCH)/compack/a8/mennew.o ;\
	)
$(d0root)/compack/compack.a(menset.o):\
  $(d0root)/compack/source/menset.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/menset.f ;\
	mv menset.o $(SCRATCH)/compack/a8/menset.o ;\
	)
$(d0root)/compack/compack.a(mensub.o):\
  $(d0root)/compack/source/mensub.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/mensub.f ;\
	mv mensub.o $(SCRATCH)/compack/a8/mensub.o ;\
	)
$(d0root)/compack/compack.a(menudo.o):\
  $(d0root)/compack/source/menudo.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/menudo.f ;\
	mv menudo.o $(SCRATCH)/compack/a8/menudo.o ;\
	)
$(d0root)/compack/compack.a(menuef.o):\
  $(d0root)/compack/source/menuef.f\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/keycom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/menuef.f ;\
	mv menuef.o $(SCRATCH)/compack/a8/menuef.o ;\
	)
$(d0root)/compack/compack.a(menuex.o):\
  $(d0root)/compack/source/menuex.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/menuex.f ;\
	mv menuex.o $(SCRATCH)/compack/a8/menuex.o ;\
	)
$(d0root)/compack/compack.a(menuop.o):\
  $(d0root)/compack/source/menuop.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/menuop.f ;\
	mv menuop.o $(SCRATCH)/compack/a8/menuop.o ;\
	)
$(d0root)/compack/compack.a(modiframs397.o):\
  $(d0root)/compack/source/modify_params.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/modify_params.f ;\
	mv modify_params.o $(SCRATCH)/compack/a8/modiframs397.o ;\
	)
$(d0root)/compack/compack.a(movint.o):\
  $(d0root)/compack/source/movint.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/movint.f ;\
	mv movint.o $(SCRATCH)/compack/a8/movint.o ;\
	)
$(d0root)/compack/compack.a(movprt.o):\
  $(d0root)/compack/source/movprt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/movprt.f ;\
	mv movprt.o $(SCRATCH)/compack/a8/movprt.o ;\
	)
$(d0root)/compack/compack.a(movstr.o):\
  $(d0root)/compack/source/movstr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/movstr.f ;\
	mv movstr.o $(SCRATCH)/compack/a8/movstr.o ;\
	)
$(d0root)/compack/compack.a(msetef.o):\
  $(d0root)/compack/source/msetef.f\
  $(d0library)/inc/keycom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/msetef.f ;\
	mv msetef.o $(SCRATCH)/compack/a8/msetef.o ;\
	)
$(d0root)/compack/compack.a(msgscr.o):\
  $(d0root)/compack/source/msgscr.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/msgscr.f ;\
	mv msgscr.o $(SCRATCH)/compack/a8/msgscr.o ;\
	)
$(d0root)/compack/compack.a(mtimer.o):\
  $(d0root)/compack/source/mtimer.f\
  $(d0library)/inc/keycom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/mtimer.f ;\
	mv mtimer.o $(SCRATCH)/compack/a8/mtimer.o ;\
	)
$(d0root)/compack/compack.a(newpar.o):\
  $(d0root)/compack/source/newpar.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/newpar.f ;\
	mv newpar.o $(SCRATCH)/compack/a8/newpar.o ;\
	)
$(d0root)/compack/compack.a(normal.o):\
  $(d0root)/compack/source/normal.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/normal.f ;\
	mv normal.o $(SCRATCH)/compack/a8/normal.o ;\
	)
$(d0root)/compack/compack.a(nulhdl.o):\
  $(d0root)/compack/source/nulhdl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/nulhdl.f ;\
	mv nulhdl.o $(SCRATCH)/compack/a8/nulhdl.o ;\
	)
$(d0root)/compack/compack.a(oline0.o):\
  $(d0root)/compack/source/oline0.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/oline0.f ;\
	mv oline0.o $(SCRATCH)/compack/a8/oline0.o ;\
	)
$(d0root)/compack/compack.a(oline1.o):\
  $(d0root)/compack/source/oline1.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/oline1.f ;\
	mv oline1.o $(SCRATCH)/compack/a9/oline1.o ;\
	)
$(d0root)/compack/compack.a(olines.o):\
  $(d0root)/compack/source/olines.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/olines.f ;\
	mv olines.o $(SCRATCH)/compack/a9/olines.o ;\
	)
$(d0root)/compack/compack.a(ordset.o):\
  $(d0root)/compack/source/ordset.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/ordset.f ;\
	mv ordset.o $(SCRATCH)/compack/a9/ordset.o ;\
	)
$(d0root)/compack/compack.a(oreads.o):\
  $(d0root)/compack/source/oreads.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/oreads.f ;\
	mv oreads.o $(SCRATCH)/compack/a9/oreads.o ;\
	)
$(d0root)/compack/compack.a(outmsg.o):\
  $(d0root)/compack/source/outmsg.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/outmsg.f ;\
	mv outmsg.o $(SCRATCH)/compack/a9/outmsg.o ;\
	)
$(d0root)/compack/compack.a(outsav.o):\
  $(d0root)/compack/source/outsav.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/outsav.f ;\
	mv outsav.o $(SCRATCH)/compack/a9/outsav.o ;\
	)
$(d0root)/compack/compack.a(page.o):\
  $(d0root)/compack/source/page.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/page.f ;\
	mv page.o $(SCRATCH)/compack/a9/page.o ;\
	)
$(d0root)/compack/compack.a(pareal.o):\
  $(d0root)/compack/source/pareal.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/pareal.f ;\
	mv pareal.o $(SCRATCH)/compack/a9/pareal.o ;\
	)
$(d0root)/compack/compack.a(parint.o):\
  $(d0root)/compack/source/parint.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/parint.f ;\
	mv parint.o $(SCRATCH)/compack/a9/parint.o ;\
	)
$(d0root)/compack/compack.a(parlog.o):\
  $(d0root)/compack/source/parlog.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/parlog.f ;\
	mv parlog.o $(SCRATCH)/compack/a9/parlog.o ;\
	)
$(d0root)/compack/compack.a(parstr.o):\
  $(d0root)/compack/source/parstr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/parstr.f ;\
	mv parstr.o $(SCRATCH)/compack/a9/parstr.o ;\
	)
$(d0root)/compack/compack.a(pfget.o):\
  $(d0root)/compack/source/pfget.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/pfget.f ;\
	mv pfget.o $(SCRATCH)/compack/a9/pfget.o ;\
	)
$(d0root)/compack/compack.a(pflabl.o):\
  $(d0root)/compack/source/pflabl.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/pflabl.f ;\
	mv pflabl.o $(SCRATCH)/compack/a9/pflabl.o ;\
	)
$(d0root)/compack/compack.a(pfnum.o):\
  $(d0root)/compack/source/pfnum.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/pfnum.f ;\
	mv pfnum.o $(SCRATCH)/compack/a9/pfnum.o ;\
	)
$(d0root)/compack/compack.a(pfwait.o):\
  $(d0root)/compack/source/pfwait.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/pfwait.f ;\
	mv pfwait.o $(SCRATCH)/compack/a9/pfwait.o ;\
	)
$(d0root)/compack/compack.a(prscom.o):\
  $(d0root)/compack/source/prscom.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/prscom.f ;\
	mv prscom.o $(SCRATCH)/compack/a9/prscom.o ;\
	)
$(d0root)/compack/compack.a(prtpbd.o):\
  $(d0root)/compack/source/prtpbd.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/prtpbd.f ;\
	mv prtpbd.o $(SCRATCH)/compack/a9/prtpbd.o ;\
	)
$(d0root)/compack/compack.a(putmsg.o):\
  $(d0root)/compack/source/putmsg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/putmsg.f ;\
	mv putmsg.o $(SCRATCH)/compack/a9/putmsg.o ;\
	)
$(d0root)/compack/compack.a(qioast.o):\
  $(d0root)/compack/source/qioast.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/qioast.f ;\
	mv qioast.o $(SCRATCH)/compack/a9/qioast.o ;\
	)
$(d0root)/compack/compack.a(rdlog.o):\
  $(d0root)/compack/source/rdlog.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/rdlog.f ;\
	mv rdlog.o $(SCRATCH)/compack/a9/rdlog.o ;\
	)
$(d0root)/compack/compack.a(rdset.o):\
  $(d0root)/compack/source/rdset.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/rdset.f ;\
	mv rdset.o $(SCRATCH)/compack/a10/rdset.o ;\
	)
$(d0root)/compack/compack.a(readme.o):\
  $(d0root)/compack/source/readme.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/trmdef.def\
  $(d0library)/inc/smgdef.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/readme.f ;\
	mv readme.o $(SCRATCH)/compack/a10/readme.o ;\
	)
$(d0root)/compack/compack.a(readpf.o):\
  $(d0root)/compack/source/readpf.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/readpf.f ;\
	mv readpf.o $(SCRATCH)/compack/a10/readpf.o ;\
	)
$(d0root)/compack/compack.a(reasav.o):\
  $(d0root)/compack/source/reasav.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/reasav.f ;\
	mv reasav.o $(SCRATCH)/compack/a10/reasav.o ;\
	)
$(d0root)/compack/compack.a(reaset.o):\
  $(d0root)/compack/source/reaset.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/smgdef.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/reaset.f ;\
	mv reaset.o $(SCRATCH)/compack/a10/reaset.o ;\
	)
$(d0root)/compack/compack.a(relmsg.o):\
  $(d0root)/compack/source/relmsg.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/relmsg.f ;\
	mv relmsg.o $(SCRATCH)/compack/a10/relmsg.o ;\
	)
$(d0root)/compack/compack.a(repscr.o):\
  $(d0root)/compack/source/repscr.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/repscr.f ;\
	mv repscr.o $(SCRATCH)/compack/a10/repscr.o ;\
	)
$(d0root)/compack/compack.a(restat.o):\
  $(d0root)/compack/source/restat.f\
  $(d0library)/inc/timsta.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/restat.f ;\
	mv restat.o $(SCRATCH)/compack/a10/restat.o ;\
	)
$(d0root)/compack/compack.a(runaux.o):\
  $(d0root)/compack/source/runaux.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/runaux.f ;\
	mv runaux.o $(SCRATCH)/compack/a10/runaux.o ;\
	)
$(d0root)/compack/compack.a(runcom.o):\
  $(d0root)/compack/source/runcom.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/runcom.f ;\
	mv runcom.o $(SCRATCH)/compack/a10/runcom.o ;\
	)
$(d0root)/compack/compack.a(runlog.o):\
  $(d0root)/compack/source/runlog.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/runlog.f ;\
	mv runlog.o $(SCRATCH)/compack/a10/runlog.o ;\
	)
$(d0root)/compack/compack.a(runset.o):\
  $(d0root)/compack/source/runset.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/runset.f ;\
	mv runset.o $(SCRATCH)/compack/a10/runset.o ;\
	)
$(d0root)/compack/compack.a(savscr.o):\
  $(d0root)/compack/source/savscr.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/savscr.f ;\
	mv savscr.o $(SCRATCH)/compack/a10/savscr.o ;\
	)
$(d0root)/compack/compack.a(scrolines.o):\
  $(d0root)/compack/source/scrolines.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/scrolines.f ;\
	mv scrolines.o $(SCRATCH)/compack/a10/scrolines.o ;\
	)
$(d0root)/compack/compack.a(set_sreen728.o):\
  $(d0root)/compack/source/set_split_screen.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/set_split_screen.f ;\
	mv set_split_screen.o $(SCRATCH)/compack/a10/set_sreen728.o ;\
	)
$(d0root)/compack/compack.a(setchk.o):\
  $(d0root)/compack/source/setchk.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/ssdef.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/setchk.f ;\
	mv setchk.o $(SCRATCH)/compack/a10/setchk.o ;\
	)
$(d0root)/compack/compack.a(setcom.o):\
  $(d0root)/compack/source/setcom.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/setcom.f ;\
	mv setcom.o $(SCRATCH)/compack/a10/setcom.o ;\
	)
$(d0root)/compack/compack.a(setdir.o):\
  $(d0root)/compack/source/setdir.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/setdir.f ;\
	mv setdir.o $(SCRATCH)/compack/a10/setdir.o ;\
	)
$(d0root)/compack/compack.a(setdmp.o):\
  $(d0root)/compack/source/setdmp.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/setdmp.f ;\
	mv setdmp.o $(SCRATCH)/compack/a10/setdmp.o ;\
	)
$(d0root)/compack/compack.a(setflg.o):\
  $(d0root)/compack/source/setflg.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/setflg.f ;\
	mv setflg.o $(SCRATCH)/compack/a10/setflg.o ;\
	)
$(d0root)/compack/compack.a(setlin.o):\
  $(d0root)/compack/source/setlin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/setlin.f ;\
	mv setlin.o $(SCRATCH)/compack/a11/setlin.o ;\
	)
$(d0root)/compack/compack.a(setmod.o):\
  $(d0root)/compack/source/setmod.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/setmod.f ;\
	mv setmod.o $(SCRATCH)/compack/a11/setmod.o ;\
	)
$(d0root)/compack/compack.a(setqio.o):\
  $(d0root)/compack/source/setqio.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/setqio.f ;\
	mv setqio.o $(SCRATCH)/compack/a11/setqio.o ;\
	)
$(d0root)/compack/compack.a(setsta.o):\
  $(d0root)/compack/source/setsta.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/setsta.f ;\
	mv setsta.o $(SCRATCH)/compack/a11/setsta.o ;\
	)
$(d0root)/compack/compack.a(spawit.o):\
  $(d0root)/compack/source/spawit.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/spawit.f ;\
	mv spawit.o $(SCRATCH)/compack/a11/spawit.o ;\
	)
$(d0root)/compack/compack.a(splsta.o):\
  $(d0root)/compack/source/splsta.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/splsta.f ;\
	mv splsta.o $(SCRATCH)/compack/a11/splsta.o ;\
	)
$(d0root)/compack/compack.a(spltit.o):\
  $(d0root)/compack/source/spltit.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/spltit.f ;\
	mv spltit.o $(SCRATCH)/compack/a11/spltit.o ;\
	)
$(d0root)/compack/compack.a(stadis.o):\
  $(d0root)/compack/source/stadis.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/stadis.f ;\
	mv stadis.o $(SCRATCH)/compack/a11/stadis.o ;\
	)
$(d0root)/compack/compack.a(stamsg.o):\
  $(d0root)/compack/source/stamsg.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/stamsg.f ;\
	mv stamsg.o $(SCRATCH)/compack/a11/stamsg.o ;\
	)
$(d0root)/compack/compack.a(strap1.o):\
  $(d0root)/compack/source/strap1.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/strap1.f ;\
	mv strap1.o $(SCRATCH)/compack/a11/strap1.o ;\
	)
$(d0root)/compack/compack.a(strapp.o):\
  $(d0root)/compack/source/strapp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/strapp.f ;\
	mv strapp.o $(SCRATCH)/compack/a11/strapp.o ;\
	)
$(d0root)/compack/compack.a(strcop.o):\
  $(d0root)/compack/source/strcop.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/strcop.f ;\
	mv strcop.o $(SCRATCH)/compack/a11/strcop.o ;\
	)
$(d0root)/compack/compack.a(strdcp.o):\
  $(d0root)/compack/source/strdcp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/strdcp.f ;\
	mv strdcp.o $(SCRATCH)/compack/a11/strdcp.o ;\
	)
$(d0root)/compack/compack.a(strfet.o):\
  $(d0root)/compack/source/strfet.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/strfet.f ;\
	mv strfet.o $(SCRATCH)/compack/a11/strfet.o ;\
	)
$(d0root)/compack/compack.a(strfre.o):\
  $(d0root)/compack/source/strfre.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/strfre.f ;\
	mv strfre.o $(SCRATCH)/compack/a11/strfre.o ;\
	)
$(d0root)/compack/compack.a(strsto.o):\
  $(d0root)/compack/source/strsto.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/strsto.f ;\
	mv strsto.o $(SCRATCH)/compack/a11/strsto.o ;\
	)
$(d0root)/compack/compack.a(sysdsp.o):\
  $(d0root)/compack/source/sysdsp.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/sysdsp.f ;\
	mv sysdsp.o $(SCRATCH)/compack/a11/sysdsp.o ;\
	)
$(d0root)/compack/compack.a(tabdis.o):\
  $(d0root)/compack/source/tabdis.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/tabdis.f ;\
	mv tabdis.o $(SCRATCH)/compack/a11/tabdis.o ;\
	)
$(d0root)/compack/compack.a(tabdisc.o):\
  $(d0root)/compack/source/tabdisc.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/tabdisc.f ;\
	mv tabdisc.o $(SCRATCH)/compack/a11/tabdisc.o ;\
	)
$(d0root)/compack/compack.a(tabdisn.o):\
  $(d0root)/compack/source/tabdisn.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/tabdisn.f ;\
	mv tabdisn.o $(SCRATCH)/compack/a11/tabdisn.o ;\
	)
$(d0root)/compack/compack.a(timast.o):\
  $(d0root)/compack/source/timast.f\
  $(d0library)/inc/keycom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/timast.f ;\
	mv timast.o $(SCRATCH)/compack/a12/timast.o ;\
	)
$(d0root)/compack/compack.a(timdis.o):\
  $(d0root)/compack/source/timdis.f\
  $(d0library)/inc/timsta.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/timdis.f ;\
	mv timdis.o $(SCRATCH)/compack/a12/timdis.o ;\
	)
$(d0root)/compack/compack.a(tline0.o):\
  $(d0root)/compack/source/tline0.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/tline0.f ;\
	mv tline0.o $(SCRATCH)/compack/a12/tline0.o ;\
	)
$(d0root)/compack/compack.a(tline1.o):\
  $(d0root)/compack/source/tline1.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/tline1.f ;\
	mv tline1.o $(SCRATCH)/compack/a12/tline1.o ;\
	)
$(d0root)/compack/compack.a(tlines.o):\
  $(d0root)/compack/source/tlines.f\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/tlines.f ;\
	mv tlines.o $(SCRATCH)/compack/a12/tlines.o ;\
	)
$(d0root)/compack/compack.a(traint.o):\
  $(d0root)/compack/source/traint.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/traint.f ;\
	mv traint.o $(SCRATCH)/compack/a12/traint.o ;\
	)
$(d0root)/compack/compack.a(tralog.o):\
  $(d0root)/compack/source/tralog.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/tralog.f ;\
	mv tralog.o $(SCRATCH)/compack/a12/tralog.o ;\
	)
$(d0root)/compack/compack.a(trarea.o):\
  $(d0root)/compack/source/trarea.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/trarea.f ;\
	mv trarea.o $(SCRATCH)/compack/a12/trarea.o ;\
	)
$(d0root)/compack/compack.a(typage.o):\
  $(d0root)/compack/source/typage.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/typage.f ;\
	mv typage.o $(SCRATCH)/compack/a12/typage.o ;\
	)
$(d0root)/compack/compack.a(usrchk.o):\
  $(d0root)/compack/source/usrchk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/usrchk.f ;\
	mv usrchk.o $(SCRATCH)/compack/a12/usrchk.o ;\
	)
$(d0root)/compack/compack.a(waibit.o):\
  $(d0root)/compack/source/waibit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/waibit.f ;\
	mv waibit.o $(SCRATCH)/compack/a12/waibit.o ;\
	)
$(d0root)/compack/compack.a(waitit.o):\
  $(d0root)/compack/source/waitit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/waitit.f ;\
	mv waitit.o $(SCRATCH)/compack/a12/waitit.o ;\
	)
$(d0root)/compack/compack.a(windis.o):\
  $(d0root)/compack/source/windis.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/keycom.inc\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/trmdef.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) compack/source/windis.f ;\
	mv windis.o $(SCRATCH)/compack/a12/windis.o ;\
	)
pre:\
  $(d0root)/compack/source/abomen.f\
  $(d0root)/compack/source/additm.f\
  $(d0root)/compack/source/askhst.f\
  $(d0root)/compack/source/askins.f\
  $(d0root)/compack/source/asklst.f\
  $(d0root)/compack/source/asknum.f\
  $(d0root)/compack/source/askrls.f\
  $(d0root)/compack/source/askrng.f\
  $(d0root)/compack/source/askval.f\
  $(d0root)/compack/source/askyno.f\
  $(d0root)/compack/source/begbat.f\
  $(d0root)/compack/source/broadc.f\
  $(d0root)/compack/source/broast.f\
  $(d0root)/compack/source/canmen.f\
  $(d0root)/compack/source/chcols.f\
  $(d0root)/compack/source/chkcom.f\
  $(d0root)/compack/source/chkist.f\
  $(d0root)/compack/source/chspac.f\
  $(d0root)/compack/source/cmpk_gconfi.f\
  $(d0root)/compack/source/confrm.f\
  $(d0root)/compack/source/conhex.f\
  $(d0root)/compack/source/conint.f\
  $(d0root)/compack/source/conrea.f\
  $(d0root)/compack/source/contry.f\
  $(d0root)/compack/source/ctrast.f\
  $(d0root)/compack/source/curlin.f\
  $(d0root)/compack/source/curonf.f\
  $(d0root)/compack/source/curso1.f\
  $(d0root)/compack/source/cursor.f\
  $(d0root)/compack/source/curtim.f\
  $(d0root)/compack/source/defcol.f\
  $(d0root)/compack/source/defspa.f\
  $(d0root)/compack/source/diabox.f\
  $(d0root)/compack/source/disabl.f\
  $(d0root)/compack/source/display_items.f\
  $(d0root)/compack/source/display_lines.f\
  $(d0root)/compack/source/dmpmsg.f\
  $(d0root)/compack/source/doproc.f\
  $(d0root)/compack/source/drgrid.f\
  $(d0root)/compack/source/drrect.f\
  $(d0root)/compack/source/dumhdl.f\
  $(d0root)/compack/source/endclr.f\
  $(d0root)/compack/source/enddmp.f\
  $(d0root)/compack/source/endlog.f\
  $(d0root)/compack/source/endspl.f\
  $(d0root)/compack/source/endsta.f\
  $(d0root)/compack/source/erase.f\
  $(d0root)/compack/source/evecom.f\
  $(d0root)/compack/source/evefil.f\
  $(d0root)/compack/source/evesav.f\
  $(d0root)/compack/source/eximen.f\
  $(d0root)/compack/source/exiusr.f\
  $(d0root)/compack/source/fillst.f\
  $(d0root)/compack/source/fndfil.f\
  $(d0root)/compack/source/fulmod.f\
  $(d0root)/compack/source/gcomnd.f\
  $(d0root)/compack/source/gconfi.f\
  $(d0root)/compack/source/getarr.f\
  $(d0root)/compack/source/getdev.f\
  $(d0root)/compack/source/getdis.f\
  $(d0root)/compack/source/getdis77.f\
  $(d0root)/compack/source/gethlp.f\
  $(d0root)/compack/source/getopt.f\
  $(d0root)/compack/source/getpar.f\
  $(d0root)/compack/source/getpar77.f\
  $(d0root)/compack/source/getscr.f\
  $(d0root)/compack/source/getspl.f\
  $(d0root)/compack/source/getsta.f\
  $(d0root)/compack/source/getstr.f\
  $(d0root)/compack/source/getsyi.f\
  $(d0root)/compack/source/getwta.f\
  $(d0root)/compack/source/gfulsc.f\
  $(d0root)/compack/source/gincom.f\
  $(d0root)/compack/source/gjourn.f\
  $(d0root)/compack/source/gline0.f\
  $(d0root)/compack/source/gline1.f\
  $(d0root)/compack/source/gline2.f\
  $(d0root)/compack/source/glines.f\
  $(d0root)/compack/source/gnosmg.f\
  $(d0root)/compack/source/gotop.f\
  $(d0root)/compack/source/gpar_lindspl_inpt.f\
  $(d0root)/compack/source/greads.f\
  $(d0root)/compack/source/gsplit.f\
  $(d0root)/compack/source/gstatu.f\
  $(d0root)/compack/source/guserm.f\
  $(d0root)/compack/source/helpme.f\
  $(d0root)/compack/source/hlpget.f\
  $(d0root)/compack/source/hlproc.f\
  $(d0root)/compack/source/inpcmd.f\
  $(d0root)/compack/source/inptrm.f\
  $(d0root)/compack/source/intast.f\
  $(d0root)/compack/source/intmen.f\
  $(d0root)/compack/source/intmsg.f\
  $(d0root)/compack/source/keyast.f\
  $(d0root)/compack/source/keydef.f\
  $(d0root)/compack/source/keyptr.f\
  $(d0root)/compack/source/lenint.f\
  $(d0root)/compack/source/libbig.f\
  $(d0root)/compack/source/libbof.f\
  $(d0root)/compack/source/libbon.f\
  $(d0root)/compack/source/libcar.f\
  $(d0root)/compack/source/libcr1.f\
  $(d0root)/compack/source/libctm.f\
  $(d0root)/compack/source/libcur.f\
  $(d0root)/compack/source/libefc.f\
  $(d0root)/compack/source/libemv.f\
  $(d0root)/compack/source/libera.f\
  $(d0root)/compack/source/liberl.f\
  $(d0root)/compack/source/liberp.f\
  $(d0root)/compack/source/libget.f\
  $(d0root)/compack/source/libhmv.f\
  $(d0root)/compack/source/libind.f\
  $(d0root)/compack/source/libkey.f\
  $(d0root)/compack/source/liblin.f\
  $(d0root)/compack/source/libnxt.f\
  $(d0root)/compack/source/libpmv.f\
  $(d0root)/compack/source/libpst.f\
  $(d0root)/compack/source/libput.f\
  $(d0root)/compack/source/librep.f\
  $(d0root)/compack/source/libres.f\
  $(d0root)/compack/source/libsav.f\
  $(d0root)/compack/source/libscr.f\
  $(d0root)/compack/source/libsna.f\
  $(d0root)/compack/source/libsta.f\
  $(d0root)/compack/source/libstm.f\
  $(d0root)/compack/source/libunp.f\
  $(d0root)/compack/source/libuns.f\
  $(d0root)/compack/source/linchk.f\
  $(d0root)/compack/source/lines.f\
  $(d0root)/compack/source/lines0.f\
  $(d0root)/compack/source/lines1.f\
  $(d0root)/compack/source/linget.f\
  $(d0root)/compack/source/lisbox.f\
  $(d0root)/compack/source/lspawn.f\
  $(d0root)/compack/source/mclref.f\
  $(d0root)/compack/source/menadd.f\
  $(d0root)/compack/source/menctr.f\
  $(d0root)/compack/source/mendef.f\
  $(d0root)/compack/source/mendis.f\
  $(d0root)/compack/source/menexg.f\
  $(d0root)/compack/source/menmai.f\
  $(d0root)/compack/source/menmod.f\
  $(d0root)/compack/source/mennew.f\
  $(d0root)/compack/source/menset.f\
  $(d0root)/compack/source/mensub.f\
  $(d0root)/compack/source/menudo.f\
  $(d0root)/compack/source/menuef.f\
  $(d0root)/compack/source/menuex.f\
  $(d0root)/compack/source/menuop.f\
  $(d0root)/compack/source/modify_params.f\
  $(d0root)/compack/source/movint.f\
  $(d0root)/compack/source/movprt.f\
  $(d0root)/compack/source/movstr.f\
  $(d0root)/compack/source/msetef.f\
  $(d0root)/compack/source/msgscr.f\
  $(d0root)/compack/source/mtimer.f\
  $(d0root)/compack/source/newpar.f\
  $(d0root)/compack/source/normal.f\
  $(d0root)/compack/source/nulhdl.f\
  $(d0root)/compack/source/oline0.f\
  $(d0root)/compack/source/oline1.f\
  $(d0root)/compack/source/olines.f\
  $(d0root)/compack/source/ordset.f\
  $(d0root)/compack/source/oreads.f\
  $(d0root)/compack/source/outmsg.f\
  $(d0root)/compack/source/outsav.f\
  $(d0root)/compack/source/page.f\
  $(d0root)/compack/source/pareal.f\
  $(d0root)/compack/source/parint.f\
  $(d0root)/compack/source/parlog.f\
  $(d0root)/compack/source/parstr.f\
  $(d0root)/compack/source/pfget.f\
  $(d0root)/compack/source/pflabl.f\
  $(d0root)/compack/source/pfnum.f\
  $(d0root)/compack/source/pfwait.f\
  $(d0root)/compack/source/prscom.f\
  $(d0root)/compack/source/prtpbd.f\
  $(d0root)/compack/source/putmsg.f\
  $(d0root)/compack/source/qioast.f\
  $(d0root)/compack/source/rdlog.f\
  $(d0root)/compack/source/rdset.f\
  $(d0root)/compack/source/readme.f\
  $(d0root)/compack/source/readpf.f\
  $(d0root)/compack/source/reasav.f\
  $(d0root)/compack/source/reaset.f\
  $(d0root)/compack/source/relmsg.f\
  $(d0root)/compack/source/repscr.f\
  $(d0root)/compack/source/restat.f\
  $(d0root)/compack/source/runaux.f\
  $(d0root)/compack/source/runcom.f\
  $(d0root)/compack/source/runlog.f\
  $(d0root)/compack/source/runset.f\
  $(d0root)/compack/source/savscr.f\
  $(d0root)/compack/source/scrolines.f\
  $(d0root)/compack/source/set_split_screen.f\
  $(d0root)/compack/source/setchk.f\
  $(d0root)/compack/source/setcom.f\
  $(d0root)/compack/source/setdir.f\
  $(d0root)/compack/source/setdmp.f\
  $(d0root)/compack/source/setflg.f\
  $(d0root)/compack/source/setlin.f\
  $(d0root)/compack/source/setmod.f\
  $(d0root)/compack/source/setqio.f\
  $(d0root)/compack/source/setsta.f\
  $(d0root)/compack/source/spawit.f\
  $(d0root)/compack/source/splsta.f\
  $(d0root)/compack/source/spltit.f\
  $(d0root)/compack/source/stadis.f\
  $(d0root)/compack/source/stamsg.f\
  $(d0root)/compack/source/strap1.f\
  $(d0root)/compack/source/strapp.f\
  $(d0root)/compack/source/strcop.f\
  $(d0root)/compack/source/strdcp.f\
  $(d0root)/compack/source/strfet.f\
  $(d0root)/compack/source/strfre.f\
  $(d0root)/compack/source/strsto.f\
  $(d0root)/compack/source/sysdsp.f\
  $(d0root)/compack/source/tabdis.f\
  $(d0root)/compack/source/tabdisc.f\
  $(d0root)/compack/source/tabdisn.f\
  $(d0root)/compack/source/timast.f\
  $(d0root)/compack/source/timdis.f\
  $(d0root)/compack/source/tline0.f\
  $(d0root)/compack/source/tline1.f\
  $(d0root)/compack/source/tlines.f\
  $(d0root)/compack/source/traint.f\
  $(d0root)/compack/source/tralog.f\
  $(d0root)/compack/source/trarea.f\
  $(d0root)/compack/source/typage.f\
  $(d0root)/compack/source/usrchk.f\
  $(d0root)/compack/source/waibit.f\
  $(d0root)/compack/source/waitit.f\
  $(d0root)/compack/source/windis.f
$(d0root)/compack/source/abomen.f:\
  $(d0root)/compack/source/abomen.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/abomen.for | vmstounix > $(d0root)/compack/source/abomen.f
$(d0root)/compack/source/additm.f:\
  $(d0root)/compack/source/additm.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/additm.for | vmstounix > $(d0root)/compack/source/additm.f
$(d0root)/compack/source/askhst.f:\
  $(d0root)/compack/source/askhst.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/askhst.for | vmstounix > $(d0root)/compack/source/askhst.f
$(d0root)/compack/source/askins.f:\
  $(d0root)/compack/source/askins.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/askins.for | vmstounix > $(d0root)/compack/source/askins.f
$(d0root)/compack/source/asklst.f:\
  $(d0root)/compack/source/asklst.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/asklst.for | vmstounix > $(d0root)/compack/source/asklst.f
$(d0root)/compack/source/asknum.f:\
  $(d0root)/compack/source/asknum.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/asknum.for | vmstounix > $(d0root)/compack/source/asknum.f
$(d0root)/compack/source/askrls.f:\
  $(d0root)/compack/source/askrls.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/askrls.for | vmstounix > $(d0root)/compack/source/askrls.f
$(d0root)/compack/source/askrng.f:\
  $(d0root)/compack/source/askrng.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/askrng.for | vmstounix > $(d0root)/compack/source/askrng.f
$(d0root)/compack/source/askval.f:\
  $(d0root)/compack/source/askval.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/askval.for | vmstounix > $(d0root)/compack/source/askval.f
$(d0root)/compack/source/askyno.f:\
  $(d0root)/compack/source/askyno.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/askyno.for | vmstounix > $(d0root)/compack/source/askyno.f
$(d0root)/compack/source/begbat.f:\
  $(d0root)/compack/source/begbat.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/begbat.for | vmstounix > $(d0root)/compack/source/begbat.f
$(d0root)/compack/source/broadc.f:\
  $(d0root)/compack/source/broadc.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/broadc.for | vmstounix > $(d0root)/compack/source/broadc.f
$(d0root)/compack/source/broast.f:\
  $(d0root)/compack/source/broast.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/broast.for | vmstounix > $(d0root)/compack/source/broast.f
$(d0root)/compack/source/canmen.f:\
  $(d0root)/compack/source/canmen.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/canmen.for | vmstounix > $(d0root)/compack/source/canmen.f
$(d0root)/compack/source/chcols.f:\
  $(d0root)/compack/source/chcols.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/chcols.for | vmstounix > $(d0root)/compack/source/chcols.f
$(d0root)/compack/source/chkcom.f:\
  $(d0root)/compack/source/chkcom.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgdef.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/chkcom.for | vmstounix > $(d0root)/compack/source/chkcom.f
$(d0root)/compack/source/chkist.f:\
  $(d0root)/compack/source/chkist.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/chkist.for | vmstounix > $(d0root)/compack/source/chkist.f
$(d0root)/compack/source/chspac.f:\
  $(d0root)/compack/source/chspac.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/chspac.for | vmstounix > $(d0root)/compack/source/chspac.f
$(d0root)/compack/source/cmpk_gconfi.f:\
  $(d0root)/compack/source/cmpk_gconfi.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/cmpk_gconfi.for | vmstounix > $(d0root)/compack/source/cmpk_gconfi.f
$(d0root)/compack/source/confrm.f:\
  $(d0root)/compack/source/confrm.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/keycom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/confrm.for | vmstounix > $(d0root)/compack/source/confrm.f
$(d0root)/compack/source/conhex.f:\
  $(d0root)/compack/source/conhex.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/conhex.for | vmstounix > $(d0root)/compack/source/conhex.f
$(d0root)/compack/source/conint.f:\
  $(d0root)/compack/source/conint.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/conint.for | vmstounix > $(d0root)/compack/source/conint.f
$(d0root)/compack/source/conrea.f:\
  $(d0root)/compack/source/conrea.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/conrea.for | vmstounix > $(d0root)/compack/source/conrea.f
$(d0root)/compack/source/contry.f:\
  $(d0root)/compack/source/contry.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/contry.for | vmstounix > $(d0root)/compack/source/contry.f
$(d0root)/compack/source/ctrast.f:\
  $(d0root)/compack/source/ctrast.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/ctrast.for | vmstounix > $(d0root)/compack/source/ctrast.f
$(d0root)/compack/source/curlin.f:\
  $(d0root)/compack/source/curlin.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/curlin.for | vmstounix > $(d0root)/compack/source/curlin.f
$(d0root)/compack/source/curonf.f:\
  $(d0root)/compack/source/curonf.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/curonf.for | vmstounix > $(d0root)/compack/source/curonf.f
$(d0root)/compack/source/curso1.f:\
  $(d0root)/compack/source/curso1.for\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/smgdef.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/curso1.for | vmstounix > $(d0root)/compack/source/curso1.f
$(d0root)/compack/source/cursor.f:\
  $(d0root)/compack/source/cursor.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/cursor.for | vmstounix > $(d0root)/compack/source/cursor.f
$(d0root)/compack/source/curtim.f:\
  $(d0root)/compack/source/curtim.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/curtim.for | vmstounix > $(d0root)/compack/source/curtim.f
$(d0root)/compack/source/defcol.f:\
  $(d0root)/compack/source/defcol.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/defcol.for | vmstounix > $(d0root)/compack/source/defcol.f
$(d0root)/compack/source/defspa.f:\
  $(d0root)/compack/source/defspa.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/defspa.for | vmstounix > $(d0root)/compack/source/defspa.f
$(d0root)/compack/source/diabox.f:\
  $(d0root)/compack/source/diabox.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/keycom.inc\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/trmdef.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/diabox.for | vmstounix > $(d0root)/compack/source/diabox.f
$(d0root)/compack/source/disabl.f:\
  $(d0root)/compack/source/disabl.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/disabl.for | vmstounix > $(d0root)/compack/source/disabl.f
$(d0root)/compack/source/display_items.f:\
  $(d0root)/compack/source/display_items.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/display_items.for | vmstounix > $(d0root)/compack/source/display_items.f
$(d0root)/compack/source/display_lines.f:\
  $(d0root)/compack/source/display_lines.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/display_lines.for | vmstounix > $(d0root)/compack/source/display_lines.f
$(d0root)/compack/source/dmpmsg.f:\
  $(d0root)/compack/source/dmpmsg.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/dmpmsg.for | vmstounix > $(d0root)/compack/source/dmpmsg.f
$(d0root)/compack/source/doproc.f:\
  $(d0root)/compack/source/doproc.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/doproc.for | vmstounix > $(d0root)/compack/source/doproc.f
$(d0root)/compack/source/drgrid.f:\
  $(d0root)/compack/source/drgrid.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/drgrid.for | vmstounix > $(d0root)/compack/source/drgrid.f
$(d0root)/compack/source/drrect.f:\
  $(d0root)/compack/source/drrect.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/drrect.for | vmstounix > $(d0root)/compack/source/drrect.f
$(d0root)/compack/source/dumhdl.f:\
  $(d0root)/compack/source/dumhdl.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/dumhdl.for | vmstounix > $(d0root)/compack/source/dumhdl.f
$(d0root)/compack/source/endclr.f:\
  $(d0root)/compack/source/endclr.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/endclr.for | vmstounix > $(d0root)/compack/source/endclr.f
$(d0root)/compack/source/enddmp.f:\
  $(d0root)/compack/source/enddmp.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/enddmp.for | vmstounix > $(d0root)/compack/source/enddmp.f
$(d0root)/compack/source/endlog.f:\
  $(d0root)/compack/source/endlog.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/endlog.for | vmstounix > $(d0root)/compack/source/endlog.f
$(d0root)/compack/source/endspl.f:\
  $(d0root)/compack/source/endspl.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/endspl.for | vmstounix > $(d0root)/compack/source/endspl.f
$(d0root)/compack/source/endsta.f:\
  $(d0root)/compack/source/endsta.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/endsta.for | vmstounix > $(d0root)/compack/source/endsta.f
$(d0root)/compack/source/erase.f:\
  $(d0root)/compack/source/erase.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/erase.for | vmstounix > $(d0root)/compack/source/erase.f
$(d0root)/compack/source/evecom.f:\
  $(d0root)/compack/source/evecom.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/evecom.for | vmstounix > $(d0root)/compack/source/evecom.f
$(d0root)/compack/source/evefil.f:\
  $(d0root)/compack/source/evefil.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/evefil.for | vmstounix > $(d0root)/compack/source/evefil.f
$(d0root)/compack/source/evesav.f:\
  $(d0root)/compack/source/evesav.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/evesav.for | vmstounix > $(d0root)/compack/source/evesav.f
$(d0root)/compack/source/eximen.f:\
  $(d0root)/compack/source/eximen.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/eximen.for | vmstounix > $(d0root)/compack/source/eximen.f
$(d0root)/compack/source/exiusr.f:\
  $(d0root)/compack/source/exiusr.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/exiusr.for | vmstounix > $(d0root)/compack/source/exiusr.f
$(d0root)/compack/source/fillst.f:\
  $(d0root)/compack/source/fillst.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/fillst.for | vmstounix > $(d0root)/compack/source/fillst.f
$(d0root)/compack/source/fndfil.f:\
  $(d0root)/compack/source/fndfil.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/fndfil.for | vmstounix > $(d0root)/compack/source/fndfil.f
$(d0root)/compack/source/fulmod.f:\
  $(d0root)/compack/source/fulmod.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/fulmod.for | vmstounix > $(d0root)/compack/source/fulmod.f
$(d0root)/compack/source/gcomnd.f:\
  $(d0root)/compack/source/gcomnd.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/gcomnd.for | vmstounix > $(d0root)/compack/source/gcomnd.f
$(d0root)/compack/source/gconfi.f:\
  $(d0root)/compack/source/gconfi.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/gconfi.for | vmstounix > $(d0root)/compack/source/gconfi.f
$(d0root)/compack/source/getarr.f:\
  $(d0root)/compack/source/getarr.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/getarr.for | vmstounix > $(d0root)/compack/source/getarr.f
$(d0root)/compack/source/getdev.f:\
  $(d0root)/compack/source/getdev.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/getdev.for | vmstounix > $(d0root)/compack/source/getdev.f
$(d0root)/compack/source/getdis.f:\
  $(d0root)/compack/source/getdis.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/getdis.for | vmstounix > $(d0root)/compack/source/getdis.f
$(d0root)/compack/source/getdis77.f:\
  $(d0root)/compack/source/getdis77.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/getdis77.for | vmstounix > $(d0root)/compack/source/getdis77.f
$(d0root)/compack/source/gethlp.f:\
  $(d0root)/compack/source/gethlp.for\
  $(d0library)/inc/smgdef.def\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/gethlp.for | vmstounix > $(d0root)/compack/source/gethlp.f
$(d0root)/compack/source/getopt.f:\
  $(d0root)/compack/source/getopt.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/getopt.for | vmstounix > $(d0root)/compack/source/getopt.f
$(d0root)/compack/source/getpar.f:\
  $(d0root)/compack/source/getpar.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/getpar.for | vmstounix > $(d0root)/compack/source/getpar.f
$(d0root)/compack/source/getpar77.f:\
  $(d0root)/compack/source/getpar77.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/getpar77.for | vmstounix > $(d0root)/compack/source/getpar77.f
$(d0root)/compack/source/getscr.f:\
  $(d0root)/compack/source/getscr.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/getscr.for | vmstounix > $(d0root)/compack/source/getscr.f
$(d0root)/compack/source/getspl.f:\
  $(d0root)/compack/source/getspl.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/getspl.for | vmstounix > $(d0root)/compack/source/getspl.f
$(d0root)/compack/source/getsta.f:\
  $(d0root)/compack/source/getsta.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/getsta.for | vmstounix > $(d0root)/compack/source/getsta.f
$(d0root)/compack/source/getstr.f:\
  $(d0root)/compack/source/getstr.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/getstr.for | vmstounix > $(d0root)/compack/source/getstr.f
$(d0root)/compack/source/getsyi.f:\
  $(d0root)/compack/source/getsyi.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/getsyi.for | vmstounix > $(d0root)/compack/source/getsyi.f
$(d0root)/compack/source/getwta.f:\
  $(d0root)/compack/source/getwta.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/getwta.for | vmstounix > $(d0root)/compack/source/getwta.f
$(d0root)/compack/source/gfulsc.f:\
  $(d0root)/compack/source/gfulsc.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/gfulsc.for | vmstounix > $(d0root)/compack/source/gfulsc.f
$(d0root)/compack/source/gincom.f:\
  $(d0root)/compack/source/gincom.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/gincom.for | vmstounix > $(d0root)/compack/source/gincom.f
$(d0root)/compack/source/gjourn.f:\
  $(d0root)/compack/source/gjourn.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/gjourn.for | vmstounix > $(d0root)/compack/source/gjourn.f
$(d0root)/compack/source/gline0.f:\
  $(d0root)/compack/source/gline0.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/gline0.for | vmstounix > $(d0root)/compack/source/gline0.f
$(d0root)/compack/source/gline1.f:\
  $(d0root)/compack/source/gline1.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/gline1.for | vmstounix > $(d0root)/compack/source/gline1.f
$(d0root)/compack/source/gline2.f:\
  $(d0root)/compack/source/gline2.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/gline2.for | vmstounix > $(d0root)/compack/source/gline2.f
$(d0root)/compack/source/glines.f:\
  $(d0root)/compack/source/glines.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/glines.for | vmstounix > $(d0root)/compack/source/glines.f
$(d0root)/compack/source/gnosmg.f:\
  $(d0root)/compack/source/gnosmg.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/gnosmg.for | vmstounix > $(d0root)/compack/source/gnosmg.f
$(d0root)/compack/source/gotop.f:\
  $(d0root)/compack/source/gotop.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/gotop.for | vmstounix > $(d0root)/compack/source/gotop.f
$(d0root)/compack/source/gpar_lindspl_inpt.f:\
  $(d0root)/compack/source/gpar_lindspl_inpt.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/gpar_lindspl_inpt.for | vmstounix > $(d0root)/compack/source/gpar_lindspl_inpt.f
$(d0root)/compack/source/greads.f:\
  $(d0root)/compack/source/greads.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/trmdef.def\
  $(d0library)/inc/smgdef.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/greads.for | vmstounix > $(d0root)/compack/source/greads.f
$(d0root)/compack/source/gsplit.f:\
  $(d0root)/compack/source/gsplit.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/gsplit.for | vmstounix > $(d0root)/compack/source/gsplit.f
$(d0root)/compack/source/gstatu.f:\
  $(d0root)/compack/source/gstatu.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/gstatu.for | vmstounix > $(d0root)/compack/source/gstatu.f
$(d0root)/compack/source/guserm.f:\
  $(d0root)/compack/source/guserm.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/guserm.for | vmstounix > $(d0root)/compack/source/guserm.f
$(d0root)/compack/source/helpme.f:\
  $(d0root)/compack/source/helpme.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/helpme.for | vmstounix > $(d0root)/compack/source/helpme.f
$(d0root)/compack/source/hlpget.f:\
  $(d0root)/compack/source/hlpget.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/hlpget.for | vmstounix > $(d0root)/compack/source/hlpget.f
$(d0root)/compack/source/hlproc.f:\
  $(d0root)/compack/source/hlproc.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/hlproc.for | vmstounix > $(d0root)/compack/source/hlproc.f
$(d0root)/compack/source/inpcmd.f:\
  $(d0root)/compack/source/inpcmd.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/inpcmd.for | vmstounix > $(d0root)/compack/source/inpcmd.f
$(d0root)/compack/source/inptrm.f:\
  $(d0root)/compack/source/inptrm.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/inptrm.for | vmstounix > $(d0root)/compack/source/inptrm.f
$(d0root)/compack/source/intast.f:\
  $(d0root)/compack/source/intast.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/intast.for | vmstounix > $(d0root)/compack/source/intast.f
$(d0root)/compack/source/intmen.f:\
  $(d0root)/compack/source/intmen.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/intmen.for | vmstounix > $(d0root)/compack/source/intmen.f
$(d0root)/compack/source/intmsg.f:\
  $(d0root)/compack/source/intmsg.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/intmsg.for | vmstounix > $(d0root)/compack/source/intmsg.f
$(d0root)/compack/source/keyast.f:\
  $(d0root)/compack/source/keyast.for\
  $(d0library)/inc/keycom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/keyast.for | vmstounix > $(d0root)/compack/source/keyast.f
$(d0root)/compack/source/keydef.f:\
  $(d0root)/compack/source/keydef.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/trmdef.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/keydef.for | vmstounix > $(d0root)/compack/source/keydef.f
$(d0root)/compack/source/keyptr.f:\
  $(d0root)/compack/source/keyptr.for\
  $(d0library)/inc/keycom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/keyptr.for | vmstounix > $(d0root)/compack/source/keyptr.f
$(d0root)/compack/source/lenint.f:\
  $(d0root)/compack/source/lenint.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/lenint.for | vmstounix > $(d0root)/compack/source/lenint.f
$(d0root)/compack/source/libbig.f:\
  $(d0root)/compack/source/libbig.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libbig.for | vmstounix > $(d0root)/compack/source/libbig.f
$(d0root)/compack/source/libbof.f:\
  $(d0root)/compack/source/libbof.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libbof.for | vmstounix > $(d0root)/compack/source/libbof.f
$(d0root)/compack/source/libbon.f:\
  $(d0root)/compack/source/libbon.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libbon.for | vmstounix > $(d0root)/compack/source/libbon.f
$(d0root)/compack/source/libcar.f:\
  $(d0root)/compack/source/libcar.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libcar.for | vmstounix > $(d0root)/compack/source/libcar.f
$(d0root)/compack/source/libcr1.f:\
  $(d0root)/compack/source/libcr1.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libcr1.for | vmstounix > $(d0root)/compack/source/libcr1.f
$(d0root)/compack/source/libctm.f:\
  $(d0root)/compack/source/libctm.for\
  $(d0library)/inc/keycom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libctm.for | vmstounix > $(d0root)/compack/source/libctm.f
$(d0root)/compack/source/libcur.f:\
  $(d0root)/compack/source/libcur.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libcur.for | vmstounix > $(d0root)/compack/source/libcur.f
$(d0root)/compack/source/libefc.f:\
  $(d0root)/compack/source/libefc.for\
  $(d0library)/inc/keycom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libefc.for | vmstounix > $(d0root)/compack/source/libefc.f
$(d0root)/compack/source/libemv.f:\
  $(d0root)/compack/source/libemv.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libemv.for | vmstounix > $(d0root)/compack/source/libemv.f
$(d0root)/compack/source/libera.f:\
  $(d0root)/compack/source/libera.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libera.for | vmstounix > $(d0root)/compack/source/libera.f
$(d0root)/compack/source/liberl.f:\
  $(d0root)/compack/source/liberl.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/liberl.for | vmstounix > $(d0root)/compack/source/liberl.f
$(d0root)/compack/source/liberp.f:\
  $(d0root)/compack/source/liberp.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/liberp.for | vmstounix > $(d0root)/compack/source/liberp.f
$(d0root)/compack/source/libget.f:\
  $(d0root)/compack/source/libget.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libget.for | vmstounix > $(d0root)/compack/source/libget.f
$(d0root)/compack/source/libhmv.f:\
  $(d0root)/compack/source/libhmv.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libhmv.for | vmstounix > $(d0root)/compack/source/libhmv.f
$(d0root)/compack/source/libind.f:\
  $(d0root)/compack/source/libind.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libind.for | vmstounix > $(d0root)/compack/source/libind.f
$(d0root)/compack/source/libkey.f:\
  $(d0root)/compack/source/libkey.for\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/keycom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libkey.for | vmstounix > $(d0root)/compack/source/libkey.f
$(d0root)/compack/source/liblin.f:\
  $(d0root)/compack/source/liblin.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/liblin.for | vmstounix > $(d0root)/compack/source/liblin.f
$(d0root)/compack/source/libnxt.f:\
  $(d0root)/compack/source/libnxt.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libnxt.for | vmstounix > $(d0root)/compack/source/libnxt.f
$(d0root)/compack/source/libpmv.f:\
  $(d0root)/compack/source/libpmv.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libpmv.for | vmstounix > $(d0root)/compack/source/libpmv.f
$(d0root)/compack/source/libpst.f:\
  $(d0root)/compack/source/libpst.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libpst.for | vmstounix > $(d0root)/compack/source/libpst.f
$(d0root)/compack/source/libput.f:\
  $(d0root)/compack/source/libput.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libput.for | vmstounix > $(d0root)/compack/source/libput.f
$(d0root)/compack/source/librep.f:\
  $(d0root)/compack/source/librep.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/librep.for | vmstounix > $(d0root)/compack/source/librep.f
$(d0root)/compack/source/libres.f:\
  $(d0root)/compack/source/libres.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libres.for | vmstounix > $(d0root)/compack/source/libres.f
$(d0root)/compack/source/libsav.f:\
  $(d0root)/compack/source/libsav.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libsav.for | vmstounix > $(d0root)/compack/source/libsav.f
$(d0root)/compack/source/libscr.f:\
  $(d0root)/compack/source/libscr.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libscr.for | vmstounix > $(d0root)/compack/source/libscr.f
$(d0root)/compack/source/libsna.f:\
  $(d0root)/compack/source/libsna.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libsna.for | vmstounix > $(d0root)/compack/source/libsna.f
$(d0root)/compack/source/libsta.f:\
  $(d0root)/compack/source/libsta.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libsta.for | vmstounix > $(d0root)/compack/source/libsta.f
$(d0root)/compack/source/libstm.f:\
  $(d0root)/compack/source/libstm.for\
  $(d0library)/inc/keycom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libstm.for | vmstounix > $(d0root)/compack/source/libstm.f
$(d0root)/compack/source/libunp.f:\
  $(d0root)/compack/source/libunp.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libunp.for | vmstounix > $(d0root)/compack/source/libunp.f
$(d0root)/compack/source/libuns.f:\
  $(d0root)/compack/source/libuns.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/libuns.for | vmstounix > $(d0root)/compack/source/libuns.f
$(d0root)/compack/source/linchk.f:\
  $(d0root)/compack/source/linchk.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/linchk.for | vmstounix > $(d0root)/compack/source/linchk.f
$(d0root)/compack/source/lines.f:\
  $(d0root)/compack/source/lines.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/lines.for | vmstounix > $(d0root)/compack/source/lines.f
$(d0root)/compack/source/lines0.f:\
  $(d0root)/compack/source/lines0.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/lines0.for | vmstounix > $(d0root)/compack/source/lines0.f
$(d0root)/compack/source/lines1.f:\
  $(d0root)/compack/source/lines1.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/lines1.for | vmstounix > $(d0root)/compack/source/lines1.f
$(d0root)/compack/source/linget.f:\
  $(d0root)/compack/source/linget.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/linget.for | vmstounix > $(d0root)/compack/source/linget.f
$(d0root)/compack/source/lisbox.f:\
  $(d0root)/compack/source/lisbox.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/keycom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/lisbox.for | vmstounix > $(d0root)/compack/source/lisbox.f
$(d0root)/compack/source/lspawn.f:\
  $(d0root)/compack/source/lspawn.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/lspawn.for | vmstounix > $(d0root)/compack/source/lspawn.f
$(d0root)/compack/source/mclref.f:\
  $(d0root)/compack/source/mclref.for\
  $(d0library)/inc/keycom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/mclref.for | vmstounix > $(d0root)/compack/source/mclref.f
$(d0root)/compack/source/menadd.f:\
  $(d0root)/compack/source/menadd.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/menadd.for | vmstounix > $(d0root)/compack/source/menadd.f
$(d0root)/compack/source/menctr.f:\
  $(d0root)/compack/source/menctr.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/menctr.for | vmstounix > $(d0root)/compack/source/menctr.f
$(d0root)/compack/source/mendef.f:\
  $(d0root)/compack/source/mendef.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/mendef.for | vmstounix > $(d0root)/compack/source/mendef.f
$(d0root)/compack/source/mendis.f:\
  $(d0root)/compack/source/mendis.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/mendis.for | vmstounix > $(d0root)/compack/source/mendis.f
$(d0root)/compack/source/menexg.f:\
  $(d0root)/compack/source/menexg.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/menexg.for | vmstounix > $(d0root)/compack/source/menexg.f
$(d0root)/compack/source/menmai.f:\
  $(d0root)/compack/source/menmai.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/menmai.for | vmstounix > $(d0root)/compack/source/menmai.f
$(d0root)/compack/source/menmod.f:\
  $(d0root)/compack/source/menmod.for\
  $(d0library)/inc/keycom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/menmod.for | vmstounix > $(d0root)/compack/source/menmod.f
$(d0root)/compack/source/mennew.f:\
  $(d0root)/compack/source/mennew.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/mennew.for | vmstounix > $(d0root)/compack/source/mennew.f
$(d0root)/compack/source/menset.f:\
  $(d0root)/compack/source/menset.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/menset.for | vmstounix > $(d0root)/compack/source/menset.f
$(d0root)/compack/source/mensub.f:\
  $(d0root)/compack/source/mensub.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/mensub.for | vmstounix > $(d0root)/compack/source/mensub.f
$(d0root)/compack/source/menudo.f:\
  $(d0root)/compack/source/menudo.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/menudo.for | vmstounix > $(d0root)/compack/source/menudo.f
$(d0root)/compack/source/menuef.f:\
  $(d0root)/compack/source/menuef.for\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/keycom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/menuef.for | vmstounix > $(d0root)/compack/source/menuef.f
$(d0root)/compack/source/menuex.f:\
  $(d0root)/compack/source/menuex.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/menuex.for | vmstounix > $(d0root)/compack/source/menuex.f
$(d0root)/compack/source/menuop.f:\
  $(d0root)/compack/source/menuop.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/menuop.for | vmstounix > $(d0root)/compack/source/menuop.f
$(d0root)/compack/source/modify_params.f:\
  $(d0root)/compack/source/modify_params.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/modify_params.for | vmstounix > $(d0root)/compack/source/modify_params.f
$(d0root)/compack/source/movint.f:\
  $(d0root)/compack/source/movint.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/movint.for | vmstounix > $(d0root)/compack/source/movint.f
$(d0root)/compack/source/movprt.f:\
  $(d0root)/compack/source/movprt.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/movprt.for | vmstounix > $(d0root)/compack/source/movprt.f
$(d0root)/compack/source/movstr.f:\
  $(d0root)/compack/source/movstr.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/movstr.for | vmstounix > $(d0root)/compack/source/movstr.f
$(d0root)/compack/source/msetef.f:\
  $(d0root)/compack/source/msetef.for\
  $(d0library)/inc/keycom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/msetef.for | vmstounix > $(d0root)/compack/source/msetef.f
$(d0root)/compack/source/msgscr.f:\
  $(d0root)/compack/source/msgscr.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/msgscr.for | vmstounix > $(d0root)/compack/source/msgscr.f
$(d0root)/compack/source/mtimer.f:\
  $(d0root)/compack/source/mtimer.for\
  $(d0library)/inc/keycom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/mtimer.for | vmstounix > $(d0root)/compack/source/mtimer.f
$(d0root)/compack/source/newpar.f:\
  $(d0root)/compack/source/newpar.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/newpar.for | vmstounix > $(d0root)/compack/source/newpar.f
$(d0root)/compack/source/normal.f:\
  $(d0root)/compack/source/normal.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/normal.for | vmstounix > $(d0root)/compack/source/normal.f
$(d0root)/compack/source/nulhdl.f:\
  $(d0root)/compack/source/nulhdl.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/nulhdl.for | vmstounix > $(d0root)/compack/source/nulhdl.f
$(d0root)/compack/source/oline0.f:\
  $(d0root)/compack/source/oline0.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/oline0.for | vmstounix > $(d0root)/compack/source/oline0.f
$(d0root)/compack/source/oline1.f:\
  $(d0root)/compack/source/oline1.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/oline1.for | vmstounix > $(d0root)/compack/source/oline1.f
$(d0root)/compack/source/olines.f:\
  $(d0root)/compack/source/olines.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/olines.for | vmstounix > $(d0root)/compack/source/olines.f
$(d0root)/compack/source/ordset.f:\
  $(d0root)/compack/source/ordset.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/ordset.for | vmstounix > $(d0root)/compack/source/ordset.f
$(d0root)/compack/source/oreads.f:\
  $(d0root)/compack/source/oreads.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/oreads.for | vmstounix > $(d0root)/compack/source/oreads.f
$(d0root)/compack/source/outmsg.f:\
  $(d0root)/compack/source/outmsg.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/outmsg.for | vmstounix > $(d0root)/compack/source/outmsg.f
$(d0root)/compack/source/outsav.f:\
  $(d0root)/compack/source/outsav.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/outsav.for | vmstounix > $(d0root)/compack/source/outsav.f
$(d0root)/compack/source/page.f:\
  $(d0root)/compack/source/page.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/page.for | vmstounix > $(d0root)/compack/source/page.f
$(d0root)/compack/source/pareal.f:\
  $(d0root)/compack/source/pareal.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/pareal.for | vmstounix > $(d0root)/compack/source/pareal.f
$(d0root)/compack/source/parint.f:\
  $(d0root)/compack/source/parint.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/parint.for | vmstounix > $(d0root)/compack/source/parint.f
$(d0root)/compack/source/parlog.f:\
  $(d0root)/compack/source/parlog.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/parlog.for | vmstounix > $(d0root)/compack/source/parlog.f
$(d0root)/compack/source/parstr.f:\
  $(d0root)/compack/source/parstr.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/parstr.for | vmstounix > $(d0root)/compack/source/parstr.f
$(d0root)/compack/source/pfget.f:\
  $(d0root)/compack/source/pfget.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/pfget.for | vmstounix > $(d0root)/compack/source/pfget.f
$(d0root)/compack/source/pflabl.f:\
  $(d0root)/compack/source/pflabl.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/pflabl.for | vmstounix > $(d0root)/compack/source/pflabl.f
$(d0root)/compack/source/pfnum.f:\
  $(d0root)/compack/source/pfnum.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/pfnum.for | vmstounix > $(d0root)/compack/source/pfnum.f
$(d0root)/compack/source/pfwait.f:\
  $(d0root)/compack/source/pfwait.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/pfwait.for | vmstounix > $(d0root)/compack/source/pfwait.f
$(d0root)/compack/source/prscom.f:\
  $(d0root)/compack/source/prscom.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/prscom.for | vmstounix > $(d0root)/compack/source/prscom.f
$(d0root)/compack/source/prtpbd.f:\
  $(d0root)/compack/source/prtpbd.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/prtpbd.for | vmstounix > $(d0root)/compack/source/prtpbd.f
$(d0root)/compack/source/putmsg.f:\
  $(d0root)/compack/source/putmsg.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/putmsg.for | vmstounix > $(d0root)/compack/source/putmsg.f
$(d0root)/compack/source/qioast.f:\
  $(d0root)/compack/source/qioast.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/qioast.for | vmstounix > $(d0root)/compack/source/qioast.f
$(d0root)/compack/source/rdlog.f:\
  $(d0root)/compack/source/rdlog.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/rdlog.for | vmstounix > $(d0root)/compack/source/rdlog.f
$(d0root)/compack/source/rdset.f:\
  $(d0root)/compack/source/rdset.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0root)/compack/source/commsg.def\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/rdset.for | vmstounix > $(d0root)/compack/source/rdset.f
$(d0root)/compack/source/readme.f:\
  $(d0root)/compack/source/readme.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/trmdef.def\
  $(d0library)/inc/smgdef.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/readme.for | vmstounix > $(d0root)/compack/source/readme.f
$(d0root)/compack/source/readpf.f:\
  $(d0root)/compack/source/readpf.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/readpf.for | vmstounix > $(d0root)/compack/source/readpf.f
$(d0root)/compack/source/reasav.f:\
  $(d0root)/compack/source/reasav.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/reasav.for | vmstounix > $(d0root)/compack/source/reasav.f
$(d0root)/compack/source/reaset.f:\
  $(d0root)/compack/source/reaset.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/smgdef.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/reaset.for | vmstounix > $(d0root)/compack/source/reaset.f
$(d0root)/compack/source/relmsg.f:\
  $(d0root)/compack/source/relmsg.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/relmsg.for | vmstounix > $(d0root)/compack/source/relmsg.f
$(d0root)/compack/source/repscr.f:\
  $(d0root)/compack/source/repscr.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/repscr.for | vmstounix > $(d0root)/compack/source/repscr.f
$(d0root)/compack/source/restat.f:\
  $(d0root)/compack/source/restat.for\
  $(d0library)/inc/timsta.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/restat.for | vmstounix > $(d0root)/compack/source/restat.f
$(d0root)/compack/source/runaux.f:\
  $(d0root)/compack/source/runaux.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/runaux.for | vmstounix > $(d0root)/compack/source/runaux.f
$(d0root)/compack/source/runcom.f:\
  $(d0root)/compack/source/runcom.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/runcom.for | vmstounix > $(d0root)/compack/source/runcom.f
$(d0root)/compack/source/runlog.f:\
  $(d0root)/compack/source/runlog.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/runlog.for | vmstounix > $(d0root)/compack/source/runlog.f
$(d0root)/compack/source/runset.f:\
  $(d0root)/compack/source/runset.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/runset.for | vmstounix > $(d0root)/compack/source/runset.f
$(d0root)/compack/source/savscr.f:\
  $(d0root)/compack/source/savscr.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/savscr.for | vmstounix > $(d0root)/compack/source/savscr.f
$(d0root)/compack/source/scrolines.f:\
  $(d0root)/compack/source/scrolines.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/scrolines.for | vmstounix > $(d0root)/compack/source/scrolines.f
$(d0root)/compack/source/set_split_screen.f:\
  $(d0root)/compack/source/set_split_screen.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/set_split_screen.for | vmstounix > $(d0root)/compack/source/set_split_screen.f
$(d0root)/compack/source/setchk.f:\
  $(d0root)/compack/source/setchk.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/ssdef.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/setchk.for | vmstounix > $(d0root)/compack/source/setchk.f
$(d0root)/compack/source/setcom.f:\
  $(d0root)/compack/source/setcom.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/setcom.for | vmstounix > $(d0root)/compack/source/setcom.f
$(d0root)/compack/source/setdir.f:\
  $(d0root)/compack/source/setdir.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/setdir.for | vmstounix > $(d0root)/compack/source/setdir.f
$(d0root)/compack/source/setdmp.f:\
  $(d0root)/compack/source/setdmp.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/setdmp.for | vmstounix > $(d0root)/compack/source/setdmp.f
$(d0root)/compack/source/setflg.f:\
  $(d0root)/compack/source/setflg.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/setflg.for | vmstounix > $(d0root)/compack/source/setflg.f
$(d0root)/compack/source/setlin.f:\
  $(d0root)/compack/source/setlin.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/setlin.for | vmstounix > $(d0root)/compack/source/setlin.f
$(d0root)/compack/source/setmod.f:\
  $(d0root)/compack/source/setmod.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/setmod.for | vmstounix > $(d0root)/compack/source/setmod.f
$(d0root)/compack/source/setqio.f:\
  $(d0root)/compack/source/setqio.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/setqio.for | vmstounix > $(d0root)/compack/source/setqio.f
$(d0root)/compack/source/setsta.f:\
  $(d0root)/compack/source/setsta.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/setsta.for | vmstounix > $(d0root)/compack/source/setsta.f
$(d0root)/compack/source/spawit.f:\
  $(d0root)/compack/source/spawit.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/spawit.for | vmstounix > $(d0root)/compack/source/spawit.f
$(d0root)/compack/source/splsta.f:\
  $(d0root)/compack/source/splsta.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/splsta.for | vmstounix > $(d0root)/compack/source/splsta.f
$(d0root)/compack/source/spltit.f:\
  $(d0root)/compack/source/spltit.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/spltit.for | vmstounix > $(d0root)/compack/source/spltit.f
$(d0root)/compack/source/stadis.f:\
  $(d0root)/compack/source/stadis.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/stadis.for | vmstounix > $(d0root)/compack/source/stadis.f
$(d0root)/compack/source/stamsg.f:\
  $(d0root)/compack/source/stamsg.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/stamsg.for | vmstounix > $(d0root)/compack/source/stamsg.f
$(d0root)/compack/source/strap1.f:\
  $(d0root)/compack/source/strap1.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/strap1.for | vmstounix > $(d0root)/compack/source/strap1.f
$(d0root)/compack/source/strapp.f:\
  $(d0root)/compack/source/strapp.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/strapp.for | vmstounix > $(d0root)/compack/source/strapp.f
$(d0root)/compack/source/strcop.f:\
  $(d0root)/compack/source/strcop.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/strcop.for | vmstounix > $(d0root)/compack/source/strcop.f
$(d0root)/compack/source/strdcp.f:\
  $(d0root)/compack/source/strdcp.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/strdcp.for | vmstounix > $(d0root)/compack/source/strdcp.f
$(d0root)/compack/source/strfet.f:\
  $(d0root)/compack/source/strfet.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/strfet.for | vmstounix > $(d0root)/compack/source/strfet.f
$(d0root)/compack/source/strfre.f:\
  $(d0root)/compack/source/strfre.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/strfre.for | vmstounix > $(d0root)/compack/source/strfre.f
$(d0root)/compack/source/strsto.f:\
  $(d0root)/compack/source/strsto.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/strsto.for | vmstounix > $(d0root)/compack/source/strsto.f
$(d0root)/compack/source/sysdsp.f:\
  $(d0root)/compack/source/sysdsp.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/sysdsp.for | vmstounix > $(d0root)/compack/source/sysdsp.f
$(d0root)/compack/source/tabdis.f:\
  $(d0root)/compack/source/tabdis.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/tabdis.for | vmstounix > $(d0root)/compack/source/tabdis.f
$(d0root)/compack/source/tabdisc.f:\
  $(d0root)/compack/source/tabdisc.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/tabdisc.for | vmstounix > $(d0root)/compack/source/tabdisc.f
$(d0root)/compack/source/tabdisn.f:\
  $(d0root)/compack/source/tabdisn.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/tabdisn.for | vmstounix > $(d0root)/compack/source/tabdisn.f
$(d0root)/compack/source/timast.f:\
  $(d0root)/compack/source/timast.for\
  $(d0library)/inc/keycom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/timast.for | vmstounix > $(d0root)/compack/source/timast.f
$(d0root)/compack/source/timdis.f:\
  $(d0root)/compack/source/timdis.for\
  $(d0library)/inc/timsta.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/timdis.for | vmstounix > $(d0root)/compack/source/timdis.f
$(d0root)/compack/source/tline0.f:\
  $(d0root)/compack/source/tline0.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/tline0.for | vmstounix > $(d0root)/compack/source/tline0.f
$(d0root)/compack/source/tline1.f:\
  $(d0root)/compack/source/tline1.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/tline1.for | vmstounix > $(d0root)/compack/source/tline1.f
$(d0root)/compack/source/tlines.f:\
  $(d0root)/compack/source/tlines.for\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/tlines.for | vmstounix > $(d0root)/compack/source/tlines.f
$(d0root)/compack/source/traint.f:\
  $(d0root)/compack/source/traint.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/traint.for | vmstounix > $(d0root)/compack/source/traint.f
$(d0root)/compack/source/tralog.f:\
  $(d0root)/compack/source/tralog.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/tralog.for | vmstounix > $(d0root)/compack/source/tralog.f
$(d0root)/compack/source/trarea.f:\
  $(d0root)/compack/source/trarea.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/trarea.for | vmstounix > $(d0root)/compack/source/trarea.f
$(d0root)/compack/source/typage.f:\
  $(d0root)/compack/source/typage.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/typage.for | vmstounix > $(d0root)/compack/source/typage.f
$(d0root)/compack/source/usrchk.f:\
  $(d0root)/compack/source/usrchk.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/usrchk.for | vmstounix > $(d0root)/compack/source/usrchk.f
$(d0root)/compack/source/waibit.f:\
  $(d0root)/compack/source/waibit.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/waibit.for | vmstounix > $(d0root)/compack/source/waibit.f
$(d0root)/compack/source/waitit.f:\
  $(d0root)/compack/source/waitit.for
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/waitit.for | vmstounix > $(d0root)/compack/source/waitit.f
$(d0root)/compack/source/windis.f:\
  $(d0root)/compack/source/windis.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc\
  $(d0library)/inc/keycom.inc\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/trmdef.def
	tasteofd0 $(FLAVOR) < $(d0root)/compack/source/windis.for | vmstounix > $(d0root)/compack/source/windis.f
for:
	touch $(d0root)/compack/source/abomen.for
	touch $(d0root)/compack/source/additm.for
	touch $(d0root)/compack/source/askhst.for
	touch $(d0root)/compack/source/askins.for
	touch $(d0root)/compack/source/asklst.for
	touch $(d0root)/compack/source/asknum.for
	touch $(d0root)/compack/source/askrls.for
	touch $(d0root)/compack/source/askrng.for
	touch $(d0root)/compack/source/askval.for
	touch $(d0root)/compack/source/askyno.for
	touch $(d0root)/compack/source/begbat.for
	touch $(d0root)/compack/source/broadc.for
	touch $(d0root)/compack/source/broast.for
	touch $(d0root)/compack/source/canmen.for
	touch $(d0root)/compack/source/chcols.for
	touch $(d0root)/compack/source/chkcom.for
	touch $(d0root)/compack/source/chkist.for
	touch $(d0root)/compack/source/chspac.for
	touch $(d0root)/compack/source/cmpk_gconfi.for
	touch $(d0root)/compack/source/confrm.for
	touch $(d0root)/compack/source/conhex.for
	touch $(d0root)/compack/source/conint.for
	touch $(d0root)/compack/source/conrea.for
	touch $(d0root)/compack/source/contry.for
	touch $(d0root)/compack/source/ctrast.for
	touch $(d0root)/compack/source/curlin.for
	touch $(d0root)/compack/source/curonf.for
	touch $(d0root)/compack/source/curso1.for
	touch $(d0root)/compack/source/cursor.for
	touch $(d0root)/compack/source/curtim.for
	touch $(d0root)/compack/source/defcol.for
	touch $(d0root)/compack/source/defspa.for
	touch $(d0root)/compack/source/diabox.for
	touch $(d0root)/compack/source/disabl.for
	touch $(d0root)/compack/source/display_items.for
	touch $(d0root)/compack/source/display_lines.for
	touch $(d0root)/compack/source/dmpmsg.for
	touch $(d0root)/compack/source/doproc.for
	touch $(d0root)/compack/source/drgrid.for
	touch $(d0root)/compack/source/drrect.for
	touch $(d0root)/compack/source/dumhdl.for
	touch $(d0root)/compack/source/endclr.for
	touch $(d0root)/compack/source/enddmp.for
	touch $(d0root)/compack/source/endlog.for
	touch $(d0root)/compack/source/endspl.for
	touch $(d0root)/compack/source/endsta.for
	touch $(d0root)/compack/source/erase.for
	touch $(d0root)/compack/source/evecom.for
	touch $(d0root)/compack/source/evefil.for
	touch $(d0root)/compack/source/evesav.for
	touch $(d0root)/compack/source/eximen.for
	touch $(d0root)/compack/source/exiusr.for
	touch $(d0root)/compack/source/fillst.for
	touch $(d0root)/compack/source/fndfil.for
	touch $(d0root)/compack/source/fulmod.for
	touch $(d0root)/compack/source/gcomnd.for
	touch $(d0root)/compack/source/gconfi.for
	touch $(d0root)/compack/source/getarr.for
	touch $(d0root)/compack/source/getdev.for
	touch $(d0root)/compack/source/getdis.for
	touch $(d0root)/compack/source/getdis77.for
	touch $(d0root)/compack/source/gethlp.for
	touch $(d0root)/compack/source/getopt.for
	touch $(d0root)/compack/source/getpar.for
	touch $(d0root)/compack/source/getpar77.for
	touch $(d0root)/compack/source/getscr.for
	touch $(d0root)/compack/source/getspl.for
	touch $(d0root)/compack/source/getsta.for
	touch $(d0root)/compack/source/getstr.for
	touch $(d0root)/compack/source/getsyi.for
	touch $(d0root)/compack/source/getwta.for
	touch $(d0root)/compack/source/gfulsc.for
	touch $(d0root)/compack/source/gincom.for
	touch $(d0root)/compack/source/gjourn.for
	touch $(d0root)/compack/source/gline0.for
	touch $(d0root)/compack/source/gline1.for
	touch $(d0root)/compack/source/gline2.for
	touch $(d0root)/compack/source/glines.for
	touch $(d0root)/compack/source/gnosmg.for
	touch $(d0root)/compack/source/gotop.for
	touch $(d0root)/compack/source/gpar_lindspl_inpt.for
	touch $(d0root)/compack/source/greads.for
	touch $(d0root)/compack/source/gsplit.for
	touch $(d0root)/compack/source/gstatu.for
	touch $(d0root)/compack/source/guserm.for
	touch $(d0root)/compack/source/helpme.for
	touch $(d0root)/compack/source/hlpget.for
	touch $(d0root)/compack/source/hlproc.for
	touch $(d0root)/compack/source/inpcmd.for
	touch $(d0root)/compack/source/inptrm.for
	touch $(d0root)/compack/source/intast.for
	touch $(d0root)/compack/source/intmen.for
	touch $(d0root)/compack/source/intmsg.for
	touch $(d0root)/compack/source/keyast.for
	touch $(d0root)/compack/source/keydef.for
	touch $(d0root)/compack/source/keyptr.for
	touch $(d0root)/compack/source/lenint.for
	touch $(d0root)/compack/source/libbig.for
	touch $(d0root)/compack/source/libbof.for
	touch $(d0root)/compack/source/libbon.for
	touch $(d0root)/compack/source/libcar.for
	touch $(d0root)/compack/source/libcr1.for
	touch $(d0root)/compack/source/libctm.for
	touch $(d0root)/compack/source/libcur.for
	touch $(d0root)/compack/source/libefc.for
	touch $(d0root)/compack/source/libemv.for
	touch $(d0root)/compack/source/libera.for
	touch $(d0root)/compack/source/liberl.for
	touch $(d0root)/compack/source/liberp.for
	touch $(d0root)/compack/source/libget.for
	touch $(d0root)/compack/source/libhmv.for
	touch $(d0root)/compack/source/libind.for
	touch $(d0root)/compack/source/libkey.for
	touch $(d0root)/compack/source/liblin.for
	touch $(d0root)/compack/source/libnxt.for
	touch $(d0root)/compack/source/libpmv.for
	touch $(d0root)/compack/source/libpst.for
	touch $(d0root)/compack/source/libput.for
	touch $(d0root)/compack/source/librep.for
	touch $(d0root)/compack/source/libres.for
	touch $(d0root)/compack/source/libsav.for
	touch $(d0root)/compack/source/libscr.for
	touch $(d0root)/compack/source/libsna.for
	touch $(d0root)/compack/source/libsta.for
	touch $(d0root)/compack/source/libstm.for
	touch $(d0root)/compack/source/libunp.for
	touch $(d0root)/compack/source/libuns.for
	touch $(d0root)/compack/source/linchk.for
	touch $(d0root)/compack/source/lines.for
	touch $(d0root)/compack/source/lines0.for
	touch $(d0root)/compack/source/lines1.for
	touch $(d0root)/compack/source/linget.for
	touch $(d0root)/compack/source/lisbox.for
	touch $(d0root)/compack/source/lspawn.for
	touch $(d0root)/compack/source/mclref.for
	touch $(d0root)/compack/source/menadd.for
	touch $(d0root)/compack/source/menctr.for
	touch $(d0root)/compack/source/mendef.for
	touch $(d0root)/compack/source/mendis.for
	touch $(d0root)/compack/source/menexg.for
	touch $(d0root)/compack/source/menmai.for
	touch $(d0root)/compack/source/menmod.for
	touch $(d0root)/compack/source/mennew.for
	touch $(d0root)/compack/source/menset.for
	touch $(d0root)/compack/source/mensub.for
	touch $(d0root)/compack/source/menudo.for
	touch $(d0root)/compack/source/menuef.for
	touch $(d0root)/compack/source/menuex.for
	touch $(d0root)/compack/source/menuop.for
	touch $(d0root)/compack/source/modify_params.for
	touch $(d0root)/compack/source/movint.for
	touch $(d0root)/compack/source/movprt.for
	touch $(d0root)/compack/source/movstr.for
	touch $(d0root)/compack/source/msetef.for
	touch $(d0root)/compack/source/msgscr.for
	touch $(d0root)/compack/source/mtimer.for
	touch $(d0root)/compack/source/newpar.for
	touch $(d0root)/compack/source/normal.for
	touch $(d0root)/compack/source/nulhdl.for
	touch $(d0root)/compack/source/oline0.for
	touch $(d0root)/compack/source/oline1.for
	touch $(d0root)/compack/source/olines.for
	touch $(d0root)/compack/source/ordset.for
	touch $(d0root)/compack/source/oreads.for
	touch $(d0root)/compack/source/outmsg.for
	touch $(d0root)/compack/source/outsav.for
	touch $(d0root)/compack/source/page.for
	touch $(d0root)/compack/source/pareal.for
	touch $(d0root)/compack/source/parint.for
	touch $(d0root)/compack/source/parlog.for
	touch $(d0root)/compack/source/parstr.for
	touch $(d0root)/compack/source/pfget.for
	touch $(d0root)/compack/source/pflabl.for
	touch $(d0root)/compack/source/pfnum.for
	touch $(d0root)/compack/source/pfwait.for
	touch $(d0root)/compack/source/prscom.for
	touch $(d0root)/compack/source/prtpbd.for
	touch $(d0root)/compack/source/putmsg.for
	touch $(d0root)/compack/source/qioast.for
	touch $(d0root)/compack/source/rdlog.for
	touch $(d0root)/compack/source/rdset.for
	touch $(d0root)/compack/source/readme.for
	touch $(d0root)/compack/source/readpf.for
	touch $(d0root)/compack/source/reasav.for
	touch $(d0root)/compack/source/reaset.for
	touch $(d0root)/compack/source/relmsg.for
	touch $(d0root)/compack/source/repscr.for
	touch $(d0root)/compack/source/restat.for
	touch $(d0root)/compack/source/runaux.for
	touch $(d0root)/compack/source/runcom.for
	touch $(d0root)/compack/source/runlog.for
	touch $(d0root)/compack/source/runset.for
	touch $(d0root)/compack/source/savscr.for
	touch $(d0root)/compack/source/scrolines.for
	touch $(d0root)/compack/source/set_split_screen.for
	touch $(d0root)/compack/source/setchk.for
	touch $(d0root)/compack/source/setcom.for
	touch $(d0root)/compack/source/setdir.for
	touch $(d0root)/compack/source/setdmp.for
	touch $(d0root)/compack/source/setflg.for
	touch $(d0root)/compack/source/setlin.for
	touch $(d0root)/compack/source/setmod.for
	touch $(d0root)/compack/source/setqio.for
	touch $(d0root)/compack/source/setsta.for
	touch $(d0root)/compack/source/spawit.for
	touch $(d0root)/compack/source/splsta.for
	touch $(d0root)/compack/source/spltit.for
	touch $(d0root)/compack/source/stadis.for
	touch $(d0root)/compack/source/stamsg.for
	touch $(d0root)/compack/source/strap1.for
	touch $(d0root)/compack/source/strapp.for
	touch $(d0root)/compack/source/strcop.for
	touch $(d0root)/compack/source/strdcp.for
	touch $(d0root)/compack/source/strfet.for
	touch $(d0root)/compack/source/strfre.for
	touch $(d0root)/compack/source/strsto.for
	touch $(d0root)/compack/source/sysdsp.for
	touch $(d0root)/compack/source/tabdis.for
	touch $(d0root)/compack/source/tabdisc.for
	touch $(d0root)/compack/source/tabdisn.for
	touch $(d0root)/compack/source/timast.for
	touch $(d0root)/compack/source/timdis.for
	touch $(d0root)/compack/source/tline0.for
	touch $(d0root)/compack/source/tline1.for
	touch $(d0root)/compack/source/tlines.for
	touch $(d0root)/compack/source/traint.for
	touch $(d0root)/compack/source/tralog.for
	touch $(d0root)/compack/source/trarea.for
	touch $(d0root)/compack/source/typage.for
	touch $(d0root)/compack/source/usrchk.for
	touch $(d0root)/compack/source/waibit.for
	touch $(d0root)/compack/source/waitit.for
	touch $(d0root)/compack/source/windis.for
