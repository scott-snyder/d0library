SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Olimit 2500 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/admin/tmp/d0librar/userlib/21798/dbl3
FLAVOR = SIUNIX
CC = cc
LEX = lex
F77 = f77
.IGNORE:
.SUFFIXES:
opt :\
  dbl3.a
dbl3.a : $(d0root)/dbl3/dbl3.a
	@ echo dbl3.a is up to date
$(d0root)/dbl3/dbl3.a:: $(SCRATCH)/nothing.nl
$(d0root)/dbl3/dbl3.a::\
  $(d0root)/dbl3/dbl3.a(dbabrd.o)\
  $(d0root)/dbl3/dbl3.a(dbabwr.o)\
  $(d0root)/dbl3/dbl3.a(dbacpl.o)\
  $(d0root)/dbl3/dbl3.a(dbacti.o)\
  $(d0root)/dbl3/dbl3.a(dbaird.o)\
  $(d0root)/dbl3/dbl3.a(dbaiwr.o)\
  $(d0root)/dbl3/dbl3.a(dbauxi.o)\
  $(d0root)/dbl3/dbl3.a(dbbook.o)\
  $(d0root)/dbl3/dbl3.a(dbcdic.o)\
  $(d0root)/dbl3/dbl3.a(dbcfri.o)\
  $(d0root)/dbl3/dbl3.a(dbchck.o)\
  $(d0root)/dbl3/dbl3.a(dbchfi.o)\
  $(d0root)/dbl3/dbl3.a(dbchky.o)\
  $(d0root)/dbl3/dbl3.a(dbclos.o)\
  $(d0root)/dbl3/dbl3.a(dbcmpr.o)\
  $(d0root)/dbl3/dbl3.a(dbcmpz.o)\
  $(d0root)/dbl3/dbl3.a(dbcomp.o)\
  $(d0root)/dbl3/dbl3.a(dbconc.o)\
  $(d0root)/dbl3/dbl3.a(dbcrdr.o)\
  $(d0root)/dbl3/dbl3.a(dbcrsd.o)\
  $(d0root)/dbl3/dbl3.a(dbctob.o)\
  $(d0root)/dbl3/dbl3.a(dbctoi.o)\
  $(d0root)/dbl3/dbl3.a(dbctor.o)\
  $(d0root)/dbl3/dbl3.a(dbdckh.o)\
  $(d0root)/dbl3/dbl3.a(dbdckv.o)\
  $(d0root)/dbl3/dbl3.a(dbdelk.o)\
  $(d0root)/dbl3/dbl3.a(dbdelt.o)\
  $(d0root)/dbl3/dbl3.a(dbdhea.o)\
  $(d0root)/dbl3/dbl3.a(dbdisd.o)\
  $(d0root)/dbl3/dbl3.a(dbdish.o)\
  $(d0root)/dbl3/dbl3.a(dbdisp.o)\
  $(d0root)/dbl3/dbl3.a(dbdisv.o)\
  $(d0root)/dbl3/dbl3.a(dbdkyh.o)\
  $(d0root)/dbl3/dbl3.a(dbdkyv.o)\
  $(d0root)/dbl3/dbl3.a(dbdont.o)\
  $(d0root)/dbl3/dbl3.a(dbdprg.o)\
  $(d0root)/dbl3/dbl3.a(dbeali.o)\
  $(d0root)/dbl3/dbl3.a(dbedas.o)\
  $(d0root)/dbl3/dbl3.a(dbedky.o)\
  $(d0root)/dbl3/dbl3.a(dbefor.o)\
  $(d0root)/dbl3/dbl3.a(dbehlp.o)\
  $(d0root)/dbl3/dbl3.a(dbenam.o)\
  $(d0root)/dbl3/dbl3.a(dbend.o)\
  $(d0root)/dbl3/dbl3.a(dbendf.o)\
  $(d0root)/dbl3/dbl3.a(dbenfz.o)\
  $(d0root)/dbl3/dbl3.a(dbentb.o)\
  $(d0root)/dbl3/dbl3.a(dbentr.o)\
  $(d0root)/dbl3/dbl3.a(dbflin.o)\
  $(d0root)/dbl3/dbl3.a(dbfpat.o)\
  $(d0root)/dbl3/dbl3.a(dbfree.o)\
  $(d0root)/dbl3/dbl3.a(dbfrst.o)\
  $(d0root)/dbl3/dbl3.a(dbfrus.o)\
  $(d0root)/dbl3/dbl3.a(dbftio.o)\
  $(d0root)/dbl3/dbl3.a(dbfzin.o)\
  $(d0root)/dbl3/dbl3.a(dbfzop.o)\
  $(d0root)/dbl3/dbl3.a(dbfzup.o)\
  $(d0root)/dbl3/dbl3.a(dbfzwr.o)\
  $(d0root)/dbl3/dbl3.a(dbget.o)\
  $(d0root)/dbl3/dbl3.a(dbgets.o)\
  $(d0root)/dbl3/dbl3.a(dbgnam.o)\
  $(d0root)/dbl3/dbl3.a(dbgpid.o)\
  $(d0root)/dbl3/dbl3.a(dbhunt.o)\
  $(d0root)/dbl3/dbl3.a(dbifch.o)\
  $(d0root)/dbl3/dbl3.a(dbifrc.o)\
  $(d0root)/dbl3/dbl3.a(dbildf.o)\
  $(d0root)/dbl3/dbl3.a(dbildu.o)\
  $(d0root)/dbl3/dbl3.a(dbin.o)\
  $(d0root)/dbl3/dbl3.a(dbinct.o)\
  $(d0root)/dbl3/dbl3.a(dbinin.o)\
  $(d0root)/dbl3/dbl3.a(dbinit.o)\
  $(d0root)/dbl3/dbl3.a(dbioty.o)\
  $(d0root)/dbl3/dbl3.a(dbizin.o)\
  $(d0root)/dbl3/dbl3.a(dbjoin.o)\
  $(d0root)/dbl3/dbl3.a(dbkept.o)\
  $(d0root)/dbl3/dbl3.a(dbkeyr.o)\
  $(d0root)/dbl3/dbl3.a(dbkeys.o)\
  $(d0root)/dbl3/dbl3.a(dbkeyt.o)\
  $(d0root)/dbl3/dbl3.a(dbkin.o)\
  $(d0root)/dbl3/dbl3.a(dbkout.o)\
  $(d0root)/dbl3/dbl3.a(dbktyp.o)\
  $(d0root)/dbl3/dbl3.a(dbkvin.o)\
  $(d0root)/dbl3/dbl3.a(dbkxin.o)\
  $(d0root)/dbl3/dbl3.a(dbkyse.o)\
  $(d0root)/dbl3/dbl3.a(dbkytg.o)\
  $(d0root)/dbl3/dbl3.a(dblast.o)\
  $(d0root)/dbl3/dbl3.a(dblinc.o)\
  $(d0root)/dbl3/dbl3.a(dblind.o)\
  $(d0root)/dbl3/dbl3.a(dblkey.o)\
  $(d0root)/dbl3/dbl3.a(dblmod.o)\
  $(d0root)/dbl3/dbl3.a(dblogl.o)\
  $(d0root)/dbl3/dbl3.a(dblook.o)\
  $(d0root)/dbl3/dbl3.a(dbmdip.o)\
  $(d0root)/dbl3/dbl3.a(dbmdir.o)\
  $(d0root)/dbl3/dbl3.a(dbnode.o)\
  $(d0root)/dbl3/dbl3.a(dbntop.o)\
  $(d0root)/dbl3/dbl3.a(dbopen.o)\
  $(d0root)/dbl3/dbl3.a(dbopts.o)\
  $(d0root)/dbl3/dbl3.a(dbout.o)\
  $(d0root)/dbl3/dbl3.a(dbpack.o)\
  $(d0root)/dbl3/dbl3.a(dbpath.o)\
  $(d0root)/dbl3/dbl3.a(dbpeek.o)\
  $(d0root)/dbl3/dbl3.a(dbpktm.o)\
  $(d0root)/dbl3/dbl3.a(dbpkts.o)\
  $(d0root)/dbl3/dbl3.a(dbplbk.o)\
  $(d0root)/dbl3/dbl3.a(dbplnt.o)\
  $(d0root)/dbl3/dbl3.a(dbplob.o)\
  $(d0root)/dbl3/dbl3.a(dbplov.o)\
  $(d0root)/dbl3/dbl3.a(dbplti.o)\
  $(d0root)/dbl3/dbl3.a(dbprdt.o)\
  $(d0root)/dbl3/dbl3.a(dbpres.o)\
  $(d0root)/dbl3/dbl3.a(dbprgd.o)\
  $(d0root)/dbl3/dbl3.a(dbprin.o)\
  $(d0root)/dbl3/dbl3.a(dbprky.o)\
  $(d0root)/dbl3/dbl3.a(dbprnt.o)\
  $(d0root)/dbl3/dbl3.a(dbproc.o)\
  $(d0root)/dbl3/dbl3.a(dbpurg.o)\
  $(d0root)/dbl3/dbl3.a(dbpurk.o)\
  $(d0root)/dbl3/dbl3.a(dbrali.o)\
  $(d0root)/dbl3/dbl3.a(dbrdda.o)\
  $(d0root)/dbl3/dbl3.a(dbrdio.o)\
  $(d0root)/dbl3/dbl3.a(dbrenk.o)\
  $(d0root)/dbl3/dbl3.a(dbrepl.o)\
  $(d0root)/dbl3/dbl3.a(dbrgck.o)\
  $(d0root)/dbl3/dbl3.a(dbrgcv.o)\
  $(d0root)/dbl3/dbl3.a(dbrhlp.o)\
  $(d0root)/dbl3/dbl3.a(dbrky1.o)\
  $(d0root)/dbl3/dbl3.a(dbrnam.o)\
  $(d0root)/dbl3/dbl3.a(dbropn.o)\
  $(d0root)/dbl3/dbl3.a(dbrtfz.o)\
  $(d0root)/dbl3/dbl3.a(dbrvnt.o)\
  $(d0root)/dbl3/dbl3.a(dbrvpl.o)\
  $(d0root)/dbl3/dbl3.a(dbrzin.o)\
  $(d0root)/dbl3/dbl3.a(dbsave.o)\
  $(d0root)/dbl3/dbl3.a(dbsblc.o)\
  $(d0root)/dbl3/dbl3.a(dbsdir.o)\
  $(d0root)/dbl3/dbl3.a(dbseky.o)\
  $(d0root)/dbl3/dbl3.a(dbsnam.o)\
  $(d0root)/dbl3/dbl3.a(dbsopn.o)\
  $(d0root)/dbl3/dbl3.a(dbspur.o)\
  $(d0root)/dbl3/dbl3.a(dbsrtm.o)\
  $(d0root)/dbl3/dbl3.a(dbtbcr.o)\
  $(d0root)/dbl3/dbl3.a(dbtbpr.o)\
  $(d0root)/dbl3/dbl3.a(dbtemp.o)\
  $(d0root)/dbl3/dbl3.a(dbtime.o)\
  $(d0root)/dbl3/dbl3.a(dbtopn.o)\
  $(d0root)/dbl3/dbl3.a(dbtous.o)\
  $(d0root)/dbl3/dbl3.a(dbucmp.o)\
  $(d0root)/dbl3/dbl3.a(dbucmz.o)\
  $(d0root)/dbl3/dbl3.a(dbudic.o)\
  $(d0root)/dbl3/dbl3.a(dbuncp.o)\
  $(d0root)/dbl3/dbl3.a(dbupck.o)\
  $(d0root)/dbl3/dbl3.a(dbupfz.o)\
  $(d0root)/dbl3/dbl3.a(dbupiz.o)\
  $(d0root)/dbl3/dbl3.a(dbupky.o)\
  $(d0root)/dbl3/dbl3.a(dbuptm.o)\
  $(d0root)/dbl3/dbl3.a(dbupts.o)\
  $(d0root)/dbl3/dbl3.a(dbuse.o)\
  $(d0root)/dbl3/dbl3.a(dbusin.o)\
  $(d0root)/dbl3/dbl3.a(dbutim.o)\
  $(d0root)/dbl3/dbl3.a(dbutis.o)\
  $(d0root)/dbl3/dbl3.a(dbuvtx.o)\
  $(d0root)/dbl3/dbl3.a(dbvhea.o)\
  $(d0root)/dbl3/dbl3.a(dbview.o)\
  $(d0root)/dbl3/dbl3.a(dbvin.o)\
  $(d0root)/dbl3/dbl3.a(dbvldt.o)\
  $(d0root)/dbl3/dbl3.a(dbvout.o)\
  $(d0root)/dbl3/dbl3.a(dbvwpr.o)\
  $(d0root)/dbl3/dbl3.a(dbwrdp.o)\
  $(d0root)/dbl3/dbl3.a(dbxini.o)\
  $(d0root)/dbl3/dbl3.a(idbtyp.o)
	ar $(ARFLAGS) $(d0root)/dbl3/dbl3.a `ls $(SCRATCH)/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/dbl3/dbl3.a `ls $(SCRATCH)/a2/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/dbl3/dbl3.a `ls $(SCRATCH)/a3/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/dbl3/dbl3.a `ls $(SCRATCH)/a4/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/dbl3/dbl3.a `ls $(SCRATCH)/a5/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/dbl3/dbl3.a `ls $(SCRATCH)/a6/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/dbl3/dbl3.a `ls $(SCRATCH)/a7/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/dbl3/dbl3.a `ls $(SCRATCH)/a8/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/dbl3/dbl3.a `ls $(SCRATCH)/a9/*.o 2> /dev/null`
	rmdirp $(SCRATCH)
$(SCRATCH)/nothing.nl:
	rmdirp $(SCRATCH)
	mkdirp $(SCRATCH)/a1
	mkdirp $(SCRATCH)/a2
	mkdirp $(SCRATCH)/a3
	mkdirp $(SCRATCH)/a4
	mkdirp $(SCRATCH)/a5
	mkdirp $(SCRATCH)/a6
	mkdirp $(SCRATCH)/a7
	mkdirp $(SCRATCH)/a8
	mkdirp $(SCRATCH)/a9
$(d0root)/dbl3/dbl3.a(dbabrd.o):\
  $(d0root)/dbl3/dbl3/dbabrd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbabrd.f ;\
	mv dbabrd.o $(SCRATCH)/a1/dbabrd.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbabwr.o):\
  $(d0root)/dbl3/dbl3/dbabwr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbabwr.f ;\
	mv dbabwr.o $(SCRATCH)/a1/dbabwr.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbacpl.o):\
  $(d0root)/dbl3/dbl3/dbacpl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbacpl.f ;\
	mv dbacpl.o $(SCRATCH)/a1/dbacpl.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbacti.o):\
  $(d0root)/dbl3/dbl3/dbacti.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbacti.f ;\
	mv dbacti.o $(SCRATCH)/a1/dbacti.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbaird.o):\
  $(d0root)/dbl3/dbl3/dbaird.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbaird.f ;\
	mv dbaird.o $(SCRATCH)/a1/dbaird.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbaiwr.o):\
  $(d0root)/dbl3/dbl3/dbaiwr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbaiwr.f ;\
	mv dbaiwr.o $(SCRATCH)/a1/dbaiwr.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbauxi.o):\
  $(d0root)/dbl3/dbl3/dbauxi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbauxi.f ;\
	mv dbauxi.o $(SCRATCH)/a1/dbauxi.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbbook.o):\
  $(d0root)/dbl3/dbl3/dbbook.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbbook.f ;\
	mv dbbook.o $(SCRATCH)/a1/dbbook.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbcdic.o):\
  $(d0root)/dbl3/dbl3/dbcdic.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbcdic.f ;\
	mv dbcdic.o $(SCRATCH)/a1/dbcdic.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbcfri.o):\
  $(d0root)/dbl3/dbl3/dbcfri.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbcfri.f ;\
	mv dbcfri.o $(SCRATCH)/a1/dbcfri.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbchck.o):\
  $(d0root)/dbl3/dbl3/dbchck.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbchck.f ;\
	mv dbchck.o $(SCRATCH)/a1/dbchck.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbchfi.o):\
  $(d0root)/dbl3/dbl3/dbchfi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbchfi.f ;\
	mv dbchfi.o $(SCRATCH)/a1/dbchfi.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbchky.o):\
  $(d0root)/dbl3/dbl3/dbchky.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbchky.f ;\
	mv dbchky.o $(SCRATCH)/a1/dbchky.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbclos.o):\
  $(d0root)/dbl3/dbl3/dbclos.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbclos.f ;\
	mv dbclos.o $(SCRATCH)/a1/dbclos.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbcmpr.o):\
  $(d0root)/dbl3/dbl3/dbcmpr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbcmpr.f ;\
	mv dbcmpr.o $(SCRATCH)/a1/dbcmpr.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbcmpz.o):\
  $(d0root)/dbl3/dbl3/dbcmpz.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbcmpz.f ;\
	mv dbcmpz.o $(SCRATCH)/a1/dbcmpz.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbcomp.o):\
  $(d0root)/dbl3/dbl3/dbcomp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbcomp.f ;\
	mv dbcomp.o $(SCRATCH)/a1/dbcomp.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbconc.o):\
  $(d0root)/dbl3/dbl3/dbconc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbconc.f ;\
	mv dbconc.o $(SCRATCH)/a1/dbconc.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbcrdr.o):\
  $(d0root)/dbl3/dbl3/dbcrdr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbcrdr.f ;\
	mv dbcrdr.o $(SCRATCH)/a1/dbcrdr.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbcrsd.o):\
  $(d0root)/dbl3/dbl3/dbcrsd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbcrsd.f ;\
	mv dbcrsd.o $(SCRATCH)/a1/dbcrsd.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbctob.o):\
  $(d0root)/dbl3/dbl3/dbctob.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbctob.f ;\
	mv dbctob.o $(SCRATCH)/a2/dbctob.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbctoi.o):\
  $(d0root)/dbl3/dbl3/dbctoi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbctoi.f ;\
	mv dbctoi.o $(SCRATCH)/a2/dbctoi.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbctor.o):\
  $(d0root)/dbl3/dbl3/dbctor.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbctor.f ;\
	mv dbctor.o $(SCRATCH)/a2/dbctor.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbdckh.o):\
  $(d0root)/dbl3/dbl3/dbdckh.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbdckh.f ;\
	mv dbdckh.o $(SCRATCH)/a2/dbdckh.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbdckv.o):\
  $(d0root)/dbl3/dbl3/dbdckv.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbdckv.f ;\
	mv dbdckv.o $(SCRATCH)/a2/dbdckv.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbdelk.o):\
  $(d0root)/dbl3/dbl3/dbdelk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbdelk.f ;\
	mv dbdelk.o $(SCRATCH)/a2/dbdelk.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbdelt.o):\
  $(d0root)/dbl3/dbl3/dbdelt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbdelt.f ;\
	mv dbdelt.o $(SCRATCH)/a2/dbdelt.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbdhea.o):\
  $(d0root)/dbl3/dbl3/dbdhea.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbdhea.f ;\
	mv dbdhea.o $(SCRATCH)/a2/dbdhea.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbdisd.o):\
  $(d0root)/dbl3/dbl3/dbdisd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbdisd.f ;\
	mv dbdisd.o $(SCRATCH)/a2/dbdisd.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbdish.o):\
  $(d0root)/dbl3/dbl3/dbdish.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbdish.f ;\
	mv dbdish.o $(SCRATCH)/a2/dbdish.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbdisp.o):\
  $(d0root)/dbl3/dbl3/dbdisp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbdisp.f ;\
	mv dbdisp.o $(SCRATCH)/a2/dbdisp.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbdisv.o):\
  $(d0root)/dbl3/dbl3/dbdisv.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbdisv.f ;\
	mv dbdisv.o $(SCRATCH)/a2/dbdisv.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbdkyh.o):\
  $(d0root)/dbl3/dbl3/dbdkyh.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbdkyh.f ;\
	mv dbdkyh.o $(SCRATCH)/a2/dbdkyh.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbdkyv.o):\
  $(d0root)/dbl3/dbl3/dbdkyv.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbdkyv.f ;\
	mv dbdkyv.o $(SCRATCH)/a2/dbdkyv.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbdont.o):\
  $(d0root)/dbl3/dbl3/dbdont.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbdont.f ;\
	mv dbdont.o $(SCRATCH)/a2/dbdont.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbdprg.o):\
  $(d0root)/dbl3/dbl3/dbdprg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbdprg.f ;\
	mv dbdprg.o $(SCRATCH)/a2/dbdprg.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbeali.o):\
  $(d0root)/dbl3/dbl3/dbeali.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbeali.f ;\
	mv dbeali.o $(SCRATCH)/a2/dbeali.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbedas.o):\
  $(d0root)/dbl3/dbl3/dbedas.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbedas.f ;\
	mv dbedas.o $(SCRATCH)/a2/dbedas.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbedky.o):\
  $(d0root)/dbl3/dbl3/dbedky.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbedky.f ;\
	mv dbedky.o $(SCRATCH)/a2/dbedky.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbefor.o):\
  $(d0root)/dbl3/dbl3/dbefor.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbefor.f ;\
	mv dbefor.o $(SCRATCH)/a2/dbefor.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbehlp.o):\
  $(d0root)/dbl3/dbl3/dbehlp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbehlp.f ;\
	mv dbehlp.o $(SCRATCH)/a3/dbehlp.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbenam.o):\
  $(d0root)/dbl3/dbl3/dbenam.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbenam.f ;\
	mv dbenam.o $(SCRATCH)/a3/dbenam.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbend.o):\
  $(d0root)/dbl3/dbl3/dbend.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbend.f ;\
	mv dbend.o $(SCRATCH)/a3/dbend.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbendf.o):\
  $(d0root)/dbl3/dbl3/dbendf.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbendf.f ;\
	mv dbendf.o $(SCRATCH)/a3/dbendf.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbenfz.o):\
  $(d0root)/dbl3/dbl3/dbenfz.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbenfz.f ;\
	mv dbenfz.o $(SCRATCH)/a3/dbenfz.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbentb.o):\
  $(d0root)/dbl3/dbl3/dbentb.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbentb.f ;\
	mv dbentb.o $(SCRATCH)/a3/dbentb.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbentr.o):\
  $(d0root)/dbl3/dbl3/dbentr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbentr.f ;\
	mv dbentr.o $(SCRATCH)/a3/dbentr.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbflin.o):\
  $(d0root)/dbl3/dbl3/dbflin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbflin.f ;\
	mv dbflin.o $(SCRATCH)/a3/dbflin.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbfpat.o):\
  $(d0root)/dbl3/dbl3/dbfpat.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbfpat.f ;\
	mv dbfpat.o $(SCRATCH)/a3/dbfpat.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbfree.o):\
  $(d0root)/dbl3/dbl3/dbfree.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbfree.f ;\
	mv dbfree.o $(SCRATCH)/a3/dbfree.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbfrst.o):\
  $(d0root)/dbl3/dbl3/dbfrst.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbfrst.f ;\
	mv dbfrst.o $(SCRATCH)/a3/dbfrst.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbfrus.o):\
  $(d0root)/dbl3/dbl3/dbfrus.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbfrus.f ;\
	mv dbfrus.o $(SCRATCH)/a3/dbfrus.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbftio.o):\
  $(d0root)/dbl3/dbl3/dbftio.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbftio.f ;\
	mv dbftio.o $(SCRATCH)/a3/dbftio.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbfzin.o):\
  $(d0root)/dbl3/dbl3/dbfzin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbfzin.f ;\
	mv dbfzin.o $(SCRATCH)/a3/dbfzin.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbfzop.o):\
  $(d0root)/dbl3/dbl3/dbfzop.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbfzop.f ;\
	mv dbfzop.o $(SCRATCH)/a3/dbfzop.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbfzup.o):\
  $(d0root)/dbl3/dbl3/dbfzup.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbfzup.f ;\
	mv dbfzup.o $(SCRATCH)/a3/dbfzup.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbfzwr.o):\
  $(d0root)/dbl3/dbl3/dbfzwr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbfzwr.f ;\
	mv dbfzwr.o $(SCRATCH)/a3/dbfzwr.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbget.o):\
  $(d0root)/dbl3/dbl3/dbget.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbget.f ;\
	mv dbget.o $(SCRATCH)/a3/dbget.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbgets.o):\
  $(d0root)/dbl3/dbl3/dbgets.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbgets.f ;\
	mv dbgets.o $(SCRATCH)/a3/dbgets.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbgnam.o):\
  $(d0root)/dbl3/dbl3/dbgnam.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbgnam.f ;\
	mv dbgnam.o $(SCRATCH)/a3/dbgnam.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbgpid.o):\
  $(d0root)/dbl3/dbl3/dbgpid.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbgpid.f ;\
	mv dbgpid.o $(SCRATCH)/a4/dbgpid.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbhunt.o):\
  $(d0root)/dbl3/dbl3/dbhunt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbhunt.f ;\
	mv dbhunt.o $(SCRATCH)/a4/dbhunt.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbifch.o):\
  $(d0root)/dbl3/dbl3/dbifch.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbifch.f ;\
	mv dbifch.o $(SCRATCH)/a4/dbifch.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbifrc.o):\
  $(d0root)/dbl3/dbl3/dbifrc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbifrc.f ;\
	mv dbifrc.o $(SCRATCH)/a4/dbifrc.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbildf.o):\
  $(d0root)/dbl3/dbl3/dbildf.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbildf.f ;\
	mv dbildf.o $(SCRATCH)/a4/dbildf.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbildu.o):\
  $(d0root)/dbl3/dbl3/dbildu.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbildu.f ;\
	mv dbildu.o $(SCRATCH)/a4/dbildu.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbin.o):\
  $(d0root)/dbl3/dbl3/dbin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbin.f ;\
	mv dbin.o $(SCRATCH)/a4/dbin.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbinct.o):\
  $(d0root)/dbl3/dbl3/dbinct.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbinct.f ;\
	mv dbinct.o $(SCRATCH)/a4/dbinct.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbinin.o):\
  $(d0root)/dbl3/dbl3/dbinin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbinin.f ;\
	mv dbinin.o $(SCRATCH)/a4/dbinin.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbinit.o):\
  $(d0root)/dbl3/dbl3/dbinit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbinit.f ;\
	mv dbinit.o $(SCRATCH)/a4/dbinit.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbioty.o):\
  $(d0root)/dbl3/dbl3/dbioty.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbioty.f ;\
	mv dbioty.o $(SCRATCH)/a4/dbioty.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbizin.o):\
  $(d0root)/dbl3/dbl3/dbizin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbizin.f ;\
	mv dbizin.o $(SCRATCH)/a4/dbizin.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbjoin.o):\
  $(d0root)/dbl3/dbl3/dbjoin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbjoin.f ;\
	mv dbjoin.o $(SCRATCH)/a4/dbjoin.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbkept.o):\
  $(d0root)/dbl3/dbl3/dbkept.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbkept.f ;\
	mv dbkept.o $(SCRATCH)/a4/dbkept.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbkeyr.o):\
  $(d0root)/dbl3/dbl3/dbkeyr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbkeyr.f ;\
	mv dbkeyr.o $(SCRATCH)/a4/dbkeyr.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbkeys.o):\
  $(d0root)/dbl3/dbl3/dbkeys.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbkeys.f ;\
	mv dbkeys.o $(SCRATCH)/a4/dbkeys.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbkeyt.o):\
  $(d0root)/dbl3/dbl3/dbkeyt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbkeyt.f ;\
	mv dbkeyt.o $(SCRATCH)/a4/dbkeyt.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbkin.o):\
  $(d0root)/dbl3/dbl3/dbkin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbkin.f ;\
	mv dbkin.o $(SCRATCH)/a4/dbkin.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbkout.o):\
  $(d0root)/dbl3/dbl3/dbkout.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbkout.f ;\
	mv dbkout.o $(SCRATCH)/a4/dbkout.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbktyp.o):\
  $(d0root)/dbl3/dbl3/dbktyp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbktyp.f ;\
	mv dbktyp.o $(SCRATCH)/a4/dbktyp.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbkvin.o):\
  $(d0root)/dbl3/dbl3/dbkvin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbkvin.f ;\
	mv dbkvin.o $(SCRATCH)/a5/dbkvin.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbkxin.o):\
  $(d0root)/dbl3/dbl3/dbkxin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbkxin.f ;\
	mv dbkxin.o $(SCRATCH)/a5/dbkxin.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbkyse.o):\
  $(d0root)/dbl3/dbl3/dbkyse.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbkyse.f ;\
	mv dbkyse.o $(SCRATCH)/a5/dbkyse.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbkytg.o):\
  $(d0root)/dbl3/dbl3/dbkytg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbkytg.f ;\
	mv dbkytg.o $(SCRATCH)/a5/dbkytg.o ;\
	)
$(d0root)/dbl3/dbl3.a(dblast.o):\
  $(d0root)/dbl3/dbl3/dblast.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dblast.f ;\
	mv dblast.o $(SCRATCH)/a5/dblast.o ;\
	)
$(d0root)/dbl3/dbl3.a(dblinc.o):\
  $(d0root)/dbl3/dbl3/dblinc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dblinc.f ;\
	mv dblinc.o $(SCRATCH)/a5/dblinc.o ;\
	)
$(d0root)/dbl3/dbl3.a(dblind.o):\
  $(d0root)/dbl3/dbl3/dblind.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dblind.f ;\
	mv dblind.o $(SCRATCH)/a5/dblind.o ;\
	)
$(d0root)/dbl3/dbl3.a(dblkey.o):\
  $(d0root)/dbl3/dbl3/dblkey.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dblkey.f ;\
	mv dblkey.o $(SCRATCH)/a5/dblkey.o ;\
	)
$(d0root)/dbl3/dbl3.a(dblmod.o):\
  $(d0root)/dbl3/dbl3/dblmod.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dblmod.f ;\
	mv dblmod.o $(SCRATCH)/a5/dblmod.o ;\
	)
$(d0root)/dbl3/dbl3.a(dblogl.o):\
  $(d0root)/dbl3/dbl3/dblogl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dblogl.f ;\
	mv dblogl.o $(SCRATCH)/a5/dblogl.o ;\
	)
$(d0root)/dbl3/dbl3.a(dblook.o):\
  $(d0root)/dbl3/dbl3/dblook.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dblook.f ;\
	mv dblook.o $(SCRATCH)/a5/dblook.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbmdip.o):\
  $(d0root)/dbl3/dbl3/dbmdip.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbmdip.f ;\
	mv dbmdip.o $(SCRATCH)/a5/dbmdip.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbmdir.o):\
  $(d0root)/dbl3/dbl3/dbmdir.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbmdir.f ;\
	mv dbmdir.o $(SCRATCH)/a5/dbmdir.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbnode.o):\
  $(d0root)/dbl3/dbl3/dbnode.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbnode.f ;\
	mv dbnode.o $(SCRATCH)/a5/dbnode.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbntop.o):\
  $(d0root)/dbl3/dbl3/dbntop.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbntop.f ;\
	mv dbntop.o $(SCRATCH)/a5/dbntop.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbopen.o):\
  $(d0root)/dbl3/dbl3/dbopen.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbopen.f ;\
	mv dbopen.o $(SCRATCH)/a5/dbopen.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbopts.o):\
  $(d0root)/dbl3/dbl3/dbopts.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbopts.f ;\
	mv dbopts.o $(SCRATCH)/a5/dbopts.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbout.o):\
  $(d0root)/dbl3/dbl3/dbout.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbout.f ;\
	mv dbout.o $(SCRATCH)/a5/dbout.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbpack.o):\
  $(d0root)/dbl3/dbl3/dbpack.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbpack.f ;\
	mv dbpack.o $(SCRATCH)/a5/dbpack.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbpath.o):\
  $(d0root)/dbl3/dbl3/dbpath.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbpath.f ;\
	mv dbpath.o $(SCRATCH)/a5/dbpath.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbpeek.o):\
  $(d0root)/dbl3/dbl3/dbpeek.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbpeek.f ;\
	mv dbpeek.o $(SCRATCH)/a6/dbpeek.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbpktm.o):\
  $(d0root)/dbl3/dbl3/dbpktm.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbpktm.f ;\
	mv dbpktm.o $(SCRATCH)/a6/dbpktm.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbpkts.o):\
  $(d0root)/dbl3/dbl3/dbpkts.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbpkts.f ;\
	mv dbpkts.o $(SCRATCH)/a6/dbpkts.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbplbk.o):\
  $(d0root)/dbl3/dbl3/dbplbk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbplbk.f ;\
	mv dbplbk.o $(SCRATCH)/a6/dbplbk.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbplnt.o):\
  $(d0root)/dbl3/dbl3/dbplnt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbplnt.f ;\
	mv dbplnt.o $(SCRATCH)/a6/dbplnt.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbplob.o):\
  $(d0root)/dbl3/dbl3/dbplob.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbplob.f ;\
	mv dbplob.o $(SCRATCH)/a6/dbplob.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbplov.o):\
  $(d0root)/dbl3/dbl3/dbplov.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbplov.f ;\
	mv dbplov.o $(SCRATCH)/a6/dbplov.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbplti.o):\
  $(d0root)/dbl3/dbl3/dbplti.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbplti.f ;\
	mv dbplti.o $(SCRATCH)/a6/dbplti.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbprdt.o):\
  $(d0root)/dbl3/dbl3/dbprdt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbprdt.f ;\
	mv dbprdt.o $(SCRATCH)/a6/dbprdt.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbpres.o):\
  $(d0root)/dbl3/dbl3/dbpres.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbpres.f ;\
	mv dbpres.o $(SCRATCH)/a6/dbpres.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbprgd.o):\
  $(d0root)/dbl3/dbl3/dbprgd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbprgd.f ;\
	mv dbprgd.o $(SCRATCH)/a6/dbprgd.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbprin.o):\
  $(d0root)/dbl3/dbl3/dbprin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbprin.f ;\
	mv dbprin.o $(SCRATCH)/a6/dbprin.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbprky.o):\
  $(d0root)/dbl3/dbl3/dbprky.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbprky.f ;\
	mv dbprky.o $(SCRATCH)/a6/dbprky.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbprnt.o):\
  $(d0root)/dbl3/dbl3/dbprnt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbprnt.f ;\
	mv dbprnt.o $(SCRATCH)/a6/dbprnt.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbproc.o):\
  $(d0root)/dbl3/dbl3/dbproc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbproc.f ;\
	mv dbproc.o $(SCRATCH)/a6/dbproc.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbpurg.o):\
  $(d0root)/dbl3/dbl3/dbpurg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbpurg.f ;\
	mv dbpurg.o $(SCRATCH)/a6/dbpurg.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbpurk.o):\
  $(d0root)/dbl3/dbl3/dbpurk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbpurk.f ;\
	mv dbpurk.o $(SCRATCH)/a6/dbpurk.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbrali.o):\
  $(d0root)/dbl3/dbl3/dbrali.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbrali.f ;\
	mv dbrali.o $(SCRATCH)/a6/dbrali.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbrdda.o):\
  $(d0root)/dbl3/dbl3/dbrdda.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbrdda.f ;\
	mv dbrdda.o $(SCRATCH)/a6/dbrdda.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbrdio.o):\
  $(d0root)/dbl3/dbl3/dbrdio.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbrdio.f ;\
	mv dbrdio.o $(SCRATCH)/a6/dbrdio.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbrenk.o):\
  $(d0root)/dbl3/dbl3/dbrenk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbrenk.f ;\
	mv dbrenk.o $(SCRATCH)/a7/dbrenk.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbrepl.o):\
  $(d0root)/dbl3/dbl3/dbrepl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbrepl.f ;\
	mv dbrepl.o $(SCRATCH)/a7/dbrepl.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbrgck.o):\
  $(d0root)/dbl3/dbl3/dbrgck.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbrgck.f ;\
	mv dbrgck.o $(SCRATCH)/a7/dbrgck.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbrgcv.o):\
  $(d0root)/dbl3/dbl3/dbrgcv.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbrgcv.f ;\
	mv dbrgcv.o $(SCRATCH)/a7/dbrgcv.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbrhlp.o):\
  $(d0root)/dbl3/dbl3/dbrhlp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbrhlp.f ;\
	mv dbrhlp.o $(SCRATCH)/a7/dbrhlp.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbrky1.o):\
  $(d0root)/dbl3/dbl3/dbrky1.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbrky1.f ;\
	mv dbrky1.o $(SCRATCH)/a7/dbrky1.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbrnam.o):\
  $(d0root)/dbl3/dbl3/dbrnam.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbrnam.f ;\
	mv dbrnam.o $(SCRATCH)/a7/dbrnam.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbropn.o):\
  $(d0root)/dbl3/dbl3/dbropn.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbropn.f ;\
	mv dbropn.o $(SCRATCH)/a7/dbropn.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbrtfz.o):\
  $(d0root)/dbl3/dbl3/dbrtfz.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbrtfz.f ;\
	mv dbrtfz.o $(SCRATCH)/a7/dbrtfz.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbrvnt.o):\
  $(d0root)/dbl3/dbl3/dbrvnt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbrvnt.f ;\
	mv dbrvnt.o $(SCRATCH)/a7/dbrvnt.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbrvpl.o):\
  $(d0root)/dbl3/dbl3/dbrvpl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbrvpl.f ;\
	mv dbrvpl.o $(SCRATCH)/a7/dbrvpl.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbrzin.o):\
  $(d0root)/dbl3/dbl3/dbrzin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbrzin.f ;\
	mv dbrzin.o $(SCRATCH)/a7/dbrzin.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbsave.o):\
  $(d0root)/dbl3/dbl3/dbsave.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbsave.f ;\
	mv dbsave.o $(SCRATCH)/a7/dbsave.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbsblc.o):\
  $(d0root)/dbl3/dbl3/dbsblc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbsblc.f ;\
	mv dbsblc.o $(SCRATCH)/a7/dbsblc.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbsdir.o):\
  $(d0root)/dbl3/dbl3/dbsdir.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbsdir.f ;\
	mv dbsdir.o $(SCRATCH)/a7/dbsdir.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbseky.o):\
  $(d0root)/dbl3/dbl3/dbseky.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbseky.f ;\
	mv dbseky.o $(SCRATCH)/a7/dbseky.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbsnam.o):\
  $(d0root)/dbl3/dbl3/dbsnam.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbsnam.f ;\
	mv dbsnam.o $(SCRATCH)/a7/dbsnam.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbsopn.o):\
  $(d0root)/dbl3/dbl3/dbsopn.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbsopn.f ;\
	mv dbsopn.o $(SCRATCH)/a7/dbsopn.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbspur.o):\
  $(d0root)/dbl3/dbl3/dbspur.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbspur.f ;\
	mv dbspur.o $(SCRATCH)/a7/dbspur.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbsrtm.o):\
  $(d0root)/dbl3/dbl3/dbsrtm.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbsrtm.f ;\
	mv dbsrtm.o $(SCRATCH)/a7/dbsrtm.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbtbcr.o):\
  $(d0root)/dbl3/dbl3/dbtbcr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbtbcr.f ;\
	mv dbtbcr.o $(SCRATCH)/a8/dbtbcr.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbtbpr.o):\
  $(d0root)/dbl3/dbl3/dbtbpr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbtbpr.f ;\
	mv dbtbpr.o $(SCRATCH)/a8/dbtbpr.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbtemp.o):\
  $(d0root)/dbl3/dbl3/dbtemp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbtemp.f ;\
	mv dbtemp.o $(SCRATCH)/a8/dbtemp.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbtime.o):\
  $(d0root)/dbl3/dbl3/dbtime.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbtime.f ;\
	mv dbtime.o $(SCRATCH)/a8/dbtime.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbtopn.o):\
  $(d0root)/dbl3/dbl3/dbtopn.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbtopn.f ;\
	mv dbtopn.o $(SCRATCH)/a8/dbtopn.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbtous.o):\
  $(d0root)/dbl3/dbl3/dbtous.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbtous.f ;\
	mv dbtous.o $(SCRATCH)/a8/dbtous.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbucmp.o):\
  $(d0root)/dbl3/dbl3/dbucmp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbucmp.f ;\
	mv dbucmp.o $(SCRATCH)/a8/dbucmp.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbucmz.o):\
  $(d0root)/dbl3/dbl3/dbucmz.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbucmz.f ;\
	mv dbucmz.o $(SCRATCH)/a8/dbucmz.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbudic.o):\
  $(d0root)/dbl3/dbl3/dbudic.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbudic.f ;\
	mv dbudic.o $(SCRATCH)/a8/dbudic.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbuncp.o):\
  $(d0root)/dbl3/dbl3/dbuncp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbuncp.f ;\
	mv dbuncp.o $(SCRATCH)/a8/dbuncp.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbupck.o):\
  $(d0root)/dbl3/dbl3/dbupck.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbupck.f ;\
	mv dbupck.o $(SCRATCH)/a8/dbupck.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbupfz.o):\
  $(d0root)/dbl3/dbl3/dbupfz.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbupfz.f ;\
	mv dbupfz.o $(SCRATCH)/a8/dbupfz.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbupiz.o):\
  $(d0root)/dbl3/dbl3/dbupiz.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbupiz.f ;\
	mv dbupiz.o $(SCRATCH)/a8/dbupiz.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbupky.o):\
  $(d0root)/dbl3/dbl3/dbupky.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbupky.f ;\
	mv dbupky.o $(SCRATCH)/a8/dbupky.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbuptm.o):\
  $(d0root)/dbl3/dbl3/dbuptm.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbuptm.f ;\
	mv dbuptm.o $(SCRATCH)/a8/dbuptm.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbupts.o):\
  $(d0root)/dbl3/dbl3/dbupts.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbupts.f ;\
	mv dbupts.o $(SCRATCH)/a8/dbupts.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbuse.o):\
  $(d0root)/dbl3/dbl3/dbuse.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbuse.f ;\
	mv dbuse.o $(SCRATCH)/a8/dbuse.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbusin.o):\
  $(d0root)/dbl3/dbl3/dbusin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbusin.f ;\
	mv dbusin.o $(SCRATCH)/a8/dbusin.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbutim.o):\
  $(d0root)/dbl3/dbl3/dbutim.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbutim.f ;\
	mv dbutim.o $(SCRATCH)/a8/dbutim.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbutis.o):\
  $(d0root)/dbl3/dbl3/dbutis.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbutis.f ;\
	mv dbutis.o $(SCRATCH)/a8/dbutis.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbuvtx.o):\
  $(d0root)/dbl3/dbl3/dbuvtx.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbuvtx.f ;\
	mv dbuvtx.o $(SCRATCH)/a9/dbuvtx.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbvhea.o):\
  $(d0root)/dbl3/dbl3/dbvhea.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbvhea.f ;\
	mv dbvhea.o $(SCRATCH)/a9/dbvhea.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbview.o):\
  $(d0root)/dbl3/dbl3/dbview.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbview.f ;\
	mv dbview.o $(SCRATCH)/a9/dbview.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbvin.o):\
  $(d0root)/dbl3/dbl3/dbvin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbvin.f ;\
	mv dbvin.o $(SCRATCH)/a9/dbvin.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbvldt.o):\
  $(d0root)/dbl3/dbl3/dbvldt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbvldt.f ;\
	mv dbvldt.o $(SCRATCH)/a9/dbvldt.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbvout.o):\
  $(d0root)/dbl3/dbl3/dbvout.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbvout.f ;\
	mv dbvout.o $(SCRATCH)/a9/dbvout.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbvwpr.o):\
  $(d0root)/dbl3/dbl3/dbvwpr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbvwpr.f ;\
	mv dbvwpr.o $(SCRATCH)/a9/dbvwpr.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbwrdp.o):\
  $(d0root)/dbl3/dbl3/dbwrdp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbwrdp.f ;\
	mv dbwrdp.o $(SCRATCH)/a9/dbwrdp.o ;\
	)
$(d0root)/dbl3/dbl3.a(dbxini.o):\
  $(d0root)/dbl3/dbl3/dbxini.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/dbxini.f ;\
	mv dbxini.o $(SCRATCH)/a9/dbxini.o ;\
	)
$(d0root)/dbl3/dbl3.a(idbtyp.o):\
  $(d0root)/dbl3/dbl3/idbtyp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) dbl3/dbl3/idbtyp.f ;\
	mv idbtyp.o $(SCRATCH)/a9/idbtyp.o ;\
	)
pre:
for:
