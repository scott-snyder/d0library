SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0gamma)/unix/source -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0gamma)/unix/source -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Nn15000 -Wb,-force_branch_fixup -Olimit 3000 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/tmp/d0librar/userlib/13768/dbl3
FLAVOR = SIUNIX
CC = cc
LEX = lex
F77 = f77
.IGNORE:
.SUFFIXES:
opt :\
  dbl3.a
dbl3.a : $(d0root)/test/dbl3/dbl3.a
	@ echo dbl3.a is up to date
$(d0root)/test/dbl3/dbl3.a:: $(SCRATCH)/nothing.nl
$(d0root)/test/dbl3/dbl3.a::\
  $(d0root)/test/dbl3/dbl3.a(xdbabrd.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbabwr.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbacpl.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbacti.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbaird.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbaiwr.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbauxi.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbbook.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbcdic.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbcfri.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbchck.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbchfi.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbchky.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbclos.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbcmpr.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbcmpz.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbcomp.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbconc.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbcrdr.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbcrsd.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbctob.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbctoi.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbctor.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbdckh.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbdckv.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbdelk.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbdelt.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbdhea.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbdisd.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbdish.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbdisp.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbdisv.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbdkyh.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbdkyv.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbdont.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbdprg.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbeali.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbedas.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbedky.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbefor.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbehlp.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbenam.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbend.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbendf.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbenfz.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbentb.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbentr.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbflin.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbfpat.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbfree.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbfrst.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbfrus.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbftio.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbfzin.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbfzop.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbfzup.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbfzwr.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbget.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbgets.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbgnam.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbgpid.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbhunt.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbifch.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbifrc.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbildf.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbildu.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbin.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbinct.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbinin.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbinit.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbioty.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbizin.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbjoin.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbkept.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbkeyr.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbkeys.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbkeyt.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbkin.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbkout.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbktyp.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbkvin.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbkxin.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbkyse.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbkytg.o)\
  $(d0root)/test/dbl3/dbl3.a(xdblast.o)\
  $(d0root)/test/dbl3/dbl3.a(xdblinc.o)\
  $(d0root)/test/dbl3/dbl3.a(xdblind.o)\
  $(d0root)/test/dbl3/dbl3.a(xdblkey.o)\
  $(d0root)/test/dbl3/dbl3.a(xdblmod.o)\
  $(d0root)/test/dbl3/dbl3.a(xdblogl.o)\
  $(d0root)/test/dbl3/dbl3.a(xdblook.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbmdip.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbmdir.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbnode.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbntop.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbopen.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbopts.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbout.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbpack.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbpath.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbpeek.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbpktm.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbpkts.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbplbk.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbplnt.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbplob.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbplov.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbplti.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbprdt.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbpres.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbprgd.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbprin.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbprky.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbprnt.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbproc.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbpurg.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbpurk.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbrali.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbrdda.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbrdio.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbrenk.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbrepl.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbrgck.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbrgcv.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbrhlp.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbrky1.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbrnam.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbropn.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbrtfz.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbrvnt.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbrvpl.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbrzin.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbsave.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbsblc.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbsdir.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbseky.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbsnam.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbsopn.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbspur.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbsrtm.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbtbcr.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbtbpr.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbtemp.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbtime.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbtopn.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbtous.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbucmp.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbucmz.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbudic.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbuncp.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbupck.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbupfz.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbupiz.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbupky.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbuptm.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbupts.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbuse.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbusin.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbutim.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbutis.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbuvtx.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbvhea.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbview.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbvin.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbvldt.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbvout.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbvwpr.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbwrdp.o)\
  $(d0root)/test/dbl3/dbl3.a(xdbxini.o)\
  $(d0root)/test/dbl3/dbl3.a(xidbtyp.o)
	ar $(ARFLAGS) $(d0root)/test/dbl3/dbl3.a `ls $(SCRATCH)/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/dbl3/dbl3.a `ls $(SCRATCH)/a2/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/dbl3/dbl3.a `ls $(SCRATCH)/a3/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/dbl3/dbl3.a `ls $(SCRATCH)/a4/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/dbl3/dbl3.a `ls $(SCRATCH)/a5/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/dbl3/dbl3.a `ls $(SCRATCH)/a6/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/dbl3/dbl3.a `ls $(SCRATCH)/a7/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/dbl3/dbl3.a `ls $(SCRATCH)/a8/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/dbl3/dbl3.a `ls $(SCRATCH)/a9/*.o 2> /dev/null`
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
$(d0root)/test/dbl3/dbl3.a(xdbabrd.o):\
  $(d0library)/dbl3/dbl3/dbabrd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbabrd.f ;\
	mv dbabrd.o $(SCRATCH)/a1/dbabrd.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbabwr.o):\
  $(d0library)/dbl3/dbl3/dbabwr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbabwr.f ;\
	mv dbabwr.o $(SCRATCH)/a1/dbabwr.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbacpl.o):\
  $(d0library)/dbl3/dbl3/dbacpl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbacpl.f ;\
	mv dbacpl.o $(SCRATCH)/a1/dbacpl.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbacti.o):\
  $(d0library)/dbl3/dbl3/dbacti.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbacti.f ;\
	mv dbacti.o $(SCRATCH)/a1/dbacti.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbaird.o):\
  $(d0library)/dbl3/dbl3/dbaird.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbaird.f ;\
	mv dbaird.o $(SCRATCH)/a1/dbaird.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbaiwr.o):\
  $(d0library)/dbl3/dbl3/dbaiwr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbaiwr.f ;\
	mv dbaiwr.o $(SCRATCH)/a1/dbaiwr.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbauxi.o):\
  $(d0library)/dbl3/dbl3/dbauxi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbauxi.f ;\
	mv dbauxi.o $(SCRATCH)/a1/dbauxi.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbbook.o):\
  $(d0library)/dbl3/dbl3/dbbook.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbbook.f ;\
	mv dbbook.o $(SCRATCH)/a1/dbbook.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbcdic.o):\
  $(d0library)/dbl3/dbl3/dbcdic.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbcdic.f ;\
	mv dbcdic.o $(SCRATCH)/a1/dbcdic.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbcfri.o):\
  $(d0library)/dbl3/dbl3/dbcfri.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbcfri.f ;\
	mv dbcfri.o $(SCRATCH)/a1/dbcfri.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbchck.o):\
  $(d0library)/dbl3/dbl3/dbchck.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbchck.f ;\
	mv dbchck.o $(SCRATCH)/a1/dbchck.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbchfi.o):\
  $(d0library)/dbl3/dbl3/dbchfi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbchfi.f ;\
	mv dbchfi.o $(SCRATCH)/a1/dbchfi.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbchky.o):\
  $(d0library)/dbl3/dbl3/dbchky.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbchky.f ;\
	mv dbchky.o $(SCRATCH)/a1/dbchky.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbclos.o):\
  $(d0library)/dbl3/dbl3/dbclos.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbclos.f ;\
	mv dbclos.o $(SCRATCH)/a1/dbclos.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbcmpr.o):\
  $(d0library)/dbl3/dbl3/dbcmpr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbcmpr.f ;\
	mv dbcmpr.o $(SCRATCH)/a1/dbcmpr.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbcmpz.o):\
  $(d0library)/dbl3/dbl3/dbcmpz.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbcmpz.f ;\
	mv dbcmpz.o $(SCRATCH)/a1/dbcmpz.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbcomp.o):\
  $(d0library)/dbl3/dbl3/dbcomp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbcomp.f ;\
	mv dbcomp.o $(SCRATCH)/a1/dbcomp.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbconc.o):\
  $(d0library)/dbl3/dbl3/dbconc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbconc.f ;\
	mv dbconc.o $(SCRATCH)/a1/dbconc.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbcrdr.o):\
  $(d0library)/dbl3/dbl3/dbcrdr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbcrdr.f ;\
	mv dbcrdr.o $(SCRATCH)/a1/dbcrdr.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbcrsd.o):\
  $(d0library)/dbl3/dbl3/dbcrsd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbcrsd.f ;\
	mv dbcrsd.o $(SCRATCH)/a1/dbcrsd.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbctob.o):\
  $(d0library)/dbl3/dbl3/dbctob.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbctob.f ;\
	mv dbctob.o $(SCRATCH)/a2/dbctob.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbctoi.o):\
  $(d0library)/dbl3/dbl3/dbctoi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbctoi.f ;\
	mv dbctoi.o $(SCRATCH)/a2/dbctoi.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbctor.o):\
  $(d0library)/dbl3/dbl3/dbctor.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbctor.f ;\
	mv dbctor.o $(SCRATCH)/a2/dbctor.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbdckh.o):\
  $(d0library)/dbl3/dbl3/dbdckh.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbdckh.f ;\
	mv dbdckh.o $(SCRATCH)/a2/dbdckh.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbdckv.o):\
  $(d0library)/dbl3/dbl3/dbdckv.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbdckv.f ;\
	mv dbdckv.o $(SCRATCH)/a2/dbdckv.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbdelk.o):\
  $(d0library)/dbl3/dbl3/dbdelk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbdelk.f ;\
	mv dbdelk.o $(SCRATCH)/a2/dbdelk.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbdelt.o):\
  $(d0library)/dbl3/dbl3/dbdelt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbdelt.f ;\
	mv dbdelt.o $(SCRATCH)/a2/dbdelt.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbdhea.o):\
  $(d0library)/dbl3/dbl3/dbdhea.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbdhea.f ;\
	mv dbdhea.o $(SCRATCH)/a2/dbdhea.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbdisd.o):\
  $(d0library)/dbl3/dbl3/dbdisd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbdisd.f ;\
	mv dbdisd.o $(SCRATCH)/a2/dbdisd.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbdish.o):\
  $(d0library)/dbl3/dbl3/dbdish.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbdish.f ;\
	mv dbdish.o $(SCRATCH)/a2/dbdish.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbdisp.o):\
  $(d0library)/dbl3/dbl3/dbdisp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbdisp.f ;\
	mv dbdisp.o $(SCRATCH)/a2/dbdisp.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbdisv.o):\
  $(d0library)/dbl3/dbl3/dbdisv.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbdisv.f ;\
	mv dbdisv.o $(SCRATCH)/a2/dbdisv.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbdkyh.o):\
  $(d0library)/dbl3/dbl3/dbdkyh.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbdkyh.f ;\
	mv dbdkyh.o $(SCRATCH)/a2/dbdkyh.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbdkyv.o):\
  $(d0library)/dbl3/dbl3/dbdkyv.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbdkyv.f ;\
	mv dbdkyv.o $(SCRATCH)/a2/dbdkyv.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbdont.o):\
  $(d0library)/dbl3/dbl3/dbdont.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbdont.f ;\
	mv dbdont.o $(SCRATCH)/a2/dbdont.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbdprg.o):\
  $(d0library)/dbl3/dbl3/dbdprg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbdprg.f ;\
	mv dbdprg.o $(SCRATCH)/a2/dbdprg.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbeali.o):\
  $(d0library)/dbl3/dbl3/dbeali.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbeali.f ;\
	mv dbeali.o $(SCRATCH)/a2/dbeali.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbedas.o):\
  $(d0library)/dbl3/dbl3/dbedas.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbedas.f ;\
	mv dbedas.o $(SCRATCH)/a2/dbedas.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbedky.o):\
  $(d0library)/dbl3/dbl3/dbedky.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbedky.f ;\
	mv dbedky.o $(SCRATCH)/a2/dbedky.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbefor.o):\
  $(d0library)/dbl3/dbl3/dbefor.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbefor.f ;\
	mv dbefor.o $(SCRATCH)/a2/dbefor.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbehlp.o):\
  $(d0library)/dbl3/dbl3/dbehlp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbehlp.f ;\
	mv dbehlp.o $(SCRATCH)/a3/dbehlp.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbenam.o):\
  $(d0library)/dbl3/dbl3/dbenam.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbenam.f ;\
	mv dbenam.o $(SCRATCH)/a3/dbenam.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbend.o):\
  $(d0library)/dbl3/dbl3/dbend.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbend.f ;\
	mv dbend.o $(SCRATCH)/a3/dbend.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbendf.o):\
  $(d0library)/dbl3/dbl3/dbendf.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbendf.f ;\
	mv dbendf.o $(SCRATCH)/a3/dbendf.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbenfz.o):\
  $(d0library)/dbl3/dbl3/dbenfz.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbenfz.f ;\
	mv dbenfz.o $(SCRATCH)/a3/dbenfz.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbentb.o):\
  $(d0library)/dbl3/dbl3/dbentb.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbentb.f ;\
	mv dbentb.o $(SCRATCH)/a3/dbentb.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbentr.o):\
  $(d0library)/dbl3/dbl3/dbentr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbentr.f ;\
	mv dbentr.o $(SCRATCH)/a3/dbentr.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbflin.o):\
  $(d0library)/dbl3/dbl3/dbflin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbflin.f ;\
	mv dbflin.o $(SCRATCH)/a3/dbflin.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbfpat.o):\
  $(d0library)/dbl3/dbl3/dbfpat.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbfpat.f ;\
	mv dbfpat.o $(SCRATCH)/a3/dbfpat.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbfree.o):\
  $(d0library)/dbl3/dbl3/dbfree.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbfree.f ;\
	mv dbfree.o $(SCRATCH)/a3/dbfree.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbfrst.o):\
  $(d0library)/dbl3/dbl3/dbfrst.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbfrst.f ;\
	mv dbfrst.o $(SCRATCH)/a3/dbfrst.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbfrus.o):\
  $(d0library)/dbl3/dbl3/dbfrus.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbfrus.f ;\
	mv dbfrus.o $(SCRATCH)/a3/dbfrus.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbftio.o):\
  $(d0library)/dbl3/dbl3/dbftio.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbftio.f ;\
	mv dbftio.o $(SCRATCH)/a3/dbftio.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbfzin.o):\
  $(d0library)/dbl3/dbl3/dbfzin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbfzin.f ;\
	mv dbfzin.o $(SCRATCH)/a3/dbfzin.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbfzop.o):\
  $(d0library)/dbl3/dbl3/dbfzop.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbfzop.f ;\
	mv dbfzop.o $(SCRATCH)/a3/dbfzop.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbfzup.o):\
  $(d0library)/dbl3/dbl3/dbfzup.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbfzup.f ;\
	mv dbfzup.o $(SCRATCH)/a3/dbfzup.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbfzwr.o):\
  $(d0library)/dbl3/dbl3/dbfzwr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbfzwr.f ;\
	mv dbfzwr.o $(SCRATCH)/a3/dbfzwr.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbget.o):\
  $(d0library)/dbl3/dbl3/dbget.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbget.f ;\
	mv dbget.o $(SCRATCH)/a3/dbget.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbgets.o):\
  $(d0library)/dbl3/dbl3/dbgets.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbgets.f ;\
	mv dbgets.o $(SCRATCH)/a3/dbgets.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbgnam.o):\
  $(d0library)/dbl3/dbl3/dbgnam.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbgnam.f ;\
	mv dbgnam.o $(SCRATCH)/a3/dbgnam.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbgpid.o):\
  $(d0library)/dbl3/dbl3/dbgpid.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbgpid.f ;\
	mv dbgpid.o $(SCRATCH)/a4/dbgpid.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbhunt.o):\
  $(d0library)/dbl3/dbl3/dbhunt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbhunt.f ;\
	mv dbhunt.o $(SCRATCH)/a4/dbhunt.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbifch.o):\
  $(d0library)/dbl3/dbl3/dbifch.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbifch.f ;\
	mv dbifch.o $(SCRATCH)/a4/dbifch.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbifrc.o):\
  $(d0library)/dbl3/dbl3/dbifrc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbifrc.f ;\
	mv dbifrc.o $(SCRATCH)/a4/dbifrc.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbildf.o):\
  $(d0library)/dbl3/dbl3/dbildf.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbildf.f ;\
	mv dbildf.o $(SCRATCH)/a4/dbildf.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbildu.o):\
  $(d0library)/dbl3/dbl3/dbildu.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbildu.f ;\
	mv dbildu.o $(SCRATCH)/a4/dbildu.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbin.o):\
  $(d0library)/dbl3/dbl3/dbin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbin.f ;\
	mv dbin.o $(SCRATCH)/a4/dbin.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbinct.o):\
  $(d0library)/dbl3/dbl3/dbinct.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbinct.f ;\
	mv dbinct.o $(SCRATCH)/a4/dbinct.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbinin.o):\
  $(d0library)/dbl3/dbl3/dbinin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbinin.f ;\
	mv dbinin.o $(SCRATCH)/a4/dbinin.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbinit.o):\
  $(d0library)/dbl3/dbl3/dbinit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbinit.f ;\
	mv dbinit.o $(SCRATCH)/a4/dbinit.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbioty.o):\
  $(d0library)/dbl3/dbl3/dbioty.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbioty.f ;\
	mv dbioty.o $(SCRATCH)/a4/dbioty.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbizin.o):\
  $(d0library)/dbl3/dbl3/dbizin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbizin.f ;\
	mv dbizin.o $(SCRATCH)/a4/dbizin.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbjoin.o):\
  $(d0library)/dbl3/dbl3/dbjoin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbjoin.f ;\
	mv dbjoin.o $(SCRATCH)/a4/dbjoin.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbkept.o):\
  $(d0library)/dbl3/dbl3/dbkept.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbkept.f ;\
	mv dbkept.o $(SCRATCH)/a4/dbkept.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbkeyr.o):\
  $(d0library)/dbl3/dbl3/dbkeyr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbkeyr.f ;\
	mv dbkeyr.o $(SCRATCH)/a4/dbkeyr.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbkeys.o):\
  $(d0library)/dbl3/dbl3/dbkeys.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbkeys.f ;\
	mv dbkeys.o $(SCRATCH)/a4/dbkeys.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbkeyt.o):\
  $(d0library)/dbl3/dbl3/dbkeyt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbkeyt.f ;\
	mv dbkeyt.o $(SCRATCH)/a4/dbkeyt.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbkin.o):\
  $(d0library)/dbl3/dbl3/dbkin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbkin.f ;\
	mv dbkin.o $(SCRATCH)/a4/dbkin.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbkout.o):\
  $(d0library)/dbl3/dbl3/dbkout.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbkout.f ;\
	mv dbkout.o $(SCRATCH)/a4/dbkout.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbktyp.o):\
  $(d0library)/dbl3/dbl3/dbktyp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbktyp.f ;\
	mv dbktyp.o $(SCRATCH)/a4/dbktyp.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbkvin.o):\
  $(d0library)/dbl3/dbl3/dbkvin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbkvin.f ;\
	mv dbkvin.o $(SCRATCH)/a5/dbkvin.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbkxin.o):\
  $(d0library)/dbl3/dbl3/dbkxin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbkxin.f ;\
	mv dbkxin.o $(SCRATCH)/a5/dbkxin.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbkyse.o):\
  $(d0library)/dbl3/dbl3/dbkyse.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbkyse.f ;\
	mv dbkyse.o $(SCRATCH)/a5/dbkyse.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbkytg.o):\
  $(d0library)/dbl3/dbl3/dbkytg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbkytg.f ;\
	mv dbkytg.o $(SCRATCH)/a5/dbkytg.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdblast.o):\
  $(d0library)/dbl3/dbl3/dblast.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dblast.f ;\
	mv dblast.o $(SCRATCH)/a5/dblast.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdblinc.o):\
  $(d0library)/dbl3/dbl3/dblinc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dblinc.f ;\
	mv dblinc.o $(SCRATCH)/a5/dblinc.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdblind.o):\
  $(d0library)/dbl3/dbl3/dblind.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dblind.f ;\
	mv dblind.o $(SCRATCH)/a5/dblind.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdblkey.o):\
  $(d0library)/dbl3/dbl3/dblkey.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dblkey.f ;\
	mv dblkey.o $(SCRATCH)/a5/dblkey.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdblmod.o):\
  $(d0library)/dbl3/dbl3/dblmod.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dblmod.f ;\
	mv dblmod.o $(SCRATCH)/a5/dblmod.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdblogl.o):\
  $(d0library)/dbl3/dbl3/dblogl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dblogl.f ;\
	mv dblogl.o $(SCRATCH)/a5/dblogl.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdblook.o):\
  $(d0library)/dbl3/dbl3/dblook.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dblook.f ;\
	mv dblook.o $(SCRATCH)/a5/dblook.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbmdip.o):\
  $(d0library)/dbl3/dbl3/dbmdip.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbmdip.f ;\
	mv dbmdip.o $(SCRATCH)/a5/dbmdip.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbmdir.o):\
  $(d0library)/dbl3/dbl3/dbmdir.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbmdir.f ;\
	mv dbmdir.o $(SCRATCH)/a5/dbmdir.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbnode.o):\
  $(d0library)/dbl3/dbl3/dbnode.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbnode.f ;\
	mv dbnode.o $(SCRATCH)/a5/dbnode.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbntop.o):\
  $(d0library)/dbl3/dbl3/dbntop.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbntop.f ;\
	mv dbntop.o $(SCRATCH)/a5/dbntop.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbopen.o):\
  $(d0library)/dbl3/dbl3/dbopen.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbopen.f ;\
	mv dbopen.o $(SCRATCH)/a5/dbopen.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbopts.o):\
  $(d0library)/dbl3/dbl3/dbopts.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbopts.f ;\
	mv dbopts.o $(SCRATCH)/a5/dbopts.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbout.o):\
  $(d0library)/dbl3/dbl3/dbout.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbout.f ;\
	mv dbout.o $(SCRATCH)/a5/dbout.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbpack.o):\
  $(d0library)/dbl3/dbl3/dbpack.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbpack.f ;\
	mv dbpack.o $(SCRATCH)/a5/dbpack.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbpath.o):\
  $(d0library)/dbl3/dbl3/dbpath.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbpath.f ;\
	mv dbpath.o $(SCRATCH)/a5/dbpath.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbpeek.o):\
  $(d0library)/dbl3/dbl3/dbpeek.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbpeek.f ;\
	mv dbpeek.o $(SCRATCH)/a6/dbpeek.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbpktm.o):\
  $(d0library)/dbl3/dbl3/dbpktm.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbpktm.f ;\
	mv dbpktm.o $(SCRATCH)/a6/dbpktm.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbpkts.o):\
  $(d0library)/dbl3/dbl3/dbpkts.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbpkts.f ;\
	mv dbpkts.o $(SCRATCH)/a6/dbpkts.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbplbk.o):\
  $(d0library)/dbl3/dbl3/dbplbk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbplbk.f ;\
	mv dbplbk.o $(SCRATCH)/a6/dbplbk.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbplnt.o):\
  $(d0library)/dbl3/dbl3/dbplnt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbplnt.f ;\
	mv dbplnt.o $(SCRATCH)/a6/dbplnt.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbplob.o):\
  $(d0library)/dbl3/dbl3/dbplob.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbplob.f ;\
	mv dbplob.o $(SCRATCH)/a6/dbplob.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbplov.o):\
  $(d0library)/dbl3/dbl3/dbplov.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbplov.f ;\
	mv dbplov.o $(SCRATCH)/a6/dbplov.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbplti.o):\
  $(d0library)/dbl3/dbl3/dbplti.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbplti.f ;\
	mv dbplti.o $(SCRATCH)/a6/dbplti.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbprdt.o):\
  $(d0library)/dbl3/dbl3/dbprdt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbprdt.f ;\
	mv dbprdt.o $(SCRATCH)/a6/dbprdt.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbpres.o):\
  $(d0library)/dbl3/dbl3/dbpres.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbpres.f ;\
	mv dbpres.o $(SCRATCH)/a6/dbpres.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbprgd.o):\
  $(d0library)/dbl3/dbl3/dbprgd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbprgd.f ;\
	mv dbprgd.o $(SCRATCH)/a6/dbprgd.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbprin.o):\
  $(d0library)/dbl3/dbl3/dbprin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbprin.f ;\
	mv dbprin.o $(SCRATCH)/a6/dbprin.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbprky.o):\
  $(d0library)/dbl3/dbl3/dbprky.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbprky.f ;\
	mv dbprky.o $(SCRATCH)/a6/dbprky.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbprnt.o):\
  $(d0library)/dbl3/dbl3/dbprnt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbprnt.f ;\
	mv dbprnt.o $(SCRATCH)/a6/dbprnt.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbproc.o):\
  $(d0library)/dbl3/dbl3/dbproc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbproc.f ;\
	mv dbproc.o $(SCRATCH)/a6/dbproc.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbpurg.o):\
  $(d0library)/dbl3/dbl3/dbpurg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbpurg.f ;\
	mv dbpurg.o $(SCRATCH)/a6/dbpurg.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbpurk.o):\
  $(d0library)/dbl3/dbl3/dbpurk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbpurk.f ;\
	mv dbpurk.o $(SCRATCH)/a6/dbpurk.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbrali.o):\
  $(d0library)/dbl3/dbl3/dbrali.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbrali.f ;\
	mv dbrali.o $(SCRATCH)/a6/dbrali.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbrdda.o):\
  $(d0library)/dbl3/dbl3/dbrdda.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbrdda.f ;\
	mv dbrdda.o $(SCRATCH)/a6/dbrdda.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbrdio.o):\
  $(d0library)/dbl3/dbl3/dbrdio.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbrdio.f ;\
	mv dbrdio.o $(SCRATCH)/a6/dbrdio.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbrenk.o):\
  $(d0library)/dbl3/dbl3/dbrenk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbrenk.f ;\
	mv dbrenk.o $(SCRATCH)/a7/dbrenk.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbrepl.o):\
  $(d0library)/dbl3/dbl3/dbrepl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbrepl.f ;\
	mv dbrepl.o $(SCRATCH)/a7/dbrepl.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbrgck.o):\
  $(d0library)/dbl3/dbl3/dbrgck.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbrgck.f ;\
	mv dbrgck.o $(SCRATCH)/a7/dbrgck.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbrgcv.o):\
  $(d0library)/dbl3/dbl3/dbrgcv.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbrgcv.f ;\
	mv dbrgcv.o $(SCRATCH)/a7/dbrgcv.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbrhlp.o):\
  $(d0library)/dbl3/dbl3/dbrhlp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbrhlp.f ;\
	mv dbrhlp.o $(SCRATCH)/a7/dbrhlp.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbrky1.o):\
  $(d0library)/dbl3/dbl3/dbrky1.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbrky1.f ;\
	mv dbrky1.o $(SCRATCH)/a7/dbrky1.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbrnam.o):\
  $(d0library)/dbl3/dbl3/dbrnam.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbrnam.f ;\
	mv dbrnam.o $(SCRATCH)/a7/dbrnam.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbropn.o):\
  $(d0library)/dbl3/dbl3/dbropn.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbropn.f ;\
	mv dbropn.o $(SCRATCH)/a7/dbropn.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbrtfz.o):\
  $(d0library)/dbl3/dbl3/dbrtfz.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbrtfz.f ;\
	mv dbrtfz.o $(SCRATCH)/a7/dbrtfz.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbrvnt.o):\
  $(d0library)/dbl3/dbl3/dbrvnt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbrvnt.f ;\
	mv dbrvnt.o $(SCRATCH)/a7/dbrvnt.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbrvpl.o):\
  $(d0library)/dbl3/dbl3/dbrvpl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbrvpl.f ;\
	mv dbrvpl.o $(SCRATCH)/a7/dbrvpl.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbrzin.o):\
  $(d0library)/dbl3/dbl3/dbrzin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbrzin.f ;\
	mv dbrzin.o $(SCRATCH)/a7/dbrzin.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbsave.o):\
  $(d0library)/dbl3/dbl3/dbsave.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbsave.f ;\
	mv dbsave.o $(SCRATCH)/a7/dbsave.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbsblc.o):\
  $(d0library)/dbl3/dbl3/dbsblc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbsblc.f ;\
	mv dbsblc.o $(SCRATCH)/a7/dbsblc.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbsdir.o):\
  $(d0library)/dbl3/dbl3/dbsdir.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbsdir.f ;\
	mv dbsdir.o $(SCRATCH)/a7/dbsdir.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbseky.o):\
  $(d0library)/dbl3/dbl3/dbseky.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbseky.f ;\
	mv dbseky.o $(SCRATCH)/a7/dbseky.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbsnam.o):\
  $(d0library)/dbl3/dbl3/dbsnam.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbsnam.f ;\
	mv dbsnam.o $(SCRATCH)/a7/dbsnam.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbsopn.o):\
  $(d0library)/dbl3/dbl3/dbsopn.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbsopn.f ;\
	mv dbsopn.o $(SCRATCH)/a7/dbsopn.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbspur.o):\
  $(d0library)/dbl3/dbl3/dbspur.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbspur.f ;\
	mv dbspur.o $(SCRATCH)/a7/dbspur.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbsrtm.o):\
  $(d0library)/dbl3/dbl3/dbsrtm.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbsrtm.f ;\
	mv dbsrtm.o $(SCRATCH)/a7/dbsrtm.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbtbcr.o):\
  $(d0library)/dbl3/dbl3/dbtbcr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbtbcr.f ;\
	mv dbtbcr.o $(SCRATCH)/a8/dbtbcr.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbtbpr.o):\
  $(d0library)/dbl3/dbl3/dbtbpr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbtbpr.f ;\
	mv dbtbpr.o $(SCRATCH)/a8/dbtbpr.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbtemp.o):\
  $(d0library)/dbl3/dbl3/dbtemp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbtemp.f ;\
	mv dbtemp.o $(SCRATCH)/a8/dbtemp.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbtime.o):\
  $(d0library)/dbl3/dbl3/dbtime.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbtime.f ;\
	mv dbtime.o $(SCRATCH)/a8/dbtime.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbtopn.o):\
  $(d0library)/dbl3/dbl3/dbtopn.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbtopn.f ;\
	mv dbtopn.o $(SCRATCH)/a8/dbtopn.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbtous.o):\
  $(d0library)/dbl3/dbl3/dbtous.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbtous.f ;\
	mv dbtous.o $(SCRATCH)/a8/dbtous.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbucmp.o):\
  $(d0library)/dbl3/dbl3/dbucmp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbucmp.f ;\
	mv dbucmp.o $(SCRATCH)/a8/dbucmp.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbucmz.o):\
  $(d0library)/dbl3/dbl3/dbucmz.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbucmz.f ;\
	mv dbucmz.o $(SCRATCH)/a8/dbucmz.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbudic.o):\
  $(d0library)/dbl3/dbl3/dbudic.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbudic.f ;\
	mv dbudic.o $(SCRATCH)/a8/dbudic.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbuncp.o):\
  $(d0library)/dbl3/dbl3/dbuncp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbuncp.f ;\
	mv dbuncp.o $(SCRATCH)/a8/dbuncp.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbupck.o):\
  $(d0library)/dbl3/dbl3/dbupck.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbupck.f ;\
	mv dbupck.o $(SCRATCH)/a8/dbupck.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbupfz.o):\
  $(d0library)/dbl3/dbl3/dbupfz.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbupfz.f ;\
	mv dbupfz.o $(SCRATCH)/a8/dbupfz.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbupiz.o):\
  $(d0library)/dbl3/dbl3/dbupiz.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbupiz.f ;\
	mv dbupiz.o $(SCRATCH)/a8/dbupiz.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbupky.o):\
  $(d0library)/dbl3/dbl3/dbupky.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbupky.f ;\
	mv dbupky.o $(SCRATCH)/a8/dbupky.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbuptm.o):\
  $(d0library)/dbl3/dbl3/dbuptm.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbuptm.f ;\
	mv dbuptm.o $(SCRATCH)/a8/dbuptm.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbupts.o):\
  $(d0library)/dbl3/dbl3/dbupts.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbupts.f ;\
	mv dbupts.o $(SCRATCH)/a8/dbupts.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbuse.o):\
  $(d0library)/dbl3/dbl3/dbuse.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbuse.f ;\
	mv dbuse.o $(SCRATCH)/a8/dbuse.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbusin.o):\
  $(d0library)/dbl3/dbl3/dbusin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbusin.f ;\
	mv dbusin.o $(SCRATCH)/a8/dbusin.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbutim.o):\
  $(d0library)/dbl3/dbl3/dbutim.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbutim.f ;\
	mv dbutim.o $(SCRATCH)/a8/dbutim.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbutis.o):\
  $(d0library)/dbl3/dbl3/dbutis.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbutis.f ;\
	mv dbutis.o $(SCRATCH)/a8/dbutis.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbuvtx.o):\
  $(d0library)/dbl3/dbl3/dbuvtx.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbuvtx.f ;\
	mv dbuvtx.o $(SCRATCH)/a9/dbuvtx.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbvhea.o):\
  $(d0library)/dbl3/dbl3/dbvhea.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbvhea.f ;\
	mv dbvhea.o $(SCRATCH)/a9/dbvhea.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbview.o):\
  $(d0library)/dbl3/dbl3/dbview.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbview.f ;\
	mv dbview.o $(SCRATCH)/a9/dbview.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbvin.o):\
  $(d0library)/dbl3/dbl3/dbvin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbvin.f ;\
	mv dbvin.o $(SCRATCH)/a9/dbvin.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbvldt.o):\
  $(d0library)/dbl3/dbl3/dbvldt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbvldt.f ;\
	mv dbvldt.o $(SCRATCH)/a9/dbvldt.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbvout.o):\
  $(d0library)/dbl3/dbl3/dbvout.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbvout.f ;\
	mv dbvout.o $(SCRATCH)/a9/dbvout.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbvwpr.o):\
  $(d0library)/dbl3/dbl3/dbvwpr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbvwpr.f ;\
	mv dbvwpr.o $(SCRATCH)/a9/dbvwpr.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbwrdp.o):\
  $(d0library)/dbl3/dbl3/dbwrdp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbwrdp.f ;\
	mv dbwrdp.o $(SCRATCH)/a9/dbwrdp.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xdbxini.o):\
  $(d0library)/dbl3/dbl3/dbxini.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/dbxini.f ;\
	mv dbxini.o $(SCRATCH)/a9/dbxini.o ;\
	)
$(d0root)/test/dbl3/dbl3.a(xidbtyp.o):\
  $(d0library)/dbl3/dbl3/idbtyp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) $(d0library)/dbl3/dbl3/idbtyp.f ;\
	mv idbtyp.o $(SCRATCH)/a9/idbtyp.o ;\
	)
pre:
for:
