SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0gamma)/unix/source -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0gamma)/unix/source -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Nn15000 -Wb,-force_branch_fixup -Olimit 3000 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/tmp/d0librar/userlib/17124/d0dbl3
FLAVOR = SIUNIX
CC = cc
LEX = lex
F77 = f77
.IGNORE:
.SUFFIXES:
debug :\
  deb_d0dbl3.a
deb_d0dbl3.a : $(d0root)/test/dbl3/deb_d0dbl3.a
	@ echo deb_d0dbl3.a is up to date
$(d0root)/test/dbl3/deb_d0dbl3.a:: $(SCRATCH)/nothing.nl
$(d0root)/test/dbl3/deb_d0dbl3.a::\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xclblnk.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xcondbl.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbclb_detseq.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbclb_end.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbclb_finish.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbclb_init.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbclblize682.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbclb_path.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbclbinfo560.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbclb_seqdet.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xerrdb.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xinzdbl.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbclbline964.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbo_ggers700.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xgt_dbfile.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_end.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_fetch.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_fetcm.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_ini.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_insrt.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_kychk.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_kycls.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_set.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_start.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd3unit.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd3utop.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd3uxh.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbmctoh.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_enddm.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_endhv.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_file.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_finf.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_fno.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_getc.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_getdm.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_gethv.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_gtfile.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_unpdm.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_unphv.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbuky.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdbume.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xdukeyd.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xmsgo.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd0dblinct238.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd0dblpkts258.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd0dblupts268.o)\
  $(d0root)/test/dbl3/deb_d0dbl3.a(xd3upt.o)
	ar $(ARFLAGS) $(d0root)/test/dbl3/deb_d0dbl3.a `ls $(SCRATCH)/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/dbl3/deb_d0dbl3.a `ls $(SCRATCH)/a2/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/test/dbl3/deb_d0dbl3.a `ls $(SCRATCH)/a3/*.o 2> /dev/null`
	rmdirp $(SCRATCH)
$(SCRATCH)/nothing.nl:
	rmdirp $(SCRATCH)
	mkdirp $(SCRATCH)/a1
	mkdirp $(SCRATCH)/a2
	mkdirp $(SCRATCH)/a3
$(d0root)/test/dbl3/deb_d0dbl3.a(xclblnk.o):\
  $(d0library)/dbl3/calib/clblnk.f\
  $(d0library)/params/calib.def\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/lkcalib.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/calib/clblnk.f ;\
	mv clblnk.o $(SCRATCH)/a1/clblnk.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xcondbl.o):\
  $(d0library)/dbl3/calib/condbl.f\
  $(d0library)/inc/zebdbl.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izstpo.link\
  $(d0library)/links/izstpn.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/calib/condbl.f ;\
	mv condbl.o $(SCRATCH)/a1/condbl.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbclb_detseq.o):\
  $(d0library)/dbl3/calib/dbclb_detseq.f\
  $(d0library)/inc/dbstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/calib/dbclb_detseq.f ;\
	mv dbclb_detseq.o $(SCRATCH)/a1/dbclb_detseq.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbclb_end.o):\
  $(d0library)/dbl3/calib/dbclb_end.f\
  $(d0library)/inc/dbstp.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/calib/dbclb_end.f ;\
	mv dbclb_end.o $(SCRATCH)/a1/dbclb_end.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbclb_finish.o):\
  $(d0library)/dbl3/calib/dbclb_finish.f\
  $(d0library)/inc/dbstp.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/calib/dbclb_finish.f ;\
	mv dbclb_finish.o $(SCRATCH)/a1/dbclb_finish.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbclb_init.o):\
  $(d0library)/dbl3/calib/dbclb_init.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/params/calib.def\
  $(d0library)/inc/lkcalib.inc\
  $(d0library)/inc/dbstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/db_srvr_units.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/calib/dbclb_init.f ;\
	mv dbclb_init.o $(SCRATCH)/a1/dbclb_init.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbclblize682.o):\
  $(d0library)/dbl3/calib/dbclb_initialize.f\
  $(d0library)/inc/dbstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/calib/dbclb_initialize.f ;\
	mv dbclb_initialize.o $(SCRATCH)/a1/dbclblize682.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbclb_path.o):\
  $(d0library)/dbl3/calib/dbclb_path.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/calib/dbclb_path.f ;\
	mv dbclb_path.o $(SCRATCH)/a1/dbclb_path.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbclbinfo560.o):\
  $(d0library)/dbl3/calib/dbclb_path_info.f\
  $(d0library)/links/izslv0.link\
  $(d0library)/links/izsmuo.link\
  $(d0library)/links/izsvtx.link\
  $(d0library)/links/izscdc.link\
  $(d0library)/links/izsfdc.link\
  $(d0library)/links/izstrd.link\
  $(d0library)/links/izscal.link\
  $(d0library)/links/izsgen.link\
  $(d0library)/links/izssam.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/calib/dbclb_path_info.f ;\
	mv dbclb_path_info.o $(SCRATCH)/a1/dbclbinfo560.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbclb_seqdet.o):\
  $(d0library)/dbl3/calib/dbclb_seqdet.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/calib/dbclb_seqdet.f ;\
	mv dbclb_seqdet.o $(SCRATCH)/a1/dbclb_seqdet.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xerrdb.o):\
  $(d0library)/dbl3/calib/errdb.f\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/calib/errdb.f ;\
	mv errdb.o $(SCRATCH)/a1/errdb.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xinzdbl.o):\
  $(d0library)/dbl3/calib/inzdbl.f\
  $(d0library)/inc/zebdbl.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/calib/inzdbl.f ;\
	mv inzdbl.o $(SCRATCH)/a1/inzdbl.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbclbline964.o):\
  $(d0library)/dbl3/dboffline/dbclb_fetch_offline.f\
  $(d0library)/params/calib.def\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zebdbl.inc\
  $(d0library)/inc/lkcalib.inc\
  $(d0library)/inc/dbstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/links/izstpc.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/dboffline/dbclb_fetch_offline.f ;\
	mv dbclb_fetch_offline.o $(SCRATCH)/a1/dbclbline964.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbo_ggers700.o):\
  $(d0library)/dbl3/dboffline/dbo_get_triggers.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/dboffline/dbo_get_triggers.f ;\
	mv dbo_get_triggers.o $(SCRATCH)/a1/dbo_ggers700.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xgt_dbfile.o):\
  $(d0library)/dbl3/dboffline/gt_dbfile.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/dboffline/gt_dbfile.f ;\
	mv gt_dbfile.o $(SCRATCH)/a1/gt_dbfile.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_end.o):\
  $(d0library)/dbl3/general/d3u_end.f\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/d3u_end.f ;\
	mv d3u_end.o $(SCRATCH)/a1/d3u_end.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_fetch.o):\
  $(d0library)/dbl3/general/d3u_fetch.f\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/d3u_fetch.f ;\
	mv d3u_fetch.o $(SCRATCH)/a1/d3u_fetch.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_fetcm.o):\
  $(d0library)/dbl3/general/d3u_fetcm.f\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/d3u_fetcm.f ;\
	mv d3u_fetcm.o $(SCRATCH)/a1/d3u_fetcm.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_ini.o):\
  $(d0library)/dbl3/general/d3u_ini.f\
  $(d0library)/inc/d3u.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/d3u_ini.f ;\
	mv d3u_ini.o $(SCRATCH)/a1/d3u_ini.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_insrt.o):\
  $(d0library)/dbl3/general/d3u_insrt.f\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/d3u_insrt.f ;\
	mv d3u_insrt.o $(SCRATCH)/a2/d3u_insrt.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_kychk.o):\
  $(d0library)/dbl3/general/d3u_kychk.f\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/d3u_kychk.f ;\
	mv d3u_kychk.o $(SCRATCH)/a2/d3u_kychk.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_kycls.o):\
  $(d0library)/dbl3/general/d3u_kycls.f\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/d3u_kycls.f ;\
	mv d3u_kycls.o $(SCRATCH)/a2/d3u_kycls.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_set.o):\
  $(d0library)/dbl3/general/d3u_set.f\
  $(d0library)/inc/d3u.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/d3u_set.f ;\
	mv d3u_set.o $(SCRATCH)/a2/d3u_set.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd3u_start.o):\
  $(d0library)/dbl3/general/d3u_start.f\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/d3u_start.f ;\
	mv d3u_start.o $(SCRATCH)/a2/d3u_start.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd3unit.o):\
  $(d0library)/dbl3/general/d3unit.f\
  $(d0library)/inc/d3u.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/d3unit.f ;\
	mv d3unit.o $(SCRATCH)/a2/d3unit.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd3utop.o):\
  $(d0library)/dbl3/general/d3utop.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/d3utop.f ;\
	mv d3utop.o $(SCRATCH)/a2/d3utop.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd3uxh.o):\
  $(d0library)/dbl3/general/d3uxh.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/d3uxh.f ;\
	mv d3uxh.o $(SCRATCH)/a2/d3uxh.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbmctoh.o):\
  $(d0library)/dbl3/general/dbmctoh.f\
  $(d0library)/params/byte_order.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/dbmctoh.f ;\
	mv dbmctoh.o $(SCRATCH)/a2/dbmctoh.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_enddm.o):\
  $(d0library)/dbl3/general/dbmu_enddm.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/dbmu_enddm.f ;\
	mv dbmu_enddm.o $(SCRATCH)/a2/dbmu_enddm.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_endhv.o):\
  $(d0library)/dbl3/general/dbmu_endhv.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/dbmu_endhv.f ;\
	mv dbmu_endhv.o $(SCRATCH)/a2/dbmu_endhv.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_file.o):\
  $(d0library)/dbl3/general/dbmu_file.f\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/dbmu_file.f ;\
	mv dbmu_file.o $(SCRATCH)/a2/dbmu_file.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_finf.o):\
  $(d0library)/dbl3/general/dbmu_finf.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/dbmu_finf.f ;\
	mv dbmu_finf.o $(SCRATCH)/a2/dbmu_finf.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_fno.o):\
  $(d0library)/dbl3/general/dbmu_fno.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/dbmu_fno.f ;\
	mv dbmu_fno.o $(SCRATCH)/a2/dbmu_fno.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_getc.o):\
  $(d0library)/dbl3/general/dbmu_getc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/dbmu_getc.f ;\
	mv dbmu_getc.o $(SCRATCH)/a2/dbmu_getc.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_getdm.o):\
  $(d0library)/dbl3/general/dbmu_getdm.f\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/dbmu_getdm.f ;\
	mv dbmu_getdm.o $(SCRATCH)/a2/dbmu_getdm.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_gethv.o):\
  $(d0library)/dbl3/general/dbmu_gethv.f\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/dbmu_gethv.f ;\
	mv dbmu_gethv.o $(SCRATCH)/a2/dbmu_gethv.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_gtfile.o):\
  $(d0library)/dbl3/general/dbmu_gtfile.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/dbmu_gtfile.f ;\
	mv dbmu_gtfile.o $(SCRATCH)/a2/dbmu_gtfile.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_unpdm.o):\
  $(d0library)/dbl3/general/dbmu_unpdm.f\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/dbmu_unpdm.f ;\
	mv dbmu_unpdm.o $(SCRATCH)/a2/dbmu_unpdm.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbmu_unphv.o):\
  $(d0library)/dbl3/general/dbmu_unphv.f\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/dbmu_unphv.f ;\
	mv dbmu_unphv.o $(SCRATCH)/a2/dbmu_unphv.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbuky.o):\
  $(d0library)/dbl3/general/dbuky.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/dbuky.f ;\
	mv dbuky.o $(SCRATCH)/a3/dbuky.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdbume.o):\
  $(d0library)/dbl3/general/dbume.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/dbume.f ;\
	mv dbume.o $(SCRATCH)/a3/dbume.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xdukeyd.o):\
  $(d0library)/dbl3/general/dukeyd.f\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/dukeyd.f ;\
	mv dukeyd.o $(SCRATCH)/a3/dukeyd.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xmsgo.o):\
  $(d0library)/dbl3/general/msgo.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/general/msgo.f ;\
	mv msgo.o $(SCRATCH)/a3/msgo.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd0dblinct238.o):\
  $(d0library)/dbl3/server/d0dbl3_dbinct.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/server/d0dbl3_dbinct.f ;\
	mv d0dbl3_dbinct.o $(SCRATCH)/a3/d0dblinct238.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd0dblpkts258.o):\
  $(d0library)/dbl3/server/d0dbl3_dbpkts.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/server/d0dbl3_dbpkts.f ;\
	mv d0dbl3_dbpkts.o $(SCRATCH)/a3/d0dblpkts258.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd0dblupts268.o):\
  $(d0library)/dbl3/server/d0dbl3_dbupts.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) $(d0library)/dbl3/server/d0dbl3_dbupts.f ;\
	mv d0dbl3_dbupts.o $(SCRATCH)/a3/d0dblupts268.o ;\
	)
$(d0root)/test/dbl3/deb_d0dbl3.a(xd3upt.o):\
  $(d0root)/test/dbl3/general/d3upt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(DEBUG) test/dbl3/general/d3upt.f ;\
	mv d3upt.o $(SCRATCH)/a1/d3upt.o ;\
	)
pre:\
  $(d0library)/dbl3/calib/clblnk.f\
  $(d0library)/dbl3/calib/condbl.f\
  $(d0library)/dbl3/calib/dbclb_detseq.f\
  $(d0library)/dbl3/calib/dbclb_end.f\
  $(d0library)/dbl3/calib/dbclb_finish.f\
  $(d0library)/dbl3/calib/dbclb_init.f\
  $(d0library)/dbl3/calib/dbclb_initialize.f\
  $(d0library)/dbl3/calib/dbclb_path.f\
  $(d0library)/dbl3/calib/dbclb_path_info.f\
  $(d0library)/dbl3/calib/dbclb_seqdet.f\
  $(d0library)/dbl3/calib/errdb.f\
  $(d0library)/dbl3/calib/inzdbl.f\
  $(d0library)/dbl3/dboffline/dbclb_fetch_offline.f\
  $(d0library)/dbl3/dboffline/dbo_get_triggers.f\
  $(d0library)/dbl3/dboffline/gt_dbfile.f\
  $(d0library)/dbl3/general/d3u_end.f\
  $(d0library)/dbl3/general/d3u_fetch.f\
  $(d0library)/dbl3/general/d3u_fetcm.f\
  $(d0library)/dbl3/general/d3u_ini.f\
  $(d0library)/dbl3/general/d3u_insrt.f\
  $(d0library)/dbl3/general/d3u_kychk.f\
  $(d0library)/dbl3/general/d3u_kycls.f\
  $(d0library)/dbl3/general/d3u_set.f\
  $(d0library)/dbl3/general/d3u_start.f\
  $(d0library)/dbl3/general/d3unit.f\
  $(d0library)/dbl3/general/d3utop.f\
  $(d0library)/dbl3/general/d3uxh.f\
  $(d0library)/dbl3/general/dbmctoh.f\
  $(d0library)/dbl3/general/dbmu_enddm.f\
  $(d0library)/dbl3/general/dbmu_endhv.f\
  $(d0library)/dbl3/general/dbmu_file.f\
  $(d0library)/dbl3/general/dbmu_finf.f\
  $(d0library)/dbl3/general/dbmu_fno.f\
  $(d0library)/dbl3/general/dbmu_getc.f\
  $(d0library)/dbl3/general/dbmu_getdm.f\
  $(d0library)/dbl3/general/dbmu_gethv.f\
  $(d0library)/dbl3/general/dbmu_gtfile.f\
  $(d0library)/dbl3/general/dbmu_unpdm.f\
  $(d0library)/dbl3/general/dbmu_unphv.f\
  $(d0library)/dbl3/general/dbuky.f\
  $(d0library)/dbl3/general/dbume.f\
  $(d0library)/dbl3/general/dukeyd.f\
  $(d0library)/dbl3/general/msgo.f\
  $(d0library)/dbl3/server/d0dbl3_dbinct.f\
  $(d0library)/dbl3/server/d0dbl3_dbpkts.f\
  $(d0library)/dbl3/server/d0dbl3_dbupts.f\
  $(d0root)/test/dbl3/general/d3upt.f
$(d0library)/dbl3/calib/clblnk.f:\
  $(d0library)/dbl3/calib/clblnk.for\
  $(d0library)/params/calib.def\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/lkcalib.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/calib/clblnk.for | vmstounix > $(d0library)/dbl3/calib/clblnk.f
$(d0library)/dbl3/calib/condbl.f:\
  $(d0library)/dbl3/calib/condbl.for\
  $(d0library)/inc/zebdbl.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izstpo.link\
  $(d0library)/links/izstpn.link
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/calib/condbl.for | vmstounix > $(d0library)/dbl3/calib/condbl.f
$(d0library)/dbl3/calib/dbclb_detseq.f:\
  $(d0library)/dbl3/calib/dbclb_detseq.for\
  $(d0library)/inc/dbstp.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/calib/dbclb_detseq.for | vmstounix > $(d0library)/dbl3/calib/dbclb_detseq.f
$(d0library)/dbl3/calib/dbclb_end.f:\
  $(d0library)/dbl3/calib/dbclb_end.for\
  $(d0library)/inc/dbstp.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/calib/dbclb_end.for | vmstounix > $(d0library)/dbl3/calib/dbclb_end.f
$(d0library)/dbl3/calib/dbclb_finish.f:\
  $(d0library)/dbl3/calib/dbclb_finish.for\
  $(d0library)/inc/dbstp.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/calib/dbclb_finish.for | vmstounix > $(d0library)/dbl3/calib/dbclb_finish.f
$(d0library)/dbl3/calib/dbclb_init.f:\
  $(d0library)/dbl3/calib/dbclb_init.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/params/calib.def\
  $(d0library)/inc/lkcalib.inc\
  $(d0library)/inc/dbstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/db_srvr_units.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/calib/dbclb_init.for | vmstounix > $(d0library)/dbl3/calib/dbclb_init.f
$(d0library)/dbl3/calib/dbclb_initialize.f:\
  $(d0library)/dbl3/calib/dbclb_initialize.for\
  $(d0library)/inc/dbstp.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/calib/dbclb_initialize.for | vmstounix > $(d0library)/dbl3/calib/dbclb_initialize.f
$(d0library)/dbl3/calib/dbclb_path.f:\
  $(d0library)/dbl3/calib/dbclb_path.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/calib/dbclb_path.for | vmstounix > $(d0library)/dbl3/calib/dbclb_path.f
$(d0library)/dbl3/calib/dbclb_path_info.f:\
  $(d0library)/dbl3/calib/dbclb_path_info.for\
  $(d0library)/links/izslv0.link\
  $(d0library)/links/izsmuo.link\
  $(d0library)/links/izsvtx.link\
  $(d0library)/links/izscdc.link\
  $(d0library)/links/izsfdc.link\
  $(d0library)/links/izstrd.link\
  $(d0library)/links/izscal.link\
  $(d0library)/links/izsgen.link\
  $(d0library)/links/izssam.link
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/calib/dbclb_path_info.for | vmstounix > $(d0library)/dbl3/calib/dbclb_path_info.f
$(d0library)/dbl3/calib/dbclb_seqdet.f:\
  $(d0library)/dbl3/calib/dbclb_seqdet.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/calib/dbclb_seqdet.for | vmstounix > $(d0library)/dbl3/calib/dbclb_seqdet.f
$(d0library)/dbl3/calib/errdb.f:\
  $(d0library)/dbl3/calib/errdb.for\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/calib/errdb.for | vmstounix > $(d0library)/dbl3/calib/errdb.f
$(d0library)/dbl3/calib/inzdbl.f:\
  $(d0library)/dbl3/calib/inzdbl.for\
  $(d0library)/inc/zebdbl.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/calib/inzdbl.for | vmstounix > $(d0library)/dbl3/calib/inzdbl.f
$(d0library)/dbl3/dboffline/dbclb_fetch_offline.f:\
  $(d0library)/dbl3/dboffline/dbclb_fetch_offline.for\
  $(d0library)/params/calib.def\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zebdbl.inc\
  $(d0library)/inc/lkcalib.inc\
  $(d0library)/inc/dbstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/links/izstpc.link
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/dboffline/dbclb_fetch_offline.for | vmstounix > $(d0library)/dbl3/dboffline/dbclb_fetch_offline.f
$(d0library)/dbl3/dboffline/dbo_get_triggers.f:\
  $(d0library)/dbl3/dboffline/dbo_get_triggers.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/dboffline/dbo_get_triggers.for | vmstounix > $(d0library)/dbl3/dboffline/dbo_get_triggers.f
$(d0library)/dbl3/dboffline/gt_dbfile.f:\
  $(d0library)/dbl3/dboffline/gt_dbfile.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/dboffline/gt_dbfile.for | vmstounix > $(d0library)/dbl3/dboffline/gt_dbfile.f
$(d0library)/dbl3/general/d3u_end.f:\
  $(d0library)/dbl3/general/d3u_end.for\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/d3u_end.for | vmstounix > $(d0library)/dbl3/general/d3u_end.f
$(d0library)/dbl3/general/d3u_fetch.f:\
  $(d0library)/dbl3/general/d3u_fetch.for\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/d3u_fetch.for | vmstounix > $(d0library)/dbl3/general/d3u_fetch.f
$(d0library)/dbl3/general/d3u_fetcm.f:\
  $(d0library)/dbl3/general/d3u_fetcm.for\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/d3u_fetcm.for | vmstounix > $(d0library)/dbl3/general/d3u_fetcm.f
$(d0library)/dbl3/general/d3u_ini.f:\
  $(d0library)/dbl3/general/d3u_ini.for\
  $(d0library)/inc/d3u.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/d3u_ini.for | vmstounix > $(d0library)/dbl3/general/d3u_ini.f
$(d0library)/dbl3/general/d3u_insrt.f:\
  $(d0library)/dbl3/general/d3u_insrt.for\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/d3u_insrt.for | vmstounix > $(d0library)/dbl3/general/d3u_insrt.f
$(d0library)/dbl3/general/d3u_kychk.f:\
  $(d0library)/dbl3/general/d3u_kychk.for\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/d3u_kychk.for | vmstounix > $(d0library)/dbl3/general/d3u_kychk.f
$(d0library)/dbl3/general/d3u_kycls.f:\
  $(d0library)/dbl3/general/d3u_kycls.for\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/d3u_kycls.for | vmstounix > $(d0library)/dbl3/general/d3u_kycls.f
$(d0library)/dbl3/general/d3u_set.f:\
  $(d0library)/dbl3/general/d3u_set.for\
  $(d0library)/inc/d3u.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/d3u_set.for | vmstounix > $(d0library)/dbl3/general/d3u_set.f
$(d0library)/dbl3/general/d3u_start.f:\
  $(d0library)/dbl3/general/d3u_start.for\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/d3u_start.for | vmstounix > $(d0library)/dbl3/general/d3u_start.f
$(d0library)/dbl3/general/d3unit.f:\
  $(d0library)/dbl3/general/d3unit.for\
  $(d0library)/inc/d3u.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/d3unit.for | vmstounix > $(d0library)/dbl3/general/d3unit.f
$(d0library)/dbl3/general/d3utop.f:\
  $(d0library)/dbl3/general/d3utop.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/d3utop.for | vmstounix > $(d0library)/dbl3/general/d3utop.f
$(d0library)/dbl3/general/d3uxh.f:\
  $(d0library)/dbl3/general/d3uxh.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/d3uxh.for | vmstounix > $(d0library)/dbl3/general/d3uxh.f
$(d0library)/dbl3/general/dbmctoh.f:\
  $(d0library)/dbl3/general/dbmctoh.for\
  $(d0library)/params/byte_order.params
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/dbmctoh.for | vmstounix > $(d0library)/dbl3/general/dbmctoh.f
$(d0library)/dbl3/general/dbmu_enddm.f:\
  $(d0library)/dbl3/general/dbmu_enddm.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/dbmu_enddm.for | vmstounix > $(d0library)/dbl3/general/dbmu_enddm.f
$(d0library)/dbl3/general/dbmu_endhv.f:\
  $(d0library)/dbl3/general/dbmu_endhv.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/dbmu_endhv.for | vmstounix > $(d0library)/dbl3/general/dbmu_endhv.f
$(d0library)/dbl3/general/dbmu_file.f:\
  $(d0library)/dbl3/general/dbmu_file.for\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/dbmu_file.for | vmstounix > $(d0library)/dbl3/general/dbmu_file.f
$(d0library)/dbl3/general/dbmu_finf.f:\
  $(d0library)/dbl3/general/dbmu_finf.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/dbmu_finf.for | vmstounix > $(d0library)/dbl3/general/dbmu_finf.f
$(d0library)/dbl3/general/dbmu_fno.f:\
  $(d0library)/dbl3/general/dbmu_fno.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/dbmu_fno.for | vmstounix > $(d0library)/dbl3/general/dbmu_fno.f
$(d0library)/dbl3/general/dbmu_getc.f:\
  $(d0library)/dbl3/general/dbmu_getc.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/dbmu_getc.for | vmstounix > $(d0library)/dbl3/general/dbmu_getc.f
$(d0library)/dbl3/general/dbmu_getdm.f:\
  $(d0library)/dbl3/general/dbmu_getdm.for\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/dbmu_getdm.for | vmstounix > $(d0library)/dbl3/general/dbmu_getdm.f
$(d0library)/dbl3/general/dbmu_gethv.f:\
  $(d0library)/dbl3/general/dbmu_gethv.for\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/dbmu_gethv.for | vmstounix > $(d0library)/dbl3/general/dbmu_gethv.f
$(d0library)/dbl3/general/dbmu_gtfile.f:\
  $(d0library)/dbl3/general/dbmu_gtfile.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/dbmu_gtfile.for | vmstounix > $(d0library)/dbl3/general/dbmu_gtfile.f
$(d0library)/dbl3/general/dbmu_unpdm.f:\
  $(d0library)/dbl3/general/dbmu_unpdm.for\
  $(d0library)/inc/d3u.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/dbmu_unpdm.for | vmstounix > $(d0library)/dbl3/general/dbmu_unpdm.f
$(d0library)/dbl3/general/dbmu_unphv.f:\
  $(d0library)/dbl3/general/dbmu_unphv.for\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/dbmu_unphv.for | vmstounix > $(d0library)/dbl3/general/dbmu_unphv.f
$(d0library)/dbl3/general/dbuky.f:\
  $(d0library)/dbl3/general/dbuky.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/dbuky.for | vmstounix > $(d0library)/dbl3/general/dbuky.f
$(d0library)/dbl3/general/dbume.f:\
  $(d0library)/dbl3/general/dbume.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/dbume.for | vmstounix > $(d0library)/dbl3/general/dbume.f
$(d0library)/dbl3/general/dukeyd.f:\
  $(d0library)/dbl3/general/dukeyd.for\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/dukeyd.for | vmstounix > $(d0library)/dbl3/general/dukeyd.f
$(d0library)/dbl3/general/msgo.f:\
  $(d0library)/dbl3/general/msgo.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/general/msgo.for | vmstounix > $(d0library)/dbl3/general/msgo.f
$(d0library)/dbl3/server/d0dbl3_dbinct.f:\
  $(d0library)/dbl3/server/d0dbl3_dbinct.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/server/d0dbl3_dbinct.for | vmstounix > $(d0library)/dbl3/server/d0dbl3_dbinct.f
$(d0library)/dbl3/server/d0dbl3_dbpkts.f:\
  $(d0library)/dbl3/server/d0dbl3_dbpkts.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/server/d0dbl3_dbpkts.for | vmstounix > $(d0library)/dbl3/server/d0dbl3_dbpkts.f
$(d0library)/dbl3/server/d0dbl3_dbupts.f:\
  $(d0library)/dbl3/server/d0dbl3_dbupts.for
	tasteofd0 $(FLAVOR) < $(d0library)/dbl3/server/d0dbl3_dbupts.for | vmstounix > $(d0library)/dbl3/server/d0dbl3_dbupts.f
$(d0root)/test/dbl3/general/d3upt.f:\
  $(d0root)/test/dbl3/general/d3upt.for
	tasteofd0 $(FLAVOR) < $(d0root)/test/dbl3/general/d3upt.for | vmstounix > $(d0root)/test/dbl3/general/d3upt.f
for:
	touch $(d0library)/dbl3/calib/clblnk.for
	touch $(d0library)/dbl3/calib/condbl.for
	touch $(d0library)/dbl3/calib/dbclb_detseq.for
	touch $(d0library)/dbl3/calib/dbclb_end.for
	touch $(d0library)/dbl3/calib/dbclb_finish.for
	touch $(d0library)/dbl3/calib/dbclb_init.for
	touch $(d0library)/dbl3/calib/dbclb_initialize.for
	touch $(d0library)/dbl3/calib/dbclb_path.for
	touch $(d0library)/dbl3/calib/dbclb_path_info.for
	touch $(d0library)/dbl3/calib/dbclb_seqdet.for
	touch $(d0library)/dbl3/calib/errdb.for
	touch $(d0library)/dbl3/calib/inzdbl.for
	touch $(d0library)/dbl3/dboffline/dbclb_fetch_offline.for
	touch $(d0library)/dbl3/dboffline/dbo_get_triggers.for
	touch $(d0library)/dbl3/dboffline/gt_dbfile.for
	touch $(d0library)/dbl3/general/d3u_end.for
	touch $(d0library)/dbl3/general/d3u_fetch.for
	touch $(d0library)/dbl3/general/d3u_fetcm.for
	touch $(d0library)/dbl3/general/d3u_ini.for
	touch $(d0library)/dbl3/general/d3u_insrt.for
	touch $(d0library)/dbl3/general/d3u_kychk.for
	touch $(d0library)/dbl3/general/d3u_kycls.for
	touch $(d0library)/dbl3/general/d3u_set.for
	touch $(d0library)/dbl3/general/d3u_start.for
	touch $(d0library)/dbl3/general/d3unit.for
	touch $(d0library)/dbl3/general/d3utop.for
	touch $(d0library)/dbl3/general/d3uxh.for
	touch $(d0library)/dbl3/general/dbmctoh.for
	touch $(d0library)/dbl3/general/dbmu_enddm.for
	touch $(d0library)/dbl3/general/dbmu_endhv.for
	touch $(d0library)/dbl3/general/dbmu_file.for
	touch $(d0library)/dbl3/general/dbmu_finf.for
	touch $(d0library)/dbl3/general/dbmu_fno.for
	touch $(d0library)/dbl3/general/dbmu_getc.for
	touch $(d0library)/dbl3/general/dbmu_getdm.for
	touch $(d0library)/dbl3/general/dbmu_gethv.for
	touch $(d0library)/dbl3/general/dbmu_gtfile.for
	touch $(d0library)/dbl3/general/dbmu_unpdm.for
	touch $(d0library)/dbl3/general/dbmu_unphv.for
	touch $(d0library)/dbl3/general/dbuky.for
	touch $(d0library)/dbl3/general/dbume.for
	touch $(d0library)/dbl3/general/dukeyd.for
	touch $(d0library)/dbl3/general/msgo.for
	touch $(d0library)/dbl3/server/d0dbl3_dbinct.for
	touch $(d0library)/dbl3/server/d0dbl3_dbpkts.for
	touch $(d0library)/dbl3/server/d0dbl3_dbupts.for
	touch $(d0root)/test/dbl3/general/d3upt.for
