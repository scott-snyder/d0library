SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Olimit 2500 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/admin/tmp/d0librar/mmstomake/cdc_util
FLAVOR = SIUNIX
F77 = f77
.IGNORE:
.SUFFIXES:

opt :\
  cdc_util.a
cdc_util.a : $(d0root)/cdc_util/cdc_util.a
	@ echo cdc_util.a is up to date
$(d0root)/cdc_util/cdc_util.a:: $(SCRATCH)/cdc_util/nothing.nl
$(d0root)/cdc_util/cdc_util.a::\
  $(d0root)/cdc_util/cdc_util.a(bkcdch.o)\
  $(d0root)/cdc_util/cdc_util.a(bkcdd2.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdcda.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdgbd.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdgch.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdgnh.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdgns.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdhit.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdlyr.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdpbd.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdpch.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdpdh.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdped.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdpzs.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdsec.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdtbd.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdtch.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdtmh.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdtms.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdtrh.o)\
  $(d0root)/cdc_util/cdc_util.a(bkdtrk.o)\
  $(d0root)/cdc_util/cdc_util.a(bkpsht.o)\
  $(d0root)/cdc_util/cdc_util.a(bkpsrw.o)\
  $(d0root)/cdc_util/cdc_util.a(bkrtst.o)\
  $(d0root)/cdc_util/cdc_util.a(bkscdc.o)\
  $(d0root)/cdc_util/cdc_util.a(bkt0ht.o)\
  $(d0root)/cdc_util/cdc_util.a(bkt0th.o)\
  $(d0root)/cdc_util/cdc_util.a(bkupgd.o)\
  $(d0root)/cdc_util/cdc_util.a(bkzdht.o)\
  $(d0root)/cdc_util/cdc_util.a(cdalgn.o)\
  $(d0root)/cdc_util/cdc_util.a(cdc_exm_anal.o)\
  $(d0root)/cdc_util/cdc_util.a(cdc_fix.o)\
  $(d0root)/cdc_util/cdc_util.a(cdc_hatch681.o)\
  $(d0root)/cdc_util/cdc_util.a(cdc_hst_menu.o)\
  $(d0root)/cdc_util/cdc_util.a(cdchit_cell.o)\
  $(d0root)/cdc_util/cdc_util.a(cdchit_hits.o)\
  $(d0root)/cdc_util/cdc_util.a(cdchit_info.o)\
  $(d0root)/cdc_util/cdc_util.a(cdchit_segm.o)\
  $(d0root)/cdc_util/cdc_util.a(cdchit_strip.o)\
  $(d0root)/cdc_util/cdc_util.a(cdcisa.o)\
  $(d0root)/cdc_util/cdc_util.a(cdclos.o)\
  $(d0root)/cdc_util/cdc_util.a(cdcvel.o)\
  $(d0root)/cdc_util/cdc_util.a(cdgetz.o)\
  $(d0root)/cdc_util/cdc_util.a(cdhfil.o)\
  $(d0root)/cdc_util/cdc_util.a(cdhitl.o)\
  $(d0root)/cdc_util/cdc_util.a(cdhits.o)\
  $(d0root)/cdc_util/cdc_util.a(cdhtrk.o)\
  $(d0root)/cdc_util/cdc_util.a(cdinit.o)\
  $(d0root)/cdc_util/cdc_util.a(cdistp.o)\
  $(d0root)/cdc_util/cdc_util.a(cdistr.o)\
  $(d0root)/cdc_util/cdc_util.a(cdnctr.o)\
  $(d0root)/cdc_util/cdc_util.a(cdpuls.o)\
  $(d0root)/cdc_util/cdc_util.a(cdroad.o)\
  $(d0root)/cdc_util/cdc_util.a(cdsave.o)\
  $(d0root)/cdc_util/cdc_util.a(cdsres.o)\
  $(d0root)/cdc_util/cdc_util.a(cdtfit.o)\
  $(d0root)/cdc_util/cdc_util.a(cdtrak.o)\
  $(d0root)/cdc_util/cdc_util.a(cdtseg.o)\
  $(d0root)/cdc_util/cdc_util.a(cdtsto.o)\
  $(d0root)/cdc_util/cdc_util.a(cdunpk.o)\
  $(d0root)/cdc_util/cdc_util.a(cdxyzl.o)\
  $(d0root)/cdc_util/cdc_util.a(crhitl.o)\
  $(d0root)/cdc_util/cdc_util.a(crhits.o)\
  $(d0root)/cdc_util/cdc_util.a(crtseg.o)\
  $(d0root)/cdc_util/cdc_util.a(darearect368.o)\
  $(d0root)/cdc_util/cdc_util.a(dbkchd.o)\
  $(d0root)/cdc_util/cdc_util.a(dbkchi.o)\
  $(d0root)/cdc_util/cdc_util.a(dbkchz.o)\
  $(d0root)/cdc_util/cdc_util.a(dbkdex.o)\
  $(d0root)/cdc_util/cdc_util.a(dbkdlr.o)\
  $(d0root)/cdc_util/cdc_util.a(dbkdsc.o)\
  $(d0root)/cdc_util/cdc_util.a(dbkeff.o)\
  $(d0root)/cdc_util/cdc_util.a(dbkped.o)\
  $(d0root)/cdc_util/cdc_util.a(dbkpul.o)\
  $(d0root)/cdc_util/cdc_util.a(dbkswr.o)\
  $(d0root)/cdc_util/cdc_util.a(dbnchk.o)\
  $(d0root)/cdc_util/cdc_util.a(dc_envadj.o)\
  $(d0root)/cdc_util/cdc_util.a(dcdedx.o)\
  $(d0root)/cdc_util/cdc_util.a(dchkt_edge.o)\
  $(d0root)/cdc_util/cdc_util.a(dchkz_edge.o)\
  $(d0root)/cdc_util/cdc_util.a(dclimb.o)\
  $(d0root)/cdc_util/cdc_util.a(dcoder.o)\
  $(d0root)/cdc_util/cdc_util.a(dcrtmp.o)\
  $(d0root)/cdc_util/cdc_util.a(ddbini.o)\
  $(d0root)/cdc_util/cdc_util.a(ddelet.o)\
  $(d0root)/cdc_util/cdc_util.a(dfitch.o)\
  $(d0root)/cdc_util/cdc_util.a(dflhst.o)\
  $(d0root)/cdc_util/cdc_util.a(dflped.o)\
  $(d0root)/cdc_util/cdc_util.a(dflpul.o)\
  $(d0root)/cdc_util/cdc_util.a(dflseg.o)\
  $(d0root)/cdc_util/cdc_util.a(dfltrk.o)\
  $(d0root)/cdc_util/cdc_util.a(dftlin.o)\
  $(d0root)/cdc_util/cdc_util.a(dgnini.o)\
  $(d0root)/cdc_util/cdc_util.a(dgtgns.o)\
  $(d0root)/cdc_util/cdc_util.a(dgtpds.o)\
  $(d0root)/cdc_util/cdc_util.a(dgttms.o)\
  $(d0root)/cdc_util/cdc_util.a(dhit_data565.o)\
  $(d0root)/cdc_util/cdc_util.a(dhit_unpack.o)\
  $(d0root)/cdc_util/cdc_util.a(dhitfl.o)\
  $(d0root)/cdc_util/cdc_util.a(dhitst.o)\
  $(d0root)/cdc_util/cdc_util.a(dhstbk.o)\
  $(d0root)/cdc_util/cdc_util.a(dhstfl.o)\
  $(d0root)/cdc_util/cdc_util.a(dhstou.o)\
  $(d0root)/cdc_util/cdc_util.a(dhtchk.o)\
  $(d0root)/cdc_util/cdc_util.a(disa_mark.o)\
  $(d0root)/cdc_util/cdc_util.a(dldtsg.o)\
  $(d0root)/cdc_util/cdc_util.a(dlinfo.o)\
  $(d0root)/cdc_util/cdc_util.a(dmdstg.o)\
  $(d0root)/cdc_util/cdc_util.a(dmonpl.o)\
  $(d0root)/cdc_util/cdc_util.a(dnlcor.o)\
  $(d0root)/cdc_util/cdc_util.a(dpdini.o)\
  $(d0root)/cdc_util/cdc_util.a(drddbg.o)\
  $(d0root)/cdc_util/cdc_util.a(dsec_dhit476.o)\
  $(d0root)/cdc_util/cdc_util.a(dsecht.o)\
  $(d0root)/cdc_util/cdc_util.a(dseglt.o)\
  $(d0root)/cdc_util/cdc_util.a(dsegme.o)\
  $(d0root)/cdc_util/cdc_util.a(dsgsec.o)\
  $(d0root)/cdc_util/cdc_util.a(dstp_fetch.o)\
  $(d0root)/cdc_util/cdc_util.a(dsvchn.o)\
  $(d0root)/cdc_util/cdc_util.a(dtdefd.o)\
  $(d0root)/cdc_util/cdc_util.a(dtlink.o)\
  $(d0root)/cdc_util/cdc_util.a(dtmini.o)\
  $(d0root)/cdc_util/cdc_util.a(dtrdia.o)\
  $(d0root)/cdc_util/cdc_util.a(dtrdmp.o)\
  $(d0root)/cdc_util/cdc_util.a(dtrdrp.o)\
  $(d0root)/cdc_util/cdc_util.a(dtrevt.o)\
  $(d0root)/cdc_util/cdc_util.a(dtrhbk.o)\
  $(d0root)/cdc_util/cdc_util.a(dtrini.o)\
  $(d0root)/cdc_util/cdc_util.a(dtrk_edge.o)\
  $(d0root)/cdc_util/cdc_util.a(dtrk_fix.o)\
  $(d0root)/cdc_util/cdc_util.a(dtrk_dedx385.o)\
  $(d0root)/cdc_util/cdc_util.a(dtrk_fixz.o)\
  $(d0root)/cdc_util/cdc_util.a(dtrkhits.o)\
  $(d0root)/cdc_util/cdc_util.a(dtrkht.o)\
  $(d0root)/cdc_util/cdc_util.a(dtrksg.o)\
  $(d0root)/cdc_util/cdc_util.a(dtrpar.o)\
  $(d0root)/cdc_util/cdc_util.a(dtrusm.o)\
  $(d0root)/cdc_util/cdc_util.a(dtsg_z.o)\
  $(d0root)/cdc_util/cdc_util.a(dxyerr.o)\
  $(d0root)/cdc_util/cdc_util.a(fill_list848.o)\
  $(d0root)/cdc_util/cdc_util.a(fill_list397.o)\
  $(d0root)/cdc_util/cdc_util.a(fldsec.o)\
  $(d0root)/cdc_util/cdc_util.a(gtdhit.o)\
  $(d0root)/cdc_util/cdc_util.a(gtditr.o)\
  $(d0root)/cdc_util/cdc_util.a(gtdtrh.o)\
  $(d0root)/cdc_util/cdc_util.a(gtdtrk.o)\
  $(d0root)/cdc_util/cdc_util.a(gtdtrk_link.o)\
  $(d0root)/cdc_util/cdc_util.a(gzcdch.o)\
  $(d0root)/cdc_util/cdc_util.a(gzcdd2.o)\
  $(d0root)/cdc_util/cdc_util.a(gzcdh2.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdalh.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdall.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdals.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdcbd.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdcda.o)\
  $(d0root)/cdc_util/cdc_util.a(gzddlp.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdgeh.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdgnh.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdgnl.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdhit.o)\
  $(d0root)/cdc_util/cdc_util.a(gzditr.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdlyr.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdmat.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdnli.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdnlo.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdpdh.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdpdl.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdrft.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdsec.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdswp.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdtmd.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdtmh.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdtmw.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdtrh.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdtrk.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdtsg.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdtva.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdtvh.o)\
  $(d0root)/cdc_util/cdc_util.a(gzdwal.o)\
  $(d0root)/cdc_util/cdc_util.a(gzl2ch.o)\
  $(d0root)/cdc_util/cdc_util.a(gzpsht.o)\
  $(d0root)/cdc_util/cdc_util.a(gzpsrw.o)\
  $(d0root)/cdc_util/cdc_util.a(gzrtst.o)\
  $(d0root)/cdc_util/cdc_util.a(gzscdc.o)\
  $(d0root)/cdc_util/cdc_util.a(gzt0dh.o)\
  $(d0root)/cdc_util/cdc_util.a(gzt0fh.o)\
  $(d0root)/cdc_util/cdc_util.a(gzt0ht.o)\
  $(d0root)/cdc_util/cdc_util.a(gzt0rw.o)\
  $(d0root)/cdc_util/cdc_util.a(gzt0td.o)\
  $(d0root)/cdc_util/cdc_util.a(gzt0th.o)\
  $(d0root)/cdc_util/cdc_util.a(gzt0tr.o)\
  $(d0root)/cdc_util/cdc_util.a(gzt0tz.o)\
  $(d0root)/cdc_util/cdc_util.a(gzupgd.o)\
  $(d0root)/cdc_util/cdc_util.a(gzzdht.o)\
  $(d0root)/cdc_util/cdc_util.a(gzzdrw.o)\
  $(d0root)/cdc_util/cdc_util.a(hbaver.o)\
  $(d0root)/cdc_util/cdc_util.a(icell.o)\
  $(d0root)/cdc_util/cdc_util.a(lcheck.o)\
  $(d0root)/cdc_util/cdc_util.a(nwires_hit.o)\
  $(d0root)/cdc_util/cdc_util.a(prcdch.o)\
  $(d0root)/cdc_util/cdc_util.a(prcdd2.o)\
  $(d0root)/cdc_util/cdc_util.a(prdcda.o)\
  $(d0root)/cdc_util/cdc_util.a(prditr.o)\
  $(d0root)/cdc_util/cdc_util.a(prdsec.o)\
  $(d0root)/cdc_util/cdc_util.a(prdtrh.o)\
  $(d0root)/cdc_util/cdc_util.a(prdtrk.o)\
  $(d0root)/cdc_util/cdc_util.a(prdtsg.o)\
  $(d0root)/cdc_util/cdc_util.a(prrtst.o)\
  $(d0root)/cdc_util/cdc_util.a(prt0dh.o)\
  $(d0root)/cdc_util/cdc_util.a(prt0ht.o)\
  $(d0root)/cdc_util/cdc_util.a(prt0rw.o)\
  $(d0root)/cdc_util/cdc_util.a(prt0td.o)\
  $(d0root)/cdc_util/cdc_util.a(prt0th.o)\
  $(d0root)/cdc_util/cdc_util.a(prt0tz.o)\
  $(d0root)/cdc_util/cdc_util.a(psrwfl.o)\
  $(d0root)/cdc_util/cdc_util.a(pudcda.o)\
  $(d0root)/cdc_util/cdc_util.a(puditr.o)\
  $(d0root)/cdc_util/cdc_util.a(pudsec.o)\
  $(d0root)/cdc_util/cdc_util.a(recdd2.o)\
  $(d0root)/cdc_util/cdc_util.a(rtstfl.o)\
  $(d0root)/cdc_util/cdc_util.a(squeeze.o)\
  $(d0root)/cdc_util/cdc_util.a(t0devt.o)\
  $(d0root)/cdc_util/cdc_util.a(t0dhfl.o)\
  $(d0root)/cdc_util/cdc_util.a(t0dini.o)\
  $(d0root)/cdc_util/cdc_util.a(t0dpar.o)\
  $(d0root)/cdc_util/cdc_util.a(t0dtrk.o)\
  $(d0root)/cdc_util/cdc_util.a(t0hitc.o)\
  $(d0root)/cdc_util/cdc_util.a(t0hits.o)\
  $(d0root)/cdc_util/cdc_util.a(t0rwfl.o)\
  $(d0root)/cdc_util/cdc_util.a(t0tdfl.o)\
  $(d0root)/cdc_util/cdc_util.a(t0trks.o)\
  $(d0root)/cdc_util/cdc_util.a(t0tzfl.o)\
  $(d0root)/cdc_util/cdc_util.a(zcdevt.o)\
  $(d0root)/cdc_util/cdc_util.a(zcdhits.o)\
  $(d0root)/cdc_util/cdc_util.a(zcdini.o)\
  $(d0root)/cdc_util/cdc_util.a(zdrwfl.o)\
  $(d0root)/cdc_util/cdc_util.a(zfdcda.o)\
  $(d0root)/cdc_util/cdc_util.a(zfditr.o)\
  $(d0root)/cdc_util/cdc_util.a(zfdsec.o)\
  $(d0root)/cdc_util/cdc_util.a(zfdtrk.o)\
  $(d0root)/cdc_util/cdc_util.a(zfdtsg.o)\
  $(d0root)/cdc_util/cdc_util.a(zgdcda.o)\
  $(d0root)/cdc_util/cdc_util.a(zgdsec.o)\
  $(d0root)/cdc_util/cdc_util.a(zgdtsg.o)\
  $(d0root)/cdc_util/cdc_util.a(zhit.o)
	ar $(ARFLAGS) $(d0root)/cdc_util/cdc_util.a `ls $(SCRATCH)/cdc_util/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/cdc_util/cdc_util.a `ls $(SCRATCH)/cdc_util/a2/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/cdc_util/cdc_util.a `ls $(SCRATCH)/cdc_util/a3/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/cdc_util/cdc_util.a `ls $(SCRATCH)/cdc_util/a4/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/cdc_util/cdc_util.a `ls $(SCRATCH)/cdc_util/a5/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/cdc_util/cdc_util.a `ls $(SCRATCH)/cdc_util/a6/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/cdc_util/cdc_util.a `ls $(SCRATCH)/cdc_util/a7/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/cdc_util/cdc_util.a `ls $(SCRATCH)/cdc_util/a8/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/cdc_util/cdc_util.a `ls $(SCRATCH)/cdc_util/a9/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/cdc_util/cdc_util.a `ls $(SCRATCH)/cdc_util/a10/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/cdc_util/cdc_util.a `ls $(SCRATCH)/cdc_util/a11/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/cdc_util/cdc_util.a `ls $(SCRATCH)/cdc_util/a12/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/cdc_util/cdc_util.a `ls $(SCRATCH)/cdc_util/a13/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/cdc_util
$(SCRATCH)/cdc_util/nothing.nl:
	rmdirp $(SCRATCH)/cdc_util
	mkdirp $(SCRATCH)/cdc_util/a1
	mkdirp $(SCRATCH)/cdc_util/a2
	mkdirp $(SCRATCH)/cdc_util/a3
	mkdirp $(SCRATCH)/cdc_util/a4
	mkdirp $(SCRATCH)/cdc_util/a5
	mkdirp $(SCRATCH)/cdc_util/a6
	mkdirp $(SCRATCH)/cdc_util/a7
	mkdirp $(SCRATCH)/cdc_util/a8
	mkdirp $(SCRATCH)/cdc_util/a9
	mkdirp $(SCRATCH)/cdc_util/a10
	mkdirp $(SCRATCH)/cdc_util/a11
	mkdirp $(SCRATCH)/cdc_util/a12
	mkdirp $(SCRATCH)/cdc_util/a13
$(d0root)/cdc_util/cdc_util.a(bkcdch.o):\
  $(d0root)/cdc_util/source/bkcdch.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/links/izcdch.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkcdch.f ;\
	mv bkcdch.o $(SCRATCH)/cdc_util/a1/bkcdch.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkcdd2.o):\
  $(d0root)/cdc_util/source/bkcdd2.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcdd2.link\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkcdd2.f ;\
	mv bkcdd2.o $(SCRATCH)/cdc_util/a1/bkcdd2.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdcda.o):\
  $(d0root)/cdc_util/source/bkdcda.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdcda.f ;\
	mv bkdcda.o $(SCRATCH)/cdc_util/a1/bkdcda.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdgbd.o):\
  $(d0root)/cdc_util/source/bkdgbd.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdgbd.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdgbd.f ;\
	mv bkdgbd.o $(SCRATCH)/cdc_util/a1/bkdgbd.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdgch.o):\
  $(d0root)/cdc_util/source/bkdgch.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdgnh.link\
  $(d0library)/links/izdgch.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdgch.f ;\
	mv bkdgch.o $(SCRATCH)/cdc_util/a1/bkdgch.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdgnh.o):\
  $(d0root)/cdc_util/source/bkdgnh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdgnh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdgnh.f ;\
	mv bkdgnh.o $(SCRATCH)/cdc_util/a1/bkdgnh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdgns.o):\
  $(d0root)/cdc_util/source/bkdgns.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdgns.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdgns.f ;\
	mv bkdgns.o $(SCRATCH)/cdc_util/a1/bkdgns.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdhit.o):\
  $(d0root)/cdc_util/source/bkdhit.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/links/izdhit.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdhit.f ;\
	mv bkdhit.o $(SCRATCH)/cdc_util/a1/bkdhit.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdlyr.o):\
  $(d0root)/cdc_util/source/bkdlyr.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/links/izdlyr.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdlyr.f ;\
	mv bkdlyr.o $(SCRATCH)/cdc_util/a1/bkdlyr.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdpbd.o):\
  $(d0root)/cdc_util/source/bkdpbd.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdpbd.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdpbd.f ;\
	mv bkdpbd.o $(SCRATCH)/cdc_util/a1/bkdpbd.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdpch.o):\
  $(d0root)/cdc_util/source/bkdpch.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdpdh.link\
  $(d0library)/links/izdpch.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdpch.f ;\
	mv bkdpch.o $(SCRATCH)/cdc_util/a1/bkdpch.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdpdh.o):\
  $(d0root)/cdc_util/source/bkdpdh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdpdh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdpdh.f ;\
	mv bkdpdh.o $(SCRATCH)/cdc_util/a1/bkdpdh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdped.o):\
  $(d0root)/cdc_util/source/bkdped.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdped.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdped.f ;\
	mv bkdped.o $(SCRATCH)/cdc_util/a1/bkdped.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdpzs.o):\
  $(d0root)/cdc_util/source/bkdpzs.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdpzs.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdpzs.f ;\
	mv bkdpzs.o $(SCRATCH)/cdc_util/a1/bkdpzs.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdsec.o):\
  $(d0root)/cdc_util/source/bkdsec.f\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izdsec.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdsec.f ;\
	mv bkdsec.o $(SCRATCH)/cdc_util/a1/bkdsec.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdtbd.o):\
  $(d0root)/cdc_util/source/bkdtbd.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdtbd.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdtbd.f ;\
	mv bkdtbd.o $(SCRATCH)/cdc_util/a1/bkdtbd.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdtch.o):\
  $(d0root)/cdc_util/source/bkdtch.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdtmh.link\
  $(d0library)/links/izdtch.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdtch.f ;\
	mv bkdtch.o $(SCRATCH)/cdc_util/a1/bkdtch.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdtmh.o):\
  $(d0root)/cdc_util/source/bkdtmh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdtmh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdtmh.f ;\
	mv bkdtmh.o $(SCRATCH)/cdc_util/a1/bkdtmh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdtms.o):\
  $(d0root)/cdc_util/source/bkdtms.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdtms.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdtms.f ;\
	mv bkdtms.o $(SCRATCH)/cdc_util/a1/bkdtms.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdtrh.o):\
  $(d0root)/cdc_util/source/bkdtrh.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/links/izztrh.link\
  $(d0library)/links/izdtrh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdtrh.f ;\
	mv bkdtrh.o $(SCRATCH)/cdc_util/a1/bkdtrh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkdtrk.o):\
  $(d0root)/cdc_util/source/bkdtrk.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izdtrk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkdtrk.f ;\
	mv bkdtrk.o $(SCRATCH)/cdc_util/a2/bkdtrk.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkpsht.o):\
  $(d0root)/cdc_util/source/bkpsht.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpsht.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkpsht.f ;\
	mv bkpsht.o $(SCRATCH)/cdc_util/a2/bkpsht.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkpsrw.o):\
  $(d0root)/cdc_util/source/bkpsrw.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpsrw.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkpsrw.f ;\
	mv bkpsrw.o $(SCRATCH)/cdc_util/a2/bkpsrw.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkrtst.o):\
  $(d0root)/cdc_util/source/bkrtst.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izrtst.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkrtst.f ;\
	mv bkrtst.o $(SCRATCH)/cdc_util/a2/bkrtst.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkscdc.o):\
  $(d0root)/cdc_util/source/bkscdc.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izstpo.link\
  $(d0library)/links/izstpn.link\
  $(d0library)/links/izscdc.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkscdc.f ;\
	mv bkscdc.o $(SCRATCH)/cdc_util/a2/bkscdc.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkt0ht.o):\
  $(d0root)/cdc_util/source/bkt0ht.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0dlnt2.inc\
  $(d0library)/links/izt0ht.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkt0ht.f ;\
	mv bkt0ht.o $(SCRATCH)/cdc_util/a2/bkt0ht.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkt0th.o):\
  $(d0root)/cdc_util/source/bkt0th.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0dlnt2.inc\
  $(d0library)/links/izt0th.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkt0th.f ;\
	mv bkt0th.o $(SCRATCH)/cdc_util/a2/bkt0th.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkupgd.o):\
  $(d0root)/cdc_util/source/bkupgd.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izupgd.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkupgd.f ;\
	mv bkupgd.o $(SCRATCH)/cdc_util/a2/bkupgd.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(bkzdht.o):\
  $(d0root)/cdc_util/source/bkzdht.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zcdln2.inc\
  $(d0library)/links/izzdht.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/bkzdht.f ;\
	mv bkzdht.o $(SCRATCH)/cdc_util/a2/bkzdht.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdalgn.o):\
  $(d0root)/cdc_util/source/cdalgn.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdresf.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdalgn.f ;\
	mv cdalgn.o $(SCRATCH)/cdc_util/a2/cdalgn.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdc_exm_anal.o):\
  $(d0root)/cdc_util/source/cdc_exm_anal.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdc_exm_anal.f ;\
	mv cdc_exm_anal.o $(SCRATCH)/cdc_util/a2/cdc_exm_anal.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdc_fix.o):\
  $(d0root)/cdc_util/source/cdc_fix.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdc_fix.f ;\
	mv cdc_fix.o $(SCRATCH)/cdc_util/a2/cdc_fix.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdc_hatch681.o):\
  $(d0root)/cdc_util/source/cdc_hst_dispatch.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdc_hst_dispatch.f ;\
	mv cdc_hst_dispatch.o $(SCRATCH)/cdc_util/a2/cdc_hatch681.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdc_hst_menu.o):\
  $(d0root)/cdc_util/source/cdc_hst_menu.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdc_hst_menu.f ;\
	mv cdc_hst_menu.o $(SCRATCH)/cdc_util/a2/cdc_hst_menu.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdchit_cell.o):\
  $(d0root)/cdc_util/source/cdchit_cell.f\
  $(d0library)/inc/pi.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdchit_cell.f ;\
	mv cdchit_cell.o $(SCRATCH)/cdc_util/a2/cdchit_cell.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdchit_hits.o):\
  $(d0root)/cdc_util/source/cdchit_hits.f\
  $(d0library)/inc/pi.inc\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdchit_hits.f ;\
	mv cdchit_hits.o $(SCRATCH)/cdc_util/a2/cdchit_hits.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdchit_info.o):\
  $(d0root)/cdc_util/source/cdchit_info.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdchit_info.f ;\
	mv cdchit_info.o $(SCRATCH)/cdc_util/a2/cdchit_info.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdchit_segm.o):\
  $(d0root)/cdc_util/source/cdchit_segm.f\
  $(d0library)/inc/pi.inc\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdchit_segm.f ;\
	mv cdchit_segm.o $(SCRATCH)/cdc_util/a2/cdchit_segm.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdchit_strip.o):\
  $(d0root)/cdc_util/source/cdchit_strip.f\
  $(d0library)/inc/pi.inc\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdchit_strip.f ;\
	mv cdchit_strip.o $(SCRATCH)/cdc_util/a2/cdchit_strip.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdcisa.o):\
  $(d0root)/cdc_util/source/cdcisa.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/links/izdrft.link\
  $(d0library)/inc/cdltrk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdcisa.f ;\
	mv cdcisa.o $(SCRATCH)/cdc_util/a2/cdcisa.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdclos.o):\
  $(d0root)/cdc_util/source/cdclos.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdclos.f ;\
	mv cdclos.o $(SCRATCH)/cdc_util/a3/cdclos.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdcvel.o):\
  $(d0root)/cdc_util/source/cdcvel.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izdtth.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdcvel.f ;\
	mv cdcvel.o $(SCRATCH)/cdc_util/a3/cdcvel.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdgetz.o):\
  $(d0root)/cdc_util/source/cdgetz.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdresf.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdgetz.f ;\
	mv cdgetz.o $(SCRATCH)/cdc_util/a3/cdgetz.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdhfil.o):\
  $(d0root)/cdc_util/source/cdhfil.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdresf.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdhfil.f ;\
	mv cdhfil.o $(SCRATCH)/cdc_util/a3/cdhfil.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdhitl.o):\
  $(d0root)/cdc_util/source/cdhitl.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/links/izuser.link\
  $(d0library)/links/izdalh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdhitl.f ;\
	mv cdhitl.o $(SCRATCH)/cdc_util/a3/cdhitl.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdhits.o):\
  $(d0root)/cdc_util/source/cdhits.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdccos.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdhits.f ;\
	mv cdhits.o $(SCRATCH)/cdc_util/a3/cdhits.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdhtrk.o):\
  $(d0root)/cdc_util/source/cdhtrk.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdhtrk.f ;\
	mv cdhtrk.o $(SCRATCH)/cdc_util/a3/cdhtrk.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdinit.o):\
  $(d0root)/cdc_util/source/cdinit.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/ddebug.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdinit.f ;\
	mv cdinit.o $(SCRATCH)/cdc_util/a3/cdinit.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdistp.o):\
  $(d0root)/cdc_util/source/cdistp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izscdc.link\
  $(d0library)/links/izdpdh.link\
  $(d0library)/links/izdgnh.link\
  $(d0library)/links/izdtmh.link\
  $(d0library)/links/izdgeh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdistp.f ;\
	mv cdistp.o $(SCRATCH)/cdc_util/a3/cdistp.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdistr.o):\
  $(d0root)/cdc_util/source/cdistr.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisv1.link\
  $(d0library)/links/izisv2.link\
  $(d0library)/links/izisp1.link\
  $(d0library)/links/izisp2.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdistr.f ;\
	mv cdistr.o $(SCRATCH)/cdc_util/a3/cdistr.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdnctr.o):\
  $(d0root)/cdc_util/source/cdnctr.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdnctr.f ;\
	mv cdnctr.o $(SCRATCH)/cdc_util/a3/cdnctr.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdpuls.o):\
  $(d0root)/cdc_util/source/cdpuls.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdresf.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/links/izcdd2.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdpuls.f ;\
	mv cdpuls.o $(SCRATCH)/cdc_util/a3/cdpuls.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdroad.o):\
  $(d0root)/cdc_util/source/cdroad.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdroad.f ;\
	mv cdroad.o $(SCRATCH)/cdc_util/a3/cdroad.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdsave.o):\
  $(d0root)/cdc_util/source/cdsave.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/links/izdalh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdsave.f ;\
	mv cdsave.o $(SCRATCH)/cdc_util/a3/cdsave.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdsres.o):\
  $(d0root)/cdc_util/source/cdsres.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdresf.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdsres.f ;\
	mv cdsres.o $(SCRATCH)/cdc_util/a3/cdsres.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdtfit.o):\
  $(d0root)/cdc_util/source/cdtfit.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdtfit.f ;\
	mv cdtfit.o $(SCRATCH)/cdc_util/a3/cdtfit.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdtrak.o):\
  $(d0root)/cdc_util/source/cdtrak.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdresf.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdtrak.f ;\
	mv cdtrak.o $(SCRATCH)/cdc_util/a3/cdtrak.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdtseg.o):\
  $(d0root)/cdc_util/source/cdtseg.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdtseg.f ;\
	mv cdtseg.o $(SCRATCH)/cdc_util/a3/cdtseg.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdtsto.o):\
  $(d0root)/cdc_util/source/cdtsto.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdtsto.f ;\
	mv cdtsto.o $(SCRATCH)/cdc_util/a3/cdtsto.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdunpk.o):\
  $(d0root)/cdc_util/source/cdunpk.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/ddebug.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdunpk.f ;\
	mv cdunpk.o $(SCRATCH)/cdc_util/a3/cdunpk.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(cdxyzl.o):\
  $(d0root)/cdc_util/source/cdxyzl.f\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/cdxyzl.f ;\
	mv cdxyzl.o $(SCRATCH)/cdc_util/a4/cdxyzl.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(crhitl.o):\
  $(d0root)/cdc_util/source/crhitl.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/links/izuser.link\
  $(d0library)/links/izdalh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/crhitl.f ;\
	mv crhitl.o $(SCRATCH)/cdc_util/a4/crhitl.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(crhits.o):\
  $(d0root)/cdc_util/source/crhits.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/crhits.f ;\
	mv crhits.o $(SCRATCH)/cdc_util/a4/crhits.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(crtseg.o):\
  $(d0root)/cdc_util/source/crtseg.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/crtseg.f ;\
	mv crtseg.o $(SCRATCH)/cdc_util/a4/crtseg.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(darearect368.o):\
  $(d0root)/cdc_util/source/darea_correct.f\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/denvadj.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/darea_correct.f ;\
	mv darea_correct.o $(SCRATCH)/cdc_util/a4/darearect368.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dbkchd.o):\
  $(d0root)/cdc_util/source/dbkchd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dbkchd.f ;\
	mv dbkchd.o $(SCRATCH)/cdc_util/a4/dbkchd.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dbkchi.o):\
  $(d0root)/cdc_util/source/dbkchi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dbkchi.f ;\
	mv dbkchi.o $(SCRATCH)/cdc_util/a4/dbkchi.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dbkchz.o):\
  $(d0root)/cdc_util/source/dbkchz.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dbkchz.f ;\
	mv dbkchz.o $(SCRATCH)/cdc_util/a4/dbkchz.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dbkdex.o):\
  $(d0root)/cdc_util/source/dbkdex.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dbkdex.f ;\
	mv dbkdex.o $(SCRATCH)/cdc_util/a4/dbkdex.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dbkdlr.o):\
  $(d0root)/cdc_util/source/dbkdlr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dbkdlr.f ;\
	mv dbkdlr.o $(SCRATCH)/cdc_util/a4/dbkdlr.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dbkdsc.o):\
  $(d0root)/cdc_util/source/dbkdsc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dbkdsc.f ;\
	mv dbkdsc.o $(SCRATCH)/cdc_util/a4/dbkdsc.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dbkeff.o):\
  $(d0root)/cdc_util/source/dbkeff.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dbkeff.f ;\
	mv dbkeff.o $(SCRATCH)/cdc_util/a4/dbkeff.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dbkped.o):\
  $(d0root)/cdc_util/source/dbkped.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dbkped.f ;\
	mv dbkped.o $(SCRATCH)/cdc_util/a4/dbkped.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dbkpul.o):\
  $(d0root)/cdc_util/source/dbkpul.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dbkpul.f ;\
	mv dbkpul.o $(SCRATCH)/cdc_util/a4/dbkpul.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dbkswr.o):\
  $(d0root)/cdc_util/source/dbkswr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dbkswr.f ;\
	mv dbkswr.o $(SCRATCH)/cdc_util/a4/dbkswr.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dbnchk.o):\
  $(d0root)/cdc_util/source/dbnchk.f\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/ddebug.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dbnchk.f ;\
	mv dbnchk.o $(SCRATCH)/cdc_util/a4/dbnchk.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dc_envadj.o):\
  $(d0root)/cdc_util/source/dc_envadj.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/dbmukey.inc\
  $(d0library)/inc/denvadj.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dc_envadj.f ;\
	mv dc_envadj.o $(SCRATCH)/cdc_util/a4/dc_envadj.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dcdedx.o):\
  $(d0root)/cdc_util/source/dcdedx.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdltrk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dcdedx.f ;\
	mv dcdedx.o $(SCRATCH)/cdc_util/a4/dcdedx.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dchkt_edge.o):\
  $(d0root)/cdc_util/source/dchkt_edge.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dchkt_edge.f ;\
	mv dchkt_edge.o $(SCRATCH)/cdc_util/a4/dchkt_edge.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dchkz_edge.o):\
  $(d0root)/cdc_util/source/dchkz_edge.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dchkz_edge.f ;\
	mv dchkz_edge.o $(SCRATCH)/cdc_util/a4/dchkz_edge.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dclimb.o):\
  $(d0root)/cdc_util/source/dclimb.f\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/dltpar.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dclimb.f ;\
	mv dclimb.o $(SCRATCH)/cdc_util/a5/dclimb.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dcoder.o):\
  $(d0root)/cdc_util/source/dcoder.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dcoder.f ;\
	mv dcoder.o $(SCRATCH)/cdc_util/a5/dcoder.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dcrtmp.o):\
  $(d0root)/cdc_util/source/dcrtmp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dcrtmp.f ;\
	mv dcrtmp.o $(SCRATCH)/cdc_util/a5/dcrtmp.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(ddbini.o):\
  $(d0root)/cdc_util/source/ddbini.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/ddbini.f ;\
	mv ddbini.o $(SCRATCH)/cdc_util/a5/ddbini.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(ddelet.o):\
  $(d0root)/cdc_util/source/ddelet.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/links/izuser.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/ddelet.f ;\
	mv ddelet.o $(SCRATCH)/cdc_util/a5/ddelet.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dfitch.o):\
  $(d0root)/cdc_util/source/dfitch.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdcpar.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izuser.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dfitch.f ;\
	mv dfitch.o $(SCRATCH)/cdc_util/a5/dfitch.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dflhst.o):\
  $(d0root)/cdc_util/source/dflhst.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dflhst.f ;\
	mv dflhst.o $(SCRATCH)/cdc_util/a5/dflhst.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dflped.o):\
  $(d0root)/cdc_util/source/dflped.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdresf.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dflped.f ;\
	mv dflped.o $(SCRATCH)/cdc_util/a5/dflped.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dflpul.o):\
  $(d0root)/cdc_util/source/dflpul.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dflpul.f ;\
	mv dflpul.o $(SCRATCH)/cdc_util/a5/dflpul.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dflseg.o):\
  $(d0root)/cdc_util/source/dflseg.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dflseg.f ;\
	mv dflseg.o $(SCRATCH)/cdc_util/a5/dflseg.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dfltrk.o):\
  $(d0root)/cdc_util/source/dfltrk.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dfltrk.f ;\
	mv dfltrk.o $(SCRATCH)/cdc_util/a5/dfltrk.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dftlin.o):\
  $(d0root)/cdc_util/source/dftlin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dftlin.f ;\
	mv dftlin.o $(SCRATCH)/cdc_util/a5/dftlin.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dgnini.o):\
  $(d0root)/cdc_util/source/dgnini.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/links/izdgch.link\
  $(d0library)/links/izcrcp.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dgnini.f ;\
	mv dgnini.o $(SCRATCH)/cdc_util/a5/dgnini.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dgtgns.o):\
  $(d0root)/cdc_util/source/dgtgns.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/links/izdgns.link\
  $(d0library)/links/izdgch.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dgtgns.f ;\
	mv dgtgns.o $(SCRATCH)/cdc_util/a5/dgtgns.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dgtpds.o):\
  $(d0root)/cdc_util/source/dgtpds.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdped.link\
  $(d0library)/links/izdpch.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dgtpds.f ;\
	mv dgtpds.o $(SCRATCH)/cdc_util/a5/dgtpds.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dgttms.o):\
  $(d0root)/cdc_util/source/dgttms.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/links/izdtms.link\
  $(d0library)/links/izdtch.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dgttms.f ;\
	mv dgttms.o $(SCRATCH)/cdc_util/a5/dgttms.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dhit_data565.o):\
  $(d0root)/cdc_util/source/dhit_mc_to_data.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/params/byte_order.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dhit_mc_to_data.f ;\
	mv dhit_mc_to_data.o $(SCRATCH)/cdc_util/a5/dhit_data565.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dhit_unpack.o):\
  $(d0root)/cdc_util/source/dhit_unpack.f\
  $(d0library)/params/byte_order.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dhit_unpack.f ;\
	mv dhit_unpack.o $(SCRATCH)/cdc_util/a5/dhit_unpack.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dhitfl.o):\
  $(d0root)/cdc_util/source/dhitfl.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/params/byte_order.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dhitfl.f ;\
	mv dhitfl.o $(SCRATCH)/cdc_util/a5/dhitfl.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dhitst.o):\
  $(d0root)/cdc_util/source/dhitst.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dhitst.f ;\
	mv dhitst.o $(SCRATCH)/cdc_util/a5/dhitst.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dhstbk.o):\
  $(d0root)/cdc_util/source/dhstbk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dhstbk.f ;\
	mv dhstbk.o $(SCRATCH)/cdc_util/a6/dhstbk.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dhstfl.o):\
  $(d0root)/cdc_util/source/dhstfl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dhstfl.f ;\
	mv dhstfl.o $(SCRATCH)/cdc_util/a6/dhstfl.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dhstou.o):\
  $(d0root)/cdc_util/source/dhstou.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dhstou.f ;\
	mv dhstou.o $(SCRATCH)/cdc_util/a6/dhstou.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dhtchk.o):\
  $(d0root)/cdc_util/source/dhtchk.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dhtchk.f ;\
	mv dhtchk.o $(SCRATCH)/cdc_util/a6/dhtchk.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(disa_mark.o):\
  $(d0root)/cdc_util/source/disa_mark.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/disa_mark.f ;\
	mv disa_mark.o $(SCRATCH)/cdc_util/a6/disa_mark.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dldtsg.o):\
  $(d0root)/cdc_util/source/dldtsg.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdcpar.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dldtsg.f ;\
	mv dldtsg.o $(SCRATCH)/cdc_util/a6/dldtsg.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dlinfo.o):\
  $(d0root)/cdc_util/source/dlinfo.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dlinfo.f ;\
	mv dlinfo.o $(SCRATCH)/cdc_util/a6/dlinfo.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dmdstg.o):\
  $(d0root)/cdc_util/source/dmdstg.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dmdstg.f ;\
	mv dmdstg.o $(SCRATCH)/cdc_util/a6/dmdstg.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dmonpl.o):\
  $(d0root)/cdc_util/source/dmonpl.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/inc/cdpara.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dmonpl.f ;\
	mv dmonpl.o $(SCRATCH)/cdc_util/a6/dmonpl.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dnlcor.o):\
  $(d0root)/cdc_util/source/dnlcor.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dnlcor.f ;\
	mv dnlcor.o $(SCRATCH)/cdc_util/a6/dnlcor.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dpdini.o):\
  $(d0root)/cdc_util/source/dpdini.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/links/izdpch.link\
  $(d0library)/links/izcrcp.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dpdini.f ;\
	mv dpdini.o $(SCRATCH)/cdc_util/a6/dpdini.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(drddbg.o):\
  $(d0root)/cdc_util/source/drddbg.f\
  $(d0library)/inc/ddebug.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/drddbg.f ;\
	mv drddbg.o $(SCRATCH)/cdc_util/a6/drddbg.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dsec_dhit476.o):\
  $(d0root)/cdc_util/source/dsec_from_dhit.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dsec_from_dhit.f ;\
	mv dsec_from_dhit.o $(SCRATCH)/cdc_util/a6/dsec_dhit476.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dsecht.o):\
  $(d0root)/cdc_util/source/dsecht.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/cdccos.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dsecht.f ;\
	mv dsecht.o $(SCRATCH)/cdc_util/a6/dsecht.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dseglt.o):\
  $(d0root)/cdc_util/source/dseglt.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdcpar.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/dltpar.inc\
  $(d0library)/links/izuser.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dseglt.f ;\
	mv dseglt.o $(SCRATCH)/cdc_util/a6/dseglt.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dsegme.o):\
  $(d0root)/cdc_util/source/dsegme.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izuser.link\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/inc/cdcpar.inc\
  $(d0library)/inc/cdloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dsegme.f ;\
	mv dsegme.o $(SCRATCH)/cdc_util/a6/dsegme.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dsgsec.o):\
  $(d0root)/cdc_util/source/dsgsec.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dsgsec.f ;\
	mv dsgsec.o $(SCRATCH)/cdc_util/a6/dsgsec.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dstp_fetch.o):\
  $(d0root)/cdc_util/source/dstp_fetch.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdpdh.link\
  $(d0library)/links/izdgnh.link\
  $(d0library)/links/izdtmh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dstp_fetch.f ;\
	mv dstp_fetch.o $(SCRATCH)/cdc_util/a6/dstp_fetch.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dsvchn.o):\
  $(d0root)/cdc_util/source/dsvchn.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdcpar.inc\
  $(d0library)/inc/dltpar.inc\
  $(d0library)/links/izuser.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dsvchn.f ;\
	mv dsvchn.o $(SCRATCH)/cdc_util/a6/dsvchn.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtdefd.o):\
  $(d0root)/cdc_util/source/dtdefd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtdefd.f ;\
	mv dtdefd.o $(SCRATCH)/cdc_util/a6/dtdefd.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtlink.o):\
  $(d0root)/cdc_util/source/dtlink.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdcpar.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/dltpar.inc\
  $(d0library)/links/izuser.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtlink.f ;\
	mv dtlink.o $(SCRATCH)/cdc_util/a7/dtlink.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtmini.o):\
  $(d0root)/cdc_util/source/dtmini.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/links/izdtch.link\
  $(d0library)/links/izcrcp.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtmini.f ;\
	mv dtmini.o $(SCRATCH)/cdc_util/a7/dtmini.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtrdia.o):\
  $(d0root)/cdc_util/source/dtrdia.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtrdia.f ;\
	mv dtrdia.o $(SCRATCH)/cdc_util/a7/dtrdia.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtrdmp.o):\
  $(d0root)/cdc_util/source/dtrdmp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtrdmp.f ;\
	mv dtrdmp.o $(SCRATCH)/cdc_util/a7/dtrdmp.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtrdrp.o):\
  $(d0root)/cdc_util/source/dtrdrp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtrdrp.f ;\
	mv dtrdrp.o $(SCRATCH)/cdc_util/a7/dtrdrp.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtrevt.o):\
  $(d0root)/cdc_util/source/dtrevt.f\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/links/izisae.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtrevt.f ;\
	mv dtrevt.o $(SCRATCH)/cdc_util/a7/dtrevt.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtrhbk.o):\
  $(d0root)/cdc_util/source/dtrhbk.f\
  $(d0library)/inc/cdpara.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtrhbk.f ;\
	mv dtrhbk.o $(SCRATCH)/cdc_util/a7/dtrhbk.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtrini.o):\
  $(d0root)/cdc_util/source/dtrini.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtrini.f ;\
	mv dtrini.o $(SCRATCH)/cdc_util/a7/dtrini.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtrk_edge.o):\
  $(d0root)/cdc_util/source/dtrk_edge.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdresf.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtrk_edge.f ;\
	mv dtrk_edge.o $(SCRATCH)/cdc_util/a7/dtrk_edge.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtrk_fix.o):\
  $(d0root)/cdc_util/source/dtrk_fix.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtrk_fix.f ;\
	mv dtrk_fix.o $(SCRATCH)/cdc_util/a7/dtrk_fix.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtrk_dedx385.o):\
  $(d0root)/cdc_util/source/dtrk_fix_dedx.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtrk_fix_dedx.f ;\
	mv dtrk_fix_dedx.o $(SCRATCH)/cdc_util/a7/dtrk_dedx385.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtrk_fixz.o):\
  $(d0root)/cdc_util/source/dtrk_fixz.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtrk_fixz.f ;\
	mv dtrk_fixz.o $(SCRATCH)/cdc_util/a7/dtrk_fixz.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtrkhits.o):\
  $(d0root)/cdc_util/source/dtrkhits.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtrkhits.f ;\
	mv dtrkhits.o $(SCRATCH)/cdc_util/a7/dtrkhits.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtrkht.o):\
  $(d0root)/cdc_util/source/dtrkht.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtrkht.f ;\
	mv dtrkht.o $(SCRATCH)/cdc_util/a7/dtrkht.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtrksg.o):\
  $(d0root)/cdc_util/source/dtrksg.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtrksg.f ;\
	mv dtrksg.o $(SCRATCH)/cdc_util/a7/dtrksg.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtrpar.o):\
  $(d0root)/cdc_util/source/dtrpar.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtrpar.f ;\
	mv dtrpar.o $(SCRATCH)/cdc_util/a7/dtrpar.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtrusm.o):\
  $(d0root)/cdc_util/source/dtrusm.f\
  $(d0library)/inc/cdpara.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtrusm.f ;\
	mv dtrusm.o $(SCRATCH)/cdc_util/a7/dtrusm.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dtsg_z.o):\
  $(d0root)/cdc_util/source/dtsg_z.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dtsg_z.f ;\
	mv dtsg_z.o $(SCRATCH)/cdc_util/a7/dtsg_z.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(dxyerr.o):\
  $(d0root)/cdc_util/source/dxyerr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/dxyerr.f ;\
	mv dxyerr.o $(SCRATCH)/cdc_util/a7/dxyerr.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(fill_list848.o):\
  $(d0root)/cdc_util/source/fill_cdc_l2hitlist.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/fill_cdc_l2hitlist.f ;\
	mv fill_cdc_l2hitlist.o $(SCRATCH)/cdc_util/a7/fill_list848.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(fill_list397.o):\
  $(d0root)/cdc_util/source/fill_dhitlist.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/fill_dhitlist.f ;\
	mv fill_dhitlist.o $(SCRATCH)/cdc_util/a8/fill_list397.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(fldsec.o):\
  $(d0root)/cdc_util/source/fldsec.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/fldsec.f ;\
	mv fldsec.o $(SCRATCH)/cdc_util/a8/fldsec.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gtdhit.o):\
  $(d0root)/cdc_util/source/gtdhit.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gtdhit.f ;\
	mv gtdhit.o $(SCRATCH)/cdc_util/a8/gtdhit.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gtditr.o):\
  $(d0root)/cdc_util/source/gtditr.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gtditr.f ;\
	mv gtditr.o $(SCRATCH)/cdc_util/a8/gtditr.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gtdtrh.o):\
  $(d0root)/cdc_util/source/gtdtrh.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gtdtrh.f ;\
	mv gtdtrh.o $(SCRATCH)/cdc_util/a8/gtdtrh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gtdtrk.o):\
  $(d0root)/cdc_util/source/gtdtrk.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gtdtrk.f ;\
	mv gtdtrk.o $(SCRATCH)/cdc_util/a8/gtdtrk.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gtdtrk_link.o):\
  $(d0root)/cdc_util/source/gtdtrk_link.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gtdtrk_link.f ;\
	mv gtdtrk_link.o $(SCRATCH)/cdc_util/a8/gtdtrk_link.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzcdch.o):\
  $(d0root)/cdc_util/source/gzcdch.f\
  $(d0library)/links/izcdch.link\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzcdch.f ;\
	mv gzcdch.o $(SCRATCH)/cdc_util/a8/gzcdch.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzcdd2.o):\
  $(d0root)/cdc_util/source/gzcdd2.f\
  $(d0library)/links/izcdd2.link\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzcdd2.f ;\
	mv gzcdd2.o $(SCRATCH)/cdc_util/a8/gzcdd2.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzcdh2.o):\
  $(d0root)/cdc_util/source/gzcdh2.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcdh2.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzcdh2.f ;\
	mv gzcdh2.o $(SCRATCH)/cdc_util/a8/gzcdh2.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdalh.o):\
  $(d0root)/cdc_util/source/gzdalh.f\
  $(d0library)/links/izdalh.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdalh.f ;\
	mv gzdalh.o $(SCRATCH)/cdc_util/a8/gzdalh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdall.o):\
  $(d0root)/cdc_util/source/gzdall.f\
  $(d0library)/links/izdall.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdall.f ;\
	mv gzdall.o $(SCRATCH)/cdc_util/a8/gzdall.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdals.o):\
  $(d0root)/cdc_util/source/gzdals.f\
  $(d0library)/links/izdals.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdals.f ;\
	mv gzdals.o $(SCRATCH)/cdc_util/a8/gzdals.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdcbd.o):\
  $(d0root)/cdc_util/source/gzdcbd.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdcbd.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdcbd.f ;\
	mv gzdcbd.o $(SCRATCH)/cdc_util/a8/gzdcbd.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdcda.o):\
  $(d0root)/cdc_util/source/gzdcda.f\
  $(d0library)/links/izdcda.link\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdcda.f ;\
	mv gzdcda.o $(SCRATCH)/cdc_util/a8/gzdcda.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzddlp.o):\
  $(d0root)/cdc_util/source/gzddlp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izddlp.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzddlp.f ;\
	mv gzddlp.o $(SCRATCH)/cdc_util/a8/gzddlp.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdgeh.o):\
  $(d0root)/cdc_util/source/gzdgeh.f\
  $(d0library)/links/izdgeh.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdgeh.f ;\
	mv gzdgeh.o $(SCRATCH)/cdc_util/a8/gzdgeh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdgnh.o):\
  $(d0root)/cdc_util/source/gzdgnh.f\
  $(d0library)/links/izdgnh.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdgnh.f ;\
	mv gzdgnh.o $(SCRATCH)/cdc_util/a8/gzdgnh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdgnl.o):\
  $(d0root)/cdc_util/source/gzdgnl.f\
  $(d0library)/links/izdgnl.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdgnl.f ;\
	mv gzdgnl.o $(SCRATCH)/cdc_util/a8/gzdgnl.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdhit.o):\
  $(d0root)/cdc_util/source/gzdhit.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izdhit.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdhit.f ;\
	mv gzdhit.o $(SCRATCH)/cdc_util/a8/gzdhit.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzditr.o):\
  $(d0root)/cdc_util/source/gzditr.f\
  $(d0library)/links/izditr.link\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzditr.f ;\
	mv gzditr.o $(SCRATCH)/cdc_util/a9/gzditr.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdlyr.o):\
  $(d0root)/cdc_util/source/gzdlyr.f\
  $(d0library)/links/izdlyr.link\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdlyr.f ;\
	mv gzdlyr.o $(SCRATCH)/cdc_util/a9/gzdlyr.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdmat.o):\
  $(d0root)/cdc_util/source/gzdmat.f\
  $(d0library)/links/izdmat.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdmat.f ;\
	mv gzdmat.o $(SCRATCH)/cdc_util/a9/gzdmat.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdnli.o):\
  $(d0root)/cdc_util/source/gzdnli.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdnli.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdnli.f ;\
	mv gzdnli.o $(SCRATCH)/cdc_util/a9/gzdnli.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdnlo.o):\
  $(d0root)/cdc_util/source/gzdnlo.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdnlo.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdnlo.f ;\
	mv gzdnlo.o $(SCRATCH)/cdc_util/a9/gzdnlo.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdpdh.o):\
  $(d0root)/cdc_util/source/gzdpdh.f\
  $(d0library)/links/izdpdh.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdpdh.f ;\
	mv gzdpdh.o $(SCRATCH)/cdc_util/a9/gzdpdh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdpdl.o):\
  $(d0root)/cdc_util/source/gzdpdl.f\
  $(d0library)/links/izdpdl.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdpdl.f ;\
	mv gzdpdl.o $(SCRATCH)/cdc_util/a9/gzdpdl.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdrft.o):\
  $(d0root)/cdc_util/source/gzdrft.f\
  $(d0library)/links/izdrft.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdrft.f ;\
	mv gzdrft.o $(SCRATCH)/cdc_util/a9/gzdrft.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdsec.o):\
  $(d0root)/cdc_util/source/gzdsec.f\
  $(d0library)/links/izdsec.link\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdsec.f ;\
	mv gzdsec.o $(SCRATCH)/cdc_util/a9/gzdsec.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdswp.o):\
  $(d0root)/cdc_util/source/gzdswp.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdswp.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdswp.f ;\
	mv gzdswp.o $(SCRATCH)/cdc_util/a9/gzdswp.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdtmd.o):\
  $(d0root)/cdc_util/source/gzdtmd.f\
  $(d0library)/links/izdtmd.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdtmd.f ;\
	mv gzdtmd.o $(SCRATCH)/cdc_util/a9/gzdtmd.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdtmh.o):\
  $(d0root)/cdc_util/source/gzdtmh.f\
  $(d0library)/links/izdtmh.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdtmh.f ;\
	mv gzdtmh.o $(SCRATCH)/cdc_util/a9/gzdtmh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdtmw.o):\
  $(d0root)/cdc_util/source/gzdtmw.f\
  $(d0library)/links/izdtmw.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdtmw.f ;\
	mv gzdtmw.o $(SCRATCH)/cdc_util/a9/gzdtmw.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdtrh.o):\
  $(d0root)/cdc_util/source/gzdtrh.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izdtrh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdtrh.f ;\
	mv gzdtrh.o $(SCRATCH)/cdc_util/a9/gzdtrh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdtrk.o):\
  $(d0root)/cdc_util/source/gzdtrk.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izdtrh.link\
  $(d0library)/links/izdtrk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdtrk.f ;\
	mv gzdtrk.o $(SCRATCH)/cdc_util/a9/gzdtrk.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdtsg.o):\
  $(d0root)/cdc_util/source/gzdtsg.f\
  $(d0library)/links/izdtsg.link\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdtsg.f ;\
	mv gzdtsg.o $(SCRATCH)/cdc_util/a9/gzdtsg.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdtva.o):\
  $(d0root)/cdc_util/source/gzdtva.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdtva.f ;\
	mv gzdtva.o $(SCRATCH)/cdc_util/a9/gzdtva.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdtvh.o):\
  $(d0root)/cdc_util/source/gzdtvh.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdtvh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdtvh.f ;\
	mv gzdtvh.o $(SCRATCH)/cdc_util/a9/gzdtvh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzdwal.o):\
  $(d0root)/cdc_util/source/gzdwal.f\
  $(d0library)/links/izdwal.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzdwal.f ;\
	mv gzdwal.o $(SCRATCH)/cdc_util/a9/gzdwal.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzl2ch.o):\
  $(d0root)/cdc_util/source/gzl2ch.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izl2ch.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzl2ch.f ;\
	mv gzl2ch.o $(SCRATCH)/cdc_util/a9/gzl2ch.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzpsht.o):\
  $(d0root)/cdc_util/source/gzpsht.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpsht.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzpsht.f ;\
	mv gzpsht.o $(SCRATCH)/cdc_util/a10/gzpsht.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzpsrw.o):\
  $(d0root)/cdc_util/source/gzpsrw.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpsrw.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzpsrw.f ;\
	mv gzpsrw.o $(SCRATCH)/cdc_util/a10/gzpsrw.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzrtst.o):\
  $(d0root)/cdc_util/source/gzrtst.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izrtst.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzrtst.f ;\
	mv gzrtst.o $(SCRATCH)/cdc_util/a10/gzrtst.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzscdc.o):\
  $(d0root)/cdc_util/source/gzscdc.f\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izscdc.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzscdc.f ;\
	mv gzscdc.o $(SCRATCH)/cdc_util/a10/gzscdc.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzt0dh.o):\
  $(d0root)/cdc_util/source/gzt0dh.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0dh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzt0dh.f ;\
	mv gzt0dh.o $(SCRATCH)/cdc_util/a10/gzt0dh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzt0fh.o):\
  $(d0root)/cdc_util/source/gzt0fh.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0fh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzt0fh.f ;\
	mv gzt0fh.o $(SCRATCH)/cdc_util/a10/gzt0fh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzt0ht.o):\
  $(d0root)/cdc_util/source/gzt0ht.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0ht.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzt0ht.f ;\
	mv gzt0ht.o $(SCRATCH)/cdc_util/a10/gzt0ht.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzt0rw.o):\
  $(d0root)/cdc_util/source/gzt0rw.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0rw.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzt0rw.f ;\
	mv gzt0rw.o $(SCRATCH)/cdc_util/a10/gzt0rw.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzt0td.o):\
  $(d0root)/cdc_util/source/gzt0td.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0td.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzt0td.f ;\
	mv gzt0td.o $(SCRATCH)/cdc_util/a10/gzt0td.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzt0th.o):\
  $(d0root)/cdc_util/source/gzt0th.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0th.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzt0th.f ;\
	mv gzt0th.o $(SCRATCH)/cdc_util/a10/gzt0th.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzt0tr.o):\
  $(d0root)/cdc_util/source/gzt0tr.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0tr.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzt0tr.f ;\
	mv gzt0tr.o $(SCRATCH)/cdc_util/a10/gzt0tr.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzt0tz.o):\
  $(d0root)/cdc_util/source/gzt0tz.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0tz.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzt0tz.f ;\
	mv gzt0tz.o $(SCRATCH)/cdc_util/a10/gzt0tz.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzupgd.o):\
  $(d0root)/cdc_util/source/gzupgd.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izupgd.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzupgd.f ;\
	mv gzupgd.o $(SCRATCH)/cdc_util/a10/gzupgd.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzzdht.o):\
  $(d0root)/cdc_util/source/gzzdht.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izzdht.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzzdht.f ;\
	mv gzzdht.o $(SCRATCH)/cdc_util/a10/gzzdht.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(gzzdrw.o):\
  $(d0root)/cdc_util/source/gzzdrw.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izzdrw.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/gzzdrw.f ;\
	mv gzzdrw.o $(SCRATCH)/cdc_util/a10/gzzdrw.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(hbaver.o):\
  $(d0root)/cdc_util/source/hbaver.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/hbaver.f ;\
	mv hbaver.o $(SCRATCH)/cdc_util/a10/hbaver.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(icell.o):\
  $(d0root)/cdc_util/source/icell.f\
  $(d0library)/inc/pi.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/icell.f ;\
	mv icell.o $(SCRATCH)/cdc_util/a10/icell.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(lcheck.o):\
  $(d0root)/cdc_util/source/lcheck.f\
  $(d0library)/inc/pi.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/lcheck.f ;\
	mv lcheck.o $(SCRATCH)/cdc_util/a10/lcheck.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(nwires_hit.o):\
  $(d0root)/cdc_util/source/nwires_hit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/nwires_hit.f ;\
	mv nwires_hit.o $(SCRATCH)/cdc_util/a10/nwires_hit.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(prcdch.o):\
  $(d0root)/cdc_util/source/prcdch.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/prcdch.f ;\
	mv prcdch.o $(SCRATCH)/cdc_util/a10/prcdch.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(prcdd2.o):\
  $(d0root)/cdc_util/source/prcdd2.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcdd2.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/prcdd2.f ;\
	mv prcdd2.o $(SCRATCH)/cdc_util/a11/prcdd2.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(prdcda.o):\
  $(d0root)/cdc_util/source/prdcda.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/prdcda.f ;\
	mv prdcda.o $(SCRATCH)/cdc_util/a11/prdcda.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(prditr.o):\
  $(d0root)/cdc_util/source/prditr.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdltrk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/prditr.f ;\
	mv prditr.o $(SCRATCH)/cdc_util/a11/prditr.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(prdsec.o):\
  $(d0root)/cdc_util/source/prdsec.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/prdsec.f ;\
	mv prdsec.o $(SCRATCH)/cdc_util/a11/prdsec.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(prdtrh.o):\
  $(d0root)/cdc_util/source/prdtrh.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/prdtrh.f ;\
	mv prdtrh.o $(SCRATCH)/cdc_util/a11/prdtrh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(prdtrk.o):\
  $(d0root)/cdc_util/source/prdtrk.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdltrk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/prdtrk.f ;\
	mv prdtrk.o $(SCRATCH)/cdc_util/a11/prdtrk.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(prdtsg.o):\
  $(d0root)/cdc_util/source/prdtsg.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdltrk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/prdtsg.f ;\
	mv prdtsg.o $(SCRATCH)/cdc_util/a11/prdtsg.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(prrtst.o):\
  $(d0root)/cdc_util/source/prrtst.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izrtst.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/prrtst.f ;\
	mv prrtst.o $(SCRATCH)/cdc_util/a11/prrtst.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(prt0dh.o):\
  $(d0root)/cdc_util/source/prt0dh.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/prt0dh.f ;\
	mv prt0dh.o $(SCRATCH)/cdc_util/a11/prt0dh.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(prt0ht.o):\
  $(d0root)/cdc_util/source/prt0ht.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/prt0ht.f ;\
	mv prt0ht.o $(SCRATCH)/cdc_util/a11/prt0ht.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(prt0rw.o):\
  $(d0root)/cdc_util/source/prt0rw.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/prt0rw.f ;\
	mv prt0rw.o $(SCRATCH)/cdc_util/a11/prt0rw.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(prt0td.o):\
  $(d0root)/cdc_util/source/prt0td.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/prt0td.f ;\
	mv prt0td.o $(SCRATCH)/cdc_util/a11/prt0td.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(prt0th.o):\
  $(d0root)/cdc_util/source/prt0th.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/prt0th.f ;\
	mv prt0th.o $(SCRATCH)/cdc_util/a11/prt0th.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(prt0tz.o):\
  $(d0root)/cdc_util/source/prt0tz.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/prt0tz.f ;\
	mv prt0tz.o $(SCRATCH)/cdc_util/a11/prt0tz.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(psrwfl.o):\
  $(d0root)/cdc_util/source/psrwfl.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcdd1.link\
  $(d0library)/links/izpsht.link\
  $(d0library)/links/izpsrw.link\
  $(d0library)/params/preshower.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/psrwfl.f ;\
	mv psrwfl.o $(SCRATCH)/cdc_util/a11/psrwfl.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(pudcda.o):\
  $(d0root)/cdc_util/source/pudcda.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/pudcda.f ;\
	mv pudcda.o $(SCRATCH)/cdc_util/a11/pudcda.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(puditr.o):\
  $(d0root)/cdc_util/source/puditr.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/puditr.f ;\
	mv puditr.o $(SCRATCH)/cdc_util/a11/puditr.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(pudsec.o):\
  $(d0root)/cdc_util/source/pudsec.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/pudsec.f ;\
	mv pudsec.o $(SCRATCH)/cdc_util/a11/pudsec.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(recdd2.o):\
  $(d0root)/cdc_util/source/recdd2.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/links/izcdd2.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/recdd2.f ;\
	mv recdd2.o $(SCRATCH)/cdc_util/a11/recdd2.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(rtstfl.o):\
  $(d0root)/cdc_util/source/rtstfl.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcdd1.link\
  $(d0library)/links/izrtst.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/rtstfl.f ;\
	mv rtstfl.o $(SCRATCH)/cdc_util/a11/rtstfl.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(squeeze.o):\
  $(d0root)/cdc_util/source/squeeze.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/squeeze.f ;\
	mv squeeze.o $(SCRATCH)/cdc_util/a12/squeeze.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(t0devt.o):\
  $(d0root)/cdc_util/source/t0devt.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0drec.inc\
  $(d0library)/inc/t0dlnt1.inc\
  $(d0library)/inc/t0dlnt2.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/t0devt.f ;\
	mv t0devt.o $(SCRATCH)/cdc_util/a12/t0devt.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(t0dhfl.o):\
  $(d0root)/cdc_util/source/t0dhfl.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0dlnt2.inc\
  $(d0library)/links/izt0dh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/t0dhfl.f ;\
	mv t0dhfl.o $(SCRATCH)/cdc_util/a12/t0dhfl.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(t0dini.o):\
  $(d0root)/cdc_util/source/t0dini.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/flags.inc\
  $(d0library)/inc/t0dlnt1.inc\
  $(d0library)/inc/t0dlnt2.inc\
  $(d0library)/inc/t0dlnt3.inc\
  $(d0library)/inc/t0dpos.inc\
  $(d0library)/inc/t0drec.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/t0dini.f ;\
	mv t0dini.o $(SCRATCH)/cdc_util/a12/t0dini.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(t0dpar.o):\
  $(d0root)/cdc_util/source/t0dpar.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/t0dpar.f ;\
	mv t0dpar.o $(SCRATCH)/cdc_util/a12/t0dpar.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(t0dtrk.o):\
  $(d0root)/cdc_util/source/t0dtrk.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0drec.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/t0dtrk.f ;\
	mv t0dtrk.o $(SCRATCH)/cdc_util/a12/t0dtrk.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(t0hitc.o):\
  $(d0root)/cdc_util/source/t0hitc.f\
  $(d0library)/inc/t0dpos.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/t0hitc.f ;\
	mv t0hitc.o $(SCRATCH)/cdc_util/a12/t0hitc.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(t0hits.o):\
  $(d0root)/cdc_util/source/t0hits.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0drec.inc\
  $(d0library)/params/t0d.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/t0hits.f ;\
	mv t0hits.o $(SCRATCH)/cdc_util/a12/t0hits.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(t0rwfl.o):\
  $(d0root)/cdc_util/source/t0rwfl.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0dlnt2.inc\
  $(d0library)/links/izt0rw.link\
  $(d0library)/params/t0d.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/t0rwfl.f ;\
	mv t0rwfl.o $(SCRATCH)/cdc_util/a12/t0rwfl.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(t0tdfl.o):\
  $(d0root)/cdc_util/source/t0tdfl.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0dlnt2.inc\
  $(d0library)/links/izt0td.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/t0tdfl.f ;\
	mv t0tdfl.o $(SCRATCH)/cdc_util/a12/t0tdfl.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(t0trks.o):\
  $(d0root)/cdc_util/source/t0trks.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0drec.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/t0trks.f ;\
	mv t0trks.o $(SCRATCH)/cdc_util/a12/t0trks.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(t0tzfl.o):\
  $(d0root)/cdc_util/source/t0tzfl.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0drec.inc\
  $(d0library)/inc/t0dlnt2.inc\
  $(d0library)/inc/t0dlnt3.inc\
  $(d0library)/links/izt0tz.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/t0tzfl.f ;\
	mv t0tzfl.o $(SCRATCH)/cdc_util/a12/t0tzfl.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(zcdevt.o):\
  $(d0root)/cdc_util/source/zcdevt.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zcdrec.inc\
  $(d0library)/inc/zcdln1.inc\
  $(d0library)/inc/zcdln2.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/zcdevt.f ;\
	mv zcdevt.o $(SCRATCH)/cdc_util/a12/zcdevt.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(zcdhits.o):\
  $(d0root)/cdc_util/source/zcdhits.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zcdrec.inc\
  $(d0library)/params/zcd.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/zcdhits.f ;\
	mv zcdhits.o $(SCRATCH)/cdc_util/a12/zcdhits.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(zcdini.o):\
  $(d0root)/cdc_util/source/zcdini.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zcdln1.inc\
  $(d0library)/inc/zcdln2.inc\
  $(d0library)/inc/zcdln3.inc\
  $(d0library)/inc/zcdrec.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/zcdini.f ;\
	mv zcdini.o $(SCRATCH)/cdc_util/a12/zcdini.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(zdrwfl.o):\
  $(d0root)/cdc_util/source/zdrwfl.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zcdln2.inc\
  $(d0library)/links/izzdrw.link\
  $(d0library)/links/izcdd1.link\
  $(d0library)/params/zcd.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/zdrwfl.f ;\
	mv zdrwfl.o $(SCRATCH)/cdc_util/a12/zdrwfl.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(zfdcda.o):\
  $(d0root)/cdc_util/source/zfdcda.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/zfdcda.f ;\
	mv zfdcda.o $(SCRATCH)/cdc_util/a12/zfdcda.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(zfditr.o):\
  $(d0root)/cdc_util/source/zfditr.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/links/izditr.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/zfditr.f ;\
	mv zfditr.o $(SCRATCH)/cdc_util/a12/zfditr.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(zfdsec.o):\
  $(d0root)/cdc_util/source/zfdsec.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/zfdsec.f ;\
	mv zfdsec.o $(SCRATCH)/cdc_util/a12/zfdsec.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(zfdtrk.o):\
  $(d0root)/cdc_util/source/zfdtrk.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdltrk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/zfdtrk.f ;\
	mv zfdtrk.o $(SCRATCH)/cdc_util/a12/zfdtrk.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(zfdtsg.o):\
  $(d0root)/cdc_util/source/zfdtsg.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/inc/cdloca.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/zfdtsg.f ;\
	mv zfdtsg.o $(SCRATCH)/cdc_util/a13/zfdtsg.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(zgdcda.o):\
  $(d0root)/cdc_util/source/zgdcda.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/zgdcda.f ;\
	mv zgdcda.o $(SCRATCH)/cdc_util/a13/zgdcda.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(zgdsec.o):\
  $(d0root)/cdc_util/source/zgdsec.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/zgdsec.f ;\
	mv zgdsec.o $(SCRATCH)/cdc_util/a13/zgdsec.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(zgdtsg.o):\
  $(d0root)/cdc_util/source/zgdtsg.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdltrk.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/zgdtsg.f ;\
	mv zgdtsg.o $(SCRATCH)/cdc_util/a13/zgdtsg.o ;\
	)
$(d0root)/cdc_util/cdc_util.a(zhit.o):\
  $(d0root)/cdc_util/source/zhit.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) cdc_util/source/zhit.f ;\
	mv zhit.o $(SCRATCH)/cdc_util/a13/zhit.o ;\
	)
pre:\
  $(d0root)/cdc_util/source/bkcdch.f\
  $(d0root)/cdc_util/source/bkcdd2.f\
  $(d0root)/cdc_util/source/bkdcda.f\
  $(d0root)/cdc_util/source/bkdgbd.f\
  $(d0root)/cdc_util/source/bkdgch.f\
  $(d0root)/cdc_util/source/bkdgnh.f\
  $(d0root)/cdc_util/source/bkdgns.f\
  $(d0root)/cdc_util/source/bkdhit.f\
  $(d0root)/cdc_util/source/bkdlyr.f\
  $(d0root)/cdc_util/source/bkdpbd.f\
  $(d0root)/cdc_util/source/bkdpch.f\
  $(d0root)/cdc_util/source/bkdpdh.f\
  $(d0root)/cdc_util/source/bkdped.f\
  $(d0root)/cdc_util/source/bkdpzs.f\
  $(d0root)/cdc_util/source/bkdsec.f\
  $(d0root)/cdc_util/source/bkdtbd.f\
  $(d0root)/cdc_util/source/bkdtch.f\
  $(d0root)/cdc_util/source/bkdtmh.f\
  $(d0root)/cdc_util/source/bkdtms.f\
  $(d0root)/cdc_util/source/bkdtrh.f\
  $(d0root)/cdc_util/source/bkdtrk.f\
  $(d0root)/cdc_util/source/bkpsht.f\
  $(d0root)/cdc_util/source/bkpsrw.f\
  $(d0root)/cdc_util/source/bkrtst.f\
  $(d0root)/cdc_util/source/bkscdc.f\
  $(d0root)/cdc_util/source/bkt0ht.f\
  $(d0root)/cdc_util/source/bkt0th.f\
  $(d0root)/cdc_util/source/bkupgd.f\
  $(d0root)/cdc_util/source/bkzdht.f\
  $(d0root)/cdc_util/source/cdalgn.f\
  $(d0root)/cdc_util/source/cdc_exm_anal.f\
  $(d0root)/cdc_util/source/cdc_fix.f\
  $(d0root)/cdc_util/source/cdc_hst_dispatch.f\
  $(d0root)/cdc_util/source/cdc_hst_menu.f\
  $(d0root)/cdc_util/source/cdchit_cell.f\
  $(d0root)/cdc_util/source/cdchit_hits.f\
  $(d0root)/cdc_util/source/cdchit_info.f\
  $(d0root)/cdc_util/source/cdchit_segm.f\
  $(d0root)/cdc_util/source/cdchit_strip.f\
  $(d0root)/cdc_util/source/cdcisa.f\
  $(d0root)/cdc_util/source/cdclos.f\
  $(d0root)/cdc_util/source/cdcvel.f\
  $(d0root)/cdc_util/source/cdgetz.f\
  $(d0root)/cdc_util/source/cdhfil.f\
  $(d0root)/cdc_util/source/cdhitl.f\
  $(d0root)/cdc_util/source/cdhits.f\
  $(d0root)/cdc_util/source/cdhtrk.f\
  $(d0root)/cdc_util/source/cdinit.f\
  $(d0root)/cdc_util/source/cdistp.f\
  $(d0root)/cdc_util/source/cdistr.f\
  $(d0root)/cdc_util/source/cdnctr.f\
  $(d0root)/cdc_util/source/cdpuls.f\
  $(d0root)/cdc_util/source/cdroad.f\
  $(d0root)/cdc_util/source/cdsave.f\
  $(d0root)/cdc_util/source/cdsres.f\
  $(d0root)/cdc_util/source/cdtfit.f\
  $(d0root)/cdc_util/source/cdtrak.f\
  $(d0root)/cdc_util/source/cdtseg.f\
  $(d0root)/cdc_util/source/cdtsto.f\
  $(d0root)/cdc_util/source/cdunpk.f\
  $(d0root)/cdc_util/source/cdxyzl.f\
  $(d0root)/cdc_util/source/crhitl.f\
  $(d0root)/cdc_util/source/crhits.f\
  $(d0root)/cdc_util/source/crtseg.f\
  $(d0root)/cdc_util/source/darea_correct.f\
  $(d0root)/cdc_util/source/dbkchd.f\
  $(d0root)/cdc_util/source/dbkchi.f\
  $(d0root)/cdc_util/source/dbkchz.f\
  $(d0root)/cdc_util/source/dbkdex.f\
  $(d0root)/cdc_util/source/dbkdlr.f\
  $(d0root)/cdc_util/source/dbkdsc.f\
  $(d0root)/cdc_util/source/dbkeff.f\
  $(d0root)/cdc_util/source/dbkped.f\
  $(d0root)/cdc_util/source/dbkpul.f\
  $(d0root)/cdc_util/source/dbkswr.f\
  $(d0root)/cdc_util/source/dbnchk.f\
  $(d0root)/cdc_util/source/dc_envadj.f\
  $(d0root)/cdc_util/source/dcdedx.f\
  $(d0root)/cdc_util/source/dchkt_edge.f\
  $(d0root)/cdc_util/source/dchkz_edge.f\
  $(d0root)/cdc_util/source/dclimb.f\
  $(d0root)/cdc_util/source/dcoder.f\
  $(d0root)/cdc_util/source/dcrtmp.f\
  $(d0root)/cdc_util/source/ddbini.f\
  $(d0root)/cdc_util/source/ddelet.f\
  $(d0root)/cdc_util/source/dfitch.f\
  $(d0root)/cdc_util/source/dflhst.f\
  $(d0root)/cdc_util/source/dflped.f\
  $(d0root)/cdc_util/source/dflpul.f\
  $(d0root)/cdc_util/source/dflseg.f\
  $(d0root)/cdc_util/source/dfltrk.f\
  $(d0root)/cdc_util/source/dftlin.f\
  $(d0root)/cdc_util/source/dgnini.f\
  $(d0root)/cdc_util/source/dgtgns.f\
  $(d0root)/cdc_util/source/dgtpds.f\
  $(d0root)/cdc_util/source/dgttms.f\
  $(d0root)/cdc_util/source/dhit_mc_to_data.f\
  $(d0root)/cdc_util/source/dhit_unpack.f\
  $(d0root)/cdc_util/source/dhitfl.f\
  $(d0root)/cdc_util/source/dhitst.f\
  $(d0root)/cdc_util/source/dhstbk.f\
  $(d0root)/cdc_util/source/dhstfl.f\
  $(d0root)/cdc_util/source/dhstou.f\
  $(d0root)/cdc_util/source/dhtchk.f\
  $(d0root)/cdc_util/source/disa_mark.f\
  $(d0root)/cdc_util/source/dldtsg.f\
  $(d0root)/cdc_util/source/dlinfo.f\
  $(d0root)/cdc_util/source/dmdstg.f\
  $(d0root)/cdc_util/source/dmonpl.f\
  $(d0root)/cdc_util/source/dnlcor.f\
  $(d0root)/cdc_util/source/dpdini.f\
  $(d0root)/cdc_util/source/drddbg.f\
  $(d0root)/cdc_util/source/dsec_from_dhit.f\
  $(d0root)/cdc_util/source/dsecht.f\
  $(d0root)/cdc_util/source/dseglt.f\
  $(d0root)/cdc_util/source/dsegme.f\
  $(d0root)/cdc_util/source/dsgsec.f\
  $(d0root)/cdc_util/source/dstp_fetch.f\
  $(d0root)/cdc_util/source/dsvchn.f\
  $(d0root)/cdc_util/source/dtdefd.f\
  $(d0root)/cdc_util/source/dtlink.f\
  $(d0root)/cdc_util/source/dtmini.f\
  $(d0root)/cdc_util/source/dtrdia.f\
  $(d0root)/cdc_util/source/dtrdmp.f\
  $(d0root)/cdc_util/source/dtrdrp.f\
  $(d0root)/cdc_util/source/dtrevt.f\
  $(d0root)/cdc_util/source/dtrhbk.f\
  $(d0root)/cdc_util/source/dtrini.f\
  $(d0root)/cdc_util/source/dtrk_edge.f\
  $(d0root)/cdc_util/source/dtrk_fix.f\
  $(d0root)/cdc_util/source/dtrk_fix_dedx.f\
  $(d0root)/cdc_util/source/dtrk_fixz.f\
  $(d0root)/cdc_util/source/dtrkhits.f\
  $(d0root)/cdc_util/source/dtrkht.f\
  $(d0root)/cdc_util/source/dtrksg.f\
  $(d0root)/cdc_util/source/dtrpar.f\
  $(d0root)/cdc_util/source/dtrusm.f\
  $(d0root)/cdc_util/source/dtsg_z.f\
  $(d0root)/cdc_util/source/dxyerr.f\
  $(d0root)/cdc_util/source/fill_cdc_l2hitlist.f\
  $(d0root)/cdc_util/source/fill_dhitlist.f\
  $(d0root)/cdc_util/source/fldsec.f\
  $(d0root)/cdc_util/source/gtdhit.f\
  $(d0root)/cdc_util/source/gtditr.f\
  $(d0root)/cdc_util/source/gtdtrh.f\
  $(d0root)/cdc_util/source/gtdtrk.f\
  $(d0root)/cdc_util/source/gtdtrk_link.f\
  $(d0root)/cdc_util/source/gzcdch.f\
  $(d0root)/cdc_util/source/gzcdd2.f\
  $(d0root)/cdc_util/source/gzcdh2.f\
  $(d0root)/cdc_util/source/gzdalh.f\
  $(d0root)/cdc_util/source/gzdall.f\
  $(d0root)/cdc_util/source/gzdals.f\
  $(d0root)/cdc_util/source/gzdcbd.f\
  $(d0root)/cdc_util/source/gzdcda.f\
  $(d0root)/cdc_util/source/gzddlp.f\
  $(d0root)/cdc_util/source/gzdgeh.f\
  $(d0root)/cdc_util/source/gzdgnh.f\
  $(d0root)/cdc_util/source/gzdgnl.f\
  $(d0root)/cdc_util/source/gzdhit.f\
  $(d0root)/cdc_util/source/gzditr.f\
  $(d0root)/cdc_util/source/gzdlyr.f\
  $(d0root)/cdc_util/source/gzdmat.f\
  $(d0root)/cdc_util/source/gzdnli.f\
  $(d0root)/cdc_util/source/gzdnlo.f\
  $(d0root)/cdc_util/source/gzdpdh.f\
  $(d0root)/cdc_util/source/gzdpdl.f\
  $(d0root)/cdc_util/source/gzdrft.f\
  $(d0root)/cdc_util/source/gzdsec.f\
  $(d0root)/cdc_util/source/gzdswp.f\
  $(d0root)/cdc_util/source/gzdtmd.f\
  $(d0root)/cdc_util/source/gzdtmh.f\
  $(d0root)/cdc_util/source/gzdtmw.f\
  $(d0root)/cdc_util/source/gzdtrh.f\
  $(d0root)/cdc_util/source/gzdtrk.f\
  $(d0root)/cdc_util/source/gzdtsg.f\
  $(d0root)/cdc_util/source/gzdtva.f\
  $(d0root)/cdc_util/source/gzdtvh.f\
  $(d0root)/cdc_util/source/gzdwal.f\
  $(d0root)/cdc_util/source/gzl2ch.f\
  $(d0root)/cdc_util/source/gzpsht.f\
  $(d0root)/cdc_util/source/gzpsrw.f\
  $(d0root)/cdc_util/source/gzrtst.f\
  $(d0root)/cdc_util/source/gzscdc.f\
  $(d0root)/cdc_util/source/gzt0dh.f\
  $(d0root)/cdc_util/source/gzt0fh.f\
  $(d0root)/cdc_util/source/gzt0ht.f\
  $(d0root)/cdc_util/source/gzt0rw.f\
  $(d0root)/cdc_util/source/gzt0td.f\
  $(d0root)/cdc_util/source/gzt0th.f\
  $(d0root)/cdc_util/source/gzt0tr.f\
  $(d0root)/cdc_util/source/gzt0tz.f\
  $(d0root)/cdc_util/source/gzupgd.f\
  $(d0root)/cdc_util/source/gzzdht.f\
  $(d0root)/cdc_util/source/gzzdrw.f\
  $(d0root)/cdc_util/source/hbaver.f\
  $(d0root)/cdc_util/source/icell.f\
  $(d0root)/cdc_util/source/lcheck.f\
  $(d0root)/cdc_util/source/nwires_hit.f\
  $(d0root)/cdc_util/source/prcdch.f\
  $(d0root)/cdc_util/source/prcdd2.f\
  $(d0root)/cdc_util/source/prdcda.f\
  $(d0root)/cdc_util/source/prditr.f\
  $(d0root)/cdc_util/source/prdsec.f\
  $(d0root)/cdc_util/source/prdtrh.f\
  $(d0root)/cdc_util/source/prdtrk.f\
  $(d0root)/cdc_util/source/prdtsg.f\
  $(d0root)/cdc_util/source/prrtst.f\
  $(d0root)/cdc_util/source/prt0dh.f\
  $(d0root)/cdc_util/source/prt0ht.f\
  $(d0root)/cdc_util/source/prt0rw.f\
  $(d0root)/cdc_util/source/prt0td.f\
  $(d0root)/cdc_util/source/prt0th.f\
  $(d0root)/cdc_util/source/prt0tz.f\
  $(d0root)/cdc_util/source/psrwfl.f\
  $(d0root)/cdc_util/source/pudcda.f\
  $(d0root)/cdc_util/source/puditr.f\
  $(d0root)/cdc_util/source/pudsec.f\
  $(d0root)/cdc_util/source/recdd2.f\
  $(d0root)/cdc_util/source/rtstfl.f\
  $(d0root)/cdc_util/source/squeeze.f\
  $(d0root)/cdc_util/source/t0devt.f\
  $(d0root)/cdc_util/source/t0dhfl.f\
  $(d0root)/cdc_util/source/t0dini.f\
  $(d0root)/cdc_util/source/t0dpar.f\
  $(d0root)/cdc_util/source/t0dtrk.f\
  $(d0root)/cdc_util/source/t0hitc.f\
  $(d0root)/cdc_util/source/t0hits.f\
  $(d0root)/cdc_util/source/t0rwfl.f\
  $(d0root)/cdc_util/source/t0tdfl.f\
  $(d0root)/cdc_util/source/t0trks.f\
  $(d0root)/cdc_util/source/t0tzfl.f\
  $(d0root)/cdc_util/source/zcdevt.f\
  $(d0root)/cdc_util/source/zcdhits.f\
  $(d0root)/cdc_util/source/zcdini.f\
  $(d0root)/cdc_util/source/zdrwfl.f\
  $(d0root)/cdc_util/source/zfdcda.f\
  $(d0root)/cdc_util/source/zfditr.f\
  $(d0root)/cdc_util/source/zfdsec.f\
  $(d0root)/cdc_util/source/zfdtrk.f\
  $(d0root)/cdc_util/source/zfdtsg.f\
  $(d0root)/cdc_util/source/zgdcda.f\
  $(d0root)/cdc_util/source/zgdsec.f\
  $(d0root)/cdc_util/source/zgdtsg.f\
  $(d0root)/cdc_util/source/zhit.f
$(d0root)/cdc_util/source/bkcdch.f:\
  $(d0root)/cdc_util/source/bkcdch.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/links/izcdch.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkcdch.for | vmstounix > $(d0root)/cdc_util/source/bkcdch.f
$(d0root)/cdc_util/source/bkcdd2.f:\
  $(d0root)/cdc_util/source/bkcdd2.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcdd2.link\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkcdd2.for | vmstounix > $(d0root)/cdc_util/source/bkcdd2.f
$(d0root)/cdc_util/source/bkdcda.f:\
  $(d0root)/cdc_util/source/bkdcda.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdcda.for | vmstounix > $(d0root)/cdc_util/source/bkdcda.f
$(d0root)/cdc_util/source/bkdgbd.f:\
  $(d0root)/cdc_util/source/bkdgbd.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdgbd.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdgbd.for | vmstounix > $(d0root)/cdc_util/source/bkdgbd.f
$(d0root)/cdc_util/source/bkdgch.f:\
  $(d0root)/cdc_util/source/bkdgch.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdgnh.link\
  $(d0library)/links/izdgch.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdgch.for | vmstounix > $(d0root)/cdc_util/source/bkdgch.f
$(d0root)/cdc_util/source/bkdgnh.f:\
  $(d0root)/cdc_util/source/bkdgnh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdgnh.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdgnh.for | vmstounix > $(d0root)/cdc_util/source/bkdgnh.f
$(d0root)/cdc_util/source/bkdgns.f:\
  $(d0root)/cdc_util/source/bkdgns.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdgns.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdgns.for | vmstounix > $(d0root)/cdc_util/source/bkdgns.f
$(d0root)/cdc_util/source/bkdhit.f:\
  $(d0root)/cdc_util/source/bkdhit.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/links/izdhit.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdhit.for | vmstounix > $(d0root)/cdc_util/source/bkdhit.f
$(d0root)/cdc_util/source/bkdlyr.f:\
  $(d0root)/cdc_util/source/bkdlyr.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/links/izdlyr.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdlyr.for | vmstounix > $(d0root)/cdc_util/source/bkdlyr.f
$(d0root)/cdc_util/source/bkdpbd.f:\
  $(d0root)/cdc_util/source/bkdpbd.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdpbd.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdpbd.for | vmstounix > $(d0root)/cdc_util/source/bkdpbd.f
$(d0root)/cdc_util/source/bkdpch.f:\
  $(d0root)/cdc_util/source/bkdpch.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdpdh.link\
  $(d0library)/links/izdpch.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdpch.for | vmstounix > $(d0root)/cdc_util/source/bkdpch.f
$(d0root)/cdc_util/source/bkdpdh.f:\
  $(d0root)/cdc_util/source/bkdpdh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdpdh.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdpdh.for | vmstounix > $(d0root)/cdc_util/source/bkdpdh.f
$(d0root)/cdc_util/source/bkdped.f:\
  $(d0root)/cdc_util/source/bkdped.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdped.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdped.for | vmstounix > $(d0root)/cdc_util/source/bkdped.f
$(d0root)/cdc_util/source/bkdpzs.f:\
  $(d0root)/cdc_util/source/bkdpzs.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdpzs.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdpzs.for | vmstounix > $(d0root)/cdc_util/source/bkdpzs.f
$(d0root)/cdc_util/source/bkdsec.f:\
  $(d0root)/cdc_util/source/bkdsec.for\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izdsec.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdsec.for | vmstounix > $(d0root)/cdc_util/source/bkdsec.f
$(d0root)/cdc_util/source/bkdtbd.f:\
  $(d0root)/cdc_util/source/bkdtbd.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdtbd.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdtbd.for | vmstounix > $(d0root)/cdc_util/source/bkdtbd.f
$(d0root)/cdc_util/source/bkdtch.f:\
  $(d0root)/cdc_util/source/bkdtch.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdtmh.link\
  $(d0library)/links/izdtch.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdtch.for | vmstounix > $(d0root)/cdc_util/source/bkdtch.f
$(d0root)/cdc_util/source/bkdtmh.f:\
  $(d0root)/cdc_util/source/bkdtmh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdtmh.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdtmh.for | vmstounix > $(d0root)/cdc_util/source/bkdtmh.f
$(d0root)/cdc_util/source/bkdtms.f:\
  $(d0root)/cdc_util/source/bkdtms.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdtms.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdtms.for | vmstounix > $(d0root)/cdc_util/source/bkdtms.f
$(d0root)/cdc_util/source/bkdtrh.f:\
  $(d0root)/cdc_util/source/bkdtrh.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/links/izztrh.link\
  $(d0library)/links/izdtrh.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdtrh.for | vmstounix > $(d0root)/cdc_util/source/bkdtrh.f
$(d0root)/cdc_util/source/bkdtrk.f:\
  $(d0root)/cdc_util/source/bkdtrk.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izdtrk.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkdtrk.for | vmstounix > $(d0root)/cdc_util/source/bkdtrk.f
$(d0root)/cdc_util/source/bkpsht.f:\
  $(d0root)/cdc_util/source/bkpsht.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpsht.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkpsht.for | vmstounix > $(d0root)/cdc_util/source/bkpsht.f
$(d0root)/cdc_util/source/bkpsrw.f:\
  $(d0root)/cdc_util/source/bkpsrw.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpsrw.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkpsrw.for | vmstounix > $(d0root)/cdc_util/source/bkpsrw.f
$(d0root)/cdc_util/source/bkrtst.f:\
  $(d0root)/cdc_util/source/bkrtst.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izrtst.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkrtst.for | vmstounix > $(d0root)/cdc_util/source/bkrtst.f
$(d0root)/cdc_util/source/bkscdc.f:\
  $(d0root)/cdc_util/source/bkscdc.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izstpo.link\
  $(d0library)/links/izstpn.link\
  $(d0library)/links/izscdc.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkscdc.for | vmstounix > $(d0root)/cdc_util/source/bkscdc.f
$(d0root)/cdc_util/source/bkt0ht.f:\
  $(d0root)/cdc_util/source/bkt0ht.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0dlnt2.inc\
  $(d0library)/links/izt0ht.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkt0ht.for | vmstounix > $(d0root)/cdc_util/source/bkt0ht.f
$(d0root)/cdc_util/source/bkt0th.f:\
  $(d0root)/cdc_util/source/bkt0th.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0dlnt2.inc\
  $(d0library)/links/izt0th.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkt0th.for | vmstounix > $(d0root)/cdc_util/source/bkt0th.f
$(d0root)/cdc_util/source/bkupgd.f:\
  $(d0root)/cdc_util/source/bkupgd.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izupgd.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkupgd.for | vmstounix > $(d0root)/cdc_util/source/bkupgd.f
$(d0root)/cdc_util/source/bkzdht.f:\
  $(d0root)/cdc_util/source/bkzdht.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zcdln2.inc\
  $(d0library)/links/izzdht.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/bkzdht.for | vmstounix > $(d0root)/cdc_util/source/bkzdht.f
$(d0root)/cdc_util/source/cdalgn.f:\
  $(d0root)/cdc_util/source/cdalgn.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdresf.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdalgn.for | vmstounix > $(d0root)/cdc_util/source/cdalgn.f
$(d0root)/cdc_util/source/cdc_exm_anal.f:\
  $(d0root)/cdc_util/source/cdc_exm_anal.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdc_exm_anal.for | vmstounix > $(d0root)/cdc_util/source/cdc_exm_anal.f
$(d0root)/cdc_util/source/cdc_fix.f:\
  $(d0root)/cdc_util/source/cdc_fix.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdc_fix.for | vmstounix > $(d0root)/cdc_util/source/cdc_fix.f
$(d0root)/cdc_util/source/cdc_hst_dispatch.f:\
  $(d0root)/cdc_util/source/cdc_hst_dispatch.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdc_hst_dispatch.for | vmstounix > $(d0root)/cdc_util/source/cdc_hst_dispatch.f
$(d0root)/cdc_util/source/cdc_hst_menu.f:\
  $(d0root)/cdc_util/source/cdc_hst_menu.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdc_hst_menu.for | vmstounix > $(d0root)/cdc_util/source/cdc_hst_menu.f
$(d0root)/cdc_util/source/cdchit_cell.f:\
  $(d0root)/cdc_util/source/cdchit_cell.for\
  $(d0library)/inc/pi.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdchit_cell.for | vmstounix > $(d0root)/cdc_util/source/cdchit_cell.f
$(d0root)/cdc_util/source/cdchit_hits.f:\
  $(d0root)/cdc_util/source/cdchit_hits.for\
  $(d0library)/inc/pi.inc\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdchit_hits.for | vmstounix > $(d0root)/cdc_util/source/cdchit_hits.f
$(d0root)/cdc_util/source/cdchit_info.f:\
  $(d0root)/cdc_util/source/cdchit_info.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdchit_info.for | vmstounix > $(d0root)/cdc_util/source/cdchit_info.f
$(d0root)/cdc_util/source/cdchit_segm.f:\
  $(d0root)/cdc_util/source/cdchit_segm.for\
  $(d0library)/inc/pi.inc\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdchit_segm.for | vmstounix > $(d0root)/cdc_util/source/cdchit_segm.f
$(d0root)/cdc_util/source/cdchit_strip.f:\
  $(d0root)/cdc_util/source/cdchit_strip.for\
  $(d0library)/inc/pi.inc\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdchit_strip.for | vmstounix > $(d0root)/cdc_util/source/cdchit_strip.f
$(d0root)/cdc_util/source/cdcisa.f:\
  $(d0root)/cdc_util/source/cdcisa.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/links/izdrft.link\
  $(d0library)/inc/cdltrk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdcisa.for | vmstounix > $(d0root)/cdc_util/source/cdcisa.f
$(d0root)/cdc_util/source/cdclos.f:\
  $(d0root)/cdc_util/source/cdclos.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdclos.for | vmstounix > $(d0root)/cdc_util/source/cdclos.f
$(d0root)/cdc_util/source/cdcvel.f:\
  $(d0root)/cdc_util/source/cdcvel.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izdtth.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdcvel.for | vmstounix > $(d0root)/cdc_util/source/cdcvel.f
$(d0root)/cdc_util/source/cdgetz.f:\
  $(d0root)/cdc_util/source/cdgetz.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdresf.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdgetz.for | vmstounix > $(d0root)/cdc_util/source/cdgetz.f
$(d0root)/cdc_util/source/cdhfil.f:\
  $(d0root)/cdc_util/source/cdhfil.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdresf.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdhfil.for | vmstounix > $(d0root)/cdc_util/source/cdhfil.f
$(d0root)/cdc_util/source/cdhitl.f:\
  $(d0root)/cdc_util/source/cdhitl.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/links/izuser.link\
  $(d0library)/links/izdalh.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdhitl.for | vmstounix > $(d0root)/cdc_util/source/cdhitl.f
$(d0root)/cdc_util/source/cdhits.f:\
  $(d0root)/cdc_util/source/cdhits.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdccos.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdhits.for | vmstounix > $(d0root)/cdc_util/source/cdhits.f
$(d0root)/cdc_util/source/cdhtrk.f:\
  $(d0root)/cdc_util/source/cdhtrk.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdhtrk.for | vmstounix > $(d0root)/cdc_util/source/cdhtrk.f
$(d0root)/cdc_util/source/cdinit.f:\
  $(d0root)/cdc_util/source/cdinit.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/ddebug.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdinit.for | vmstounix > $(d0root)/cdc_util/source/cdinit.f
$(d0root)/cdc_util/source/cdistp.f:\
  $(d0root)/cdc_util/source/cdistp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izscdc.link\
  $(d0library)/links/izdpdh.link\
  $(d0library)/links/izdgnh.link\
  $(d0library)/links/izdtmh.link\
  $(d0library)/links/izdgeh.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdistp.for | vmstounix > $(d0root)/cdc_util/source/cdistp.f
$(d0root)/cdc_util/source/cdistr.f:\
  $(d0root)/cdc_util/source/cdistr.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisv1.link\
  $(d0library)/links/izisv2.link\
  $(d0library)/links/izisp1.link\
  $(d0library)/links/izisp2.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdistr.for | vmstounix > $(d0root)/cdc_util/source/cdistr.f
$(d0root)/cdc_util/source/cdnctr.f:\
  $(d0root)/cdc_util/source/cdnctr.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdnctr.for | vmstounix > $(d0root)/cdc_util/source/cdnctr.f
$(d0root)/cdc_util/source/cdpuls.f:\
  $(d0root)/cdc_util/source/cdpuls.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdresf.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/links/izcdd2.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdpuls.for | vmstounix > $(d0root)/cdc_util/source/cdpuls.f
$(d0root)/cdc_util/source/cdroad.f:\
  $(d0root)/cdc_util/source/cdroad.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdroad.for | vmstounix > $(d0root)/cdc_util/source/cdroad.f
$(d0root)/cdc_util/source/cdsave.f:\
  $(d0root)/cdc_util/source/cdsave.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/links/izdalh.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdsave.for | vmstounix > $(d0root)/cdc_util/source/cdsave.f
$(d0root)/cdc_util/source/cdsres.f:\
  $(d0root)/cdc_util/source/cdsres.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdresf.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdsres.for | vmstounix > $(d0root)/cdc_util/source/cdsres.f
$(d0root)/cdc_util/source/cdtfit.f:\
  $(d0root)/cdc_util/source/cdtfit.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdtfit.for | vmstounix > $(d0root)/cdc_util/source/cdtfit.f
$(d0root)/cdc_util/source/cdtrak.f:\
  $(d0root)/cdc_util/source/cdtrak.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdresf.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdtrak.for | vmstounix > $(d0root)/cdc_util/source/cdtrak.f
$(d0root)/cdc_util/source/cdtseg.f:\
  $(d0root)/cdc_util/source/cdtseg.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdtseg.for | vmstounix > $(d0root)/cdc_util/source/cdtseg.f
$(d0root)/cdc_util/source/cdtsto.f:\
  $(d0root)/cdc_util/source/cdtsto.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdtsto.for | vmstounix > $(d0root)/cdc_util/source/cdtsto.f
$(d0root)/cdc_util/source/cdunpk.f:\
  $(d0root)/cdc_util/source/cdunpk.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/ddebug.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdunpk.for | vmstounix > $(d0root)/cdc_util/source/cdunpk.f
$(d0root)/cdc_util/source/cdxyzl.f:\
  $(d0root)/cdc_util/source/cdxyzl.for\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/cdxyzl.for | vmstounix > $(d0root)/cdc_util/source/cdxyzl.f
$(d0root)/cdc_util/source/crhitl.f:\
  $(d0root)/cdc_util/source/crhitl.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/links/izuser.link\
  $(d0library)/links/izdalh.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/crhitl.for | vmstounix > $(d0root)/cdc_util/source/crhitl.f
$(d0root)/cdc_util/source/crhits.f:\
  $(d0root)/cdc_util/source/crhits.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/crhits.for | vmstounix > $(d0root)/cdc_util/source/crhits.f
$(d0root)/cdc_util/source/crtseg.f:\
  $(d0root)/cdc_util/source/crtseg.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/crtseg.for | vmstounix > $(d0root)/cdc_util/source/crtseg.f
$(d0root)/cdc_util/source/darea_correct.f:\
  $(d0root)/cdc_util/source/darea_correct.for\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/denvadj.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/darea_correct.for | vmstounix > $(d0root)/cdc_util/source/darea_correct.f
$(d0root)/cdc_util/source/dbkchd.f:\
  $(d0root)/cdc_util/source/dbkchd.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dbkchd.for | vmstounix > $(d0root)/cdc_util/source/dbkchd.f
$(d0root)/cdc_util/source/dbkchi.f:\
  $(d0root)/cdc_util/source/dbkchi.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dbkchi.for | vmstounix > $(d0root)/cdc_util/source/dbkchi.f
$(d0root)/cdc_util/source/dbkchz.f:\
  $(d0root)/cdc_util/source/dbkchz.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dbkchz.for | vmstounix > $(d0root)/cdc_util/source/dbkchz.f
$(d0root)/cdc_util/source/dbkdex.f:\
  $(d0root)/cdc_util/source/dbkdex.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dbkdex.for | vmstounix > $(d0root)/cdc_util/source/dbkdex.f
$(d0root)/cdc_util/source/dbkdlr.f:\
  $(d0root)/cdc_util/source/dbkdlr.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dbkdlr.for | vmstounix > $(d0root)/cdc_util/source/dbkdlr.f
$(d0root)/cdc_util/source/dbkdsc.f:\
  $(d0root)/cdc_util/source/dbkdsc.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dbkdsc.for | vmstounix > $(d0root)/cdc_util/source/dbkdsc.f
$(d0root)/cdc_util/source/dbkeff.f:\
  $(d0root)/cdc_util/source/dbkeff.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dbkeff.for | vmstounix > $(d0root)/cdc_util/source/dbkeff.f
$(d0root)/cdc_util/source/dbkped.f:\
  $(d0root)/cdc_util/source/dbkped.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dbkped.for | vmstounix > $(d0root)/cdc_util/source/dbkped.f
$(d0root)/cdc_util/source/dbkpul.f:\
  $(d0root)/cdc_util/source/dbkpul.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dbkpul.for | vmstounix > $(d0root)/cdc_util/source/dbkpul.f
$(d0root)/cdc_util/source/dbkswr.f:\
  $(d0root)/cdc_util/source/dbkswr.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dbkswr.for | vmstounix > $(d0root)/cdc_util/source/dbkswr.f
$(d0root)/cdc_util/source/dbnchk.f:\
  $(d0root)/cdc_util/source/dbnchk.for\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/ddebug.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dbnchk.for | vmstounix > $(d0root)/cdc_util/source/dbnchk.f
$(d0root)/cdc_util/source/dc_envadj.f:\
  $(d0root)/cdc_util/source/dc_envadj.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/dbmukey.inc\
  $(d0library)/inc/denvadj.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dc_envadj.for | vmstounix > $(d0root)/cdc_util/source/dc_envadj.f
$(d0root)/cdc_util/source/dcdedx.f:\
  $(d0root)/cdc_util/source/dcdedx.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdltrk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dcdedx.for | vmstounix > $(d0root)/cdc_util/source/dcdedx.f
$(d0root)/cdc_util/source/dchkt_edge.f:\
  $(d0root)/cdc_util/source/dchkt_edge.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dchkt_edge.for | vmstounix > $(d0root)/cdc_util/source/dchkt_edge.f
$(d0root)/cdc_util/source/dchkz_edge.f:\
  $(d0root)/cdc_util/source/dchkz_edge.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dchkz_edge.for | vmstounix > $(d0root)/cdc_util/source/dchkz_edge.f
$(d0root)/cdc_util/source/dclimb.f:\
  $(d0root)/cdc_util/source/dclimb.for\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/dltpar.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dclimb.for | vmstounix > $(d0root)/cdc_util/source/dclimb.f
$(d0root)/cdc_util/source/dcoder.f:\
  $(d0root)/cdc_util/source/dcoder.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dcoder.for | vmstounix > $(d0root)/cdc_util/source/dcoder.f
$(d0root)/cdc_util/source/dcrtmp.f:\
  $(d0root)/cdc_util/source/dcrtmp.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dcrtmp.for | vmstounix > $(d0root)/cdc_util/source/dcrtmp.f
$(d0root)/cdc_util/source/ddbini.f:\
  $(d0root)/cdc_util/source/ddbini.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/ddbini.for | vmstounix > $(d0root)/cdc_util/source/ddbini.f
$(d0root)/cdc_util/source/ddelet.f:\
  $(d0root)/cdc_util/source/ddelet.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/links/izuser.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/ddelet.for | vmstounix > $(d0root)/cdc_util/source/ddelet.f
$(d0root)/cdc_util/source/dfitch.f:\
  $(d0root)/cdc_util/source/dfitch.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdcpar.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izuser.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dfitch.for | vmstounix > $(d0root)/cdc_util/source/dfitch.f
$(d0root)/cdc_util/source/dflhst.f:\
  $(d0root)/cdc_util/source/dflhst.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dflhst.for | vmstounix > $(d0root)/cdc_util/source/dflhst.f
$(d0root)/cdc_util/source/dflped.f:\
  $(d0root)/cdc_util/source/dflped.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdresf.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dflped.for | vmstounix > $(d0root)/cdc_util/source/dflped.f
$(d0root)/cdc_util/source/dflpul.f:\
  $(d0root)/cdc_util/source/dflpul.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dflpul.for | vmstounix > $(d0root)/cdc_util/source/dflpul.f
$(d0root)/cdc_util/source/dflseg.f:\
  $(d0root)/cdc_util/source/dflseg.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dflseg.for | vmstounix > $(d0root)/cdc_util/source/dflseg.f
$(d0root)/cdc_util/source/dfltrk.f:\
  $(d0root)/cdc_util/source/dfltrk.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dfltrk.for | vmstounix > $(d0root)/cdc_util/source/dfltrk.f
$(d0root)/cdc_util/source/dftlin.f:\
  $(d0root)/cdc_util/source/dftlin.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dftlin.for | vmstounix > $(d0root)/cdc_util/source/dftlin.f
$(d0root)/cdc_util/source/dgnini.f:\
  $(d0root)/cdc_util/source/dgnini.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/links/izdgch.link\
  $(d0library)/links/izcrcp.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dgnini.for | vmstounix > $(d0root)/cdc_util/source/dgnini.f
$(d0root)/cdc_util/source/dgtgns.f:\
  $(d0root)/cdc_util/source/dgtgns.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/links/izdgns.link\
  $(d0library)/links/izdgch.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dgtgns.for | vmstounix > $(d0root)/cdc_util/source/dgtgns.f
$(d0root)/cdc_util/source/dgtpds.f:\
  $(d0root)/cdc_util/source/dgtpds.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdped.link\
  $(d0library)/links/izdpch.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dgtpds.for | vmstounix > $(d0root)/cdc_util/source/dgtpds.f
$(d0root)/cdc_util/source/dgttms.f:\
  $(d0root)/cdc_util/source/dgttms.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/links/izdtms.link\
  $(d0library)/links/izdtch.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dgttms.for | vmstounix > $(d0root)/cdc_util/source/dgttms.f
$(d0root)/cdc_util/source/dhit_mc_to_data.f:\
  $(d0root)/cdc_util/source/dhit_mc_to_data.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/params/byte_order.params
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dhit_mc_to_data.for | vmstounix > $(d0root)/cdc_util/source/dhit_mc_to_data.f
$(d0root)/cdc_util/source/dhit_unpack.f:\
  $(d0root)/cdc_util/source/dhit_unpack.for\
  $(d0library)/params/byte_order.params
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dhit_unpack.for | vmstounix > $(d0root)/cdc_util/source/dhit_unpack.f
$(d0root)/cdc_util/source/dhitfl.f:\
  $(d0root)/cdc_util/source/dhitfl.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/params/byte_order.params
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dhitfl.for | vmstounix > $(d0root)/cdc_util/source/dhitfl.f
$(d0root)/cdc_util/source/dhitst.f:\
  $(d0root)/cdc_util/source/dhitst.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dhitst.for | vmstounix > $(d0root)/cdc_util/source/dhitst.f
$(d0root)/cdc_util/source/dhstbk.f:\
  $(d0root)/cdc_util/source/dhstbk.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dhstbk.for | vmstounix > $(d0root)/cdc_util/source/dhstbk.f
$(d0root)/cdc_util/source/dhstfl.f:\
  $(d0root)/cdc_util/source/dhstfl.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dhstfl.for | vmstounix > $(d0root)/cdc_util/source/dhstfl.f
$(d0root)/cdc_util/source/dhstou.f:\
  $(d0root)/cdc_util/source/dhstou.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dhstou.for | vmstounix > $(d0root)/cdc_util/source/dhstou.f
$(d0root)/cdc_util/source/dhtchk.f:\
  $(d0root)/cdc_util/source/dhtchk.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dhtchk.for | vmstounix > $(d0root)/cdc_util/source/dhtchk.f
$(d0root)/cdc_util/source/disa_mark.f:\
  $(d0root)/cdc_util/source/disa_mark.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/disa_mark.for | vmstounix > $(d0root)/cdc_util/source/disa_mark.f
$(d0root)/cdc_util/source/dldtsg.f:\
  $(d0root)/cdc_util/source/dldtsg.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdcpar.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dldtsg.for | vmstounix > $(d0root)/cdc_util/source/dldtsg.f
$(d0root)/cdc_util/source/dlinfo.f:\
  $(d0root)/cdc_util/source/dlinfo.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dlinfo.for | vmstounix > $(d0root)/cdc_util/source/dlinfo.f
$(d0root)/cdc_util/source/dmdstg.f:\
  $(d0root)/cdc_util/source/dmdstg.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dmdstg.for | vmstounix > $(d0root)/cdc_util/source/dmdstg.f
$(d0root)/cdc_util/source/dmonpl.f:\
  $(d0root)/cdc_util/source/dmonpl.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/inc/cdpara.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dmonpl.for | vmstounix > $(d0root)/cdc_util/source/dmonpl.f
$(d0root)/cdc_util/source/dnlcor.f:\
  $(d0root)/cdc_util/source/dnlcor.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dnlcor.for | vmstounix > $(d0root)/cdc_util/source/dnlcor.f
$(d0root)/cdc_util/source/dpdini.f:\
  $(d0root)/cdc_util/source/dpdini.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/links/izdpch.link\
  $(d0library)/links/izcrcp.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dpdini.for | vmstounix > $(d0root)/cdc_util/source/dpdini.f
$(d0root)/cdc_util/source/drddbg.f:\
  $(d0root)/cdc_util/source/drddbg.for\
  $(d0library)/inc/ddebug.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/drddbg.for | vmstounix > $(d0root)/cdc_util/source/drddbg.f
$(d0root)/cdc_util/source/dsec_from_dhit.f:\
  $(d0root)/cdc_util/source/dsec_from_dhit.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdpara.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dsec_from_dhit.for | vmstounix > $(d0root)/cdc_util/source/dsec_from_dhit.f
$(d0root)/cdc_util/source/dsecht.f:\
  $(d0root)/cdc_util/source/dsecht.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/cdccos.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dsecht.for | vmstounix > $(d0root)/cdc_util/source/dsecht.f
$(d0root)/cdc_util/source/dseglt.f:\
  $(d0root)/cdc_util/source/dseglt.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdcpar.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/dltpar.inc\
  $(d0library)/links/izuser.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dseglt.for | vmstounix > $(d0root)/cdc_util/source/dseglt.f
$(d0root)/cdc_util/source/dsegme.f:\
  $(d0root)/cdc_util/source/dsegme.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izuser.link\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/inc/cdcpar.inc\
  $(d0library)/inc/cdloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dsegme.for | vmstounix > $(d0root)/cdc_util/source/dsegme.f
$(d0root)/cdc_util/source/dsgsec.f:\
  $(d0root)/cdc_util/source/dsgsec.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dsgsec.for | vmstounix > $(d0root)/cdc_util/source/dsgsec.f
$(d0root)/cdc_util/source/dstp_fetch.f:\
  $(d0root)/cdc_util/source/dstp_fetch.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdpdh.link\
  $(d0library)/links/izdgnh.link\
  $(d0library)/links/izdtmh.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dstp_fetch.for | vmstounix > $(d0root)/cdc_util/source/dstp_fetch.f
$(d0root)/cdc_util/source/dsvchn.f:\
  $(d0root)/cdc_util/source/dsvchn.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdcpar.inc\
  $(d0library)/inc/dltpar.inc\
  $(d0library)/links/izuser.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dsvchn.for | vmstounix > $(d0root)/cdc_util/source/dsvchn.f
$(d0root)/cdc_util/source/dtdefd.f:\
  $(d0root)/cdc_util/source/dtdefd.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtdefd.for | vmstounix > $(d0root)/cdc_util/source/dtdefd.f
$(d0root)/cdc_util/source/dtlink.f:\
  $(d0root)/cdc_util/source/dtlink.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdcpar.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdloca.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/dltpar.inc\
  $(d0library)/links/izuser.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtlink.for | vmstounix > $(d0root)/cdc_util/source/dtlink.f
$(d0root)/cdc_util/source/dtmini.f:\
  $(d0root)/cdc_util/source/dtmini.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/links/izdtch.link\
  $(d0library)/links/izcrcp.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtmini.for | vmstounix > $(d0root)/cdc_util/source/dtmini.f
$(d0root)/cdc_util/source/dtrdia.f:\
  $(d0root)/cdc_util/source/dtrdia.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtrdia.for | vmstounix > $(d0root)/cdc_util/source/dtrdia.f
$(d0root)/cdc_util/source/dtrdmp.f:\
  $(d0root)/cdc_util/source/dtrdmp.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtrdmp.for | vmstounix > $(d0root)/cdc_util/source/dtrdmp.f
$(d0root)/cdc_util/source/dtrdrp.f:\
  $(d0root)/cdc_util/source/dtrdrp.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtrdrp.for | vmstounix > $(d0root)/cdc_util/source/dtrdrp.f
$(d0root)/cdc_util/source/dtrevt.f:\
  $(d0root)/cdc_util/source/dtrevt.for\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/links/izisae.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtrevt.for | vmstounix > $(d0root)/cdc_util/source/dtrevt.f
$(d0root)/cdc_util/source/dtrhbk.f:\
  $(d0root)/cdc_util/source/dtrhbk.for\
  $(d0library)/inc/cdpara.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtrhbk.for | vmstounix > $(d0root)/cdc_util/source/dtrhbk.f
$(d0root)/cdc_util/source/dtrini.f:\
  $(d0root)/cdc_util/source/dtrini.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtrini.for | vmstounix > $(d0root)/cdc_util/source/dtrini.f
$(d0root)/cdc_util/source/dtrk_edge.f:\
  $(d0root)/cdc_util/source/dtrk_edge.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/ddebug.inc\
  $(d0library)/inc/cdresf.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdltrk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtrk_edge.for | vmstounix > $(d0root)/cdc_util/source/dtrk_edge.f
$(d0root)/cdc_util/source/dtrk_fix.f:\
  $(d0root)/cdc_util/source/dtrk_fix.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtrk_fix.for | vmstounix > $(d0root)/cdc_util/source/dtrk_fix.f
$(d0root)/cdc_util/source/dtrk_fix_dedx.f:\
  $(d0root)/cdc_util/source/dtrk_fix_dedx.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtrk_fix_dedx.for | vmstounix > $(d0root)/cdc_util/source/dtrk_fix_dedx.f
$(d0root)/cdc_util/source/dtrk_fixz.f:\
  $(d0root)/cdc_util/source/dtrk_fixz.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtrk_fixz.for | vmstounix > $(d0root)/cdc_util/source/dtrk_fixz.f
$(d0root)/cdc_util/source/dtrkhits.f:\
  $(d0root)/cdc_util/source/dtrkhits.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtrkhits.for | vmstounix > $(d0root)/cdc_util/source/dtrkhits.f
$(d0root)/cdc_util/source/dtrkht.f:\
  $(d0root)/cdc_util/source/dtrkht.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtrkht.for | vmstounix > $(d0root)/cdc_util/source/dtrkht.f
$(d0root)/cdc_util/source/dtrksg.f:\
  $(d0root)/cdc_util/source/dtrksg.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtrksg.for | vmstounix > $(d0root)/cdc_util/source/dtrksg.f
$(d0root)/cdc_util/source/dtrpar.f:\
  $(d0root)/cdc_util/source/dtrpar.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtrpar.for | vmstounix > $(d0root)/cdc_util/source/dtrpar.f
$(d0root)/cdc_util/source/dtrusm.f:\
  $(d0root)/cdc_util/source/dtrusm.for\
  $(d0library)/inc/cdpara.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtrusm.for | vmstounix > $(d0root)/cdc_util/source/dtrusm.f
$(d0root)/cdc_util/source/dtsg_z.f:\
  $(d0root)/cdc_util/source/dtsg_z.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dtsg_z.for | vmstounix > $(d0root)/cdc_util/source/dtsg_z.f
$(d0root)/cdc_util/source/dxyerr.f:\
  $(d0root)/cdc_util/source/dxyerr.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/dxyerr.for | vmstounix > $(d0root)/cdc_util/source/dxyerr.f
$(d0root)/cdc_util/source/fill_cdc_l2hitlist.f:\
  $(d0root)/cdc_util/source/fill_cdc_l2hitlist.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/fill_cdc_l2hitlist.for | vmstounix > $(d0root)/cdc_util/source/fill_cdc_l2hitlist.f
$(d0root)/cdc_util/source/fill_dhitlist.f:\
  $(d0root)/cdc_util/source/fill_dhitlist.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/fill_dhitlist.for | vmstounix > $(d0root)/cdc_util/source/fill_dhitlist.f
$(d0root)/cdc_util/source/fldsec.f:\
  $(d0root)/cdc_util/source/fldsec.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/fldsec.for | vmstounix > $(d0root)/cdc_util/source/fldsec.f
$(d0root)/cdc_util/source/gtdhit.f:\
  $(d0root)/cdc_util/source/gtdhit.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gtdhit.for | vmstounix > $(d0root)/cdc_util/source/gtdhit.f
$(d0root)/cdc_util/source/gtditr.f:\
  $(d0root)/cdc_util/source/gtditr.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gtditr.for | vmstounix > $(d0root)/cdc_util/source/gtditr.f
$(d0root)/cdc_util/source/gtdtrh.f:\
  $(d0root)/cdc_util/source/gtdtrh.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gtdtrh.for | vmstounix > $(d0root)/cdc_util/source/gtdtrh.f
$(d0root)/cdc_util/source/gtdtrk.f:\
  $(d0root)/cdc_util/source/gtdtrk.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gtdtrk.for | vmstounix > $(d0root)/cdc_util/source/gtdtrk.f
$(d0root)/cdc_util/source/gtdtrk_link.f:\
  $(d0root)/cdc_util/source/gtdtrk_link.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gtdtrk_link.for | vmstounix > $(d0root)/cdc_util/source/gtdtrk_link.f
$(d0root)/cdc_util/source/gzcdch.f:\
  $(d0root)/cdc_util/source/gzcdch.for\
  $(d0library)/links/izcdch.link\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzcdch.for | vmstounix > $(d0root)/cdc_util/source/gzcdch.f
$(d0root)/cdc_util/source/gzcdd2.f:\
  $(d0root)/cdc_util/source/gzcdd2.for\
  $(d0library)/links/izcdd2.link\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzcdd2.for | vmstounix > $(d0root)/cdc_util/source/gzcdd2.f
$(d0root)/cdc_util/source/gzcdh2.f:\
  $(d0root)/cdc_util/source/gzcdh2.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcdh2.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzcdh2.for | vmstounix > $(d0root)/cdc_util/source/gzcdh2.f
$(d0root)/cdc_util/source/gzdalh.f:\
  $(d0root)/cdc_util/source/gzdalh.for\
  $(d0library)/links/izdalh.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdalh.for | vmstounix > $(d0root)/cdc_util/source/gzdalh.f
$(d0root)/cdc_util/source/gzdall.f:\
  $(d0root)/cdc_util/source/gzdall.for\
  $(d0library)/links/izdall.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdall.for | vmstounix > $(d0root)/cdc_util/source/gzdall.f
$(d0root)/cdc_util/source/gzdals.f:\
  $(d0root)/cdc_util/source/gzdals.for\
  $(d0library)/links/izdals.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdals.for | vmstounix > $(d0root)/cdc_util/source/gzdals.f
$(d0root)/cdc_util/source/gzdcbd.f:\
  $(d0root)/cdc_util/source/gzdcbd.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdcbd.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdcbd.for | vmstounix > $(d0root)/cdc_util/source/gzdcbd.f
$(d0root)/cdc_util/source/gzdcda.f:\
  $(d0root)/cdc_util/source/gzdcda.for\
  $(d0library)/links/izdcda.link\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdcda.for | vmstounix > $(d0root)/cdc_util/source/gzdcda.f
$(d0root)/cdc_util/source/gzddlp.f:\
  $(d0root)/cdc_util/source/gzddlp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izddlp.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzddlp.for | vmstounix > $(d0root)/cdc_util/source/gzddlp.f
$(d0root)/cdc_util/source/gzdgeh.f:\
  $(d0root)/cdc_util/source/gzdgeh.for\
  $(d0library)/links/izdgeh.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdgeh.for | vmstounix > $(d0root)/cdc_util/source/gzdgeh.f
$(d0root)/cdc_util/source/gzdgnh.f:\
  $(d0root)/cdc_util/source/gzdgnh.for\
  $(d0library)/links/izdgnh.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdgnh.for | vmstounix > $(d0root)/cdc_util/source/gzdgnh.f
$(d0root)/cdc_util/source/gzdgnl.f:\
  $(d0root)/cdc_util/source/gzdgnl.for\
  $(d0library)/links/izdgnl.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdgnl.for | vmstounix > $(d0root)/cdc_util/source/gzdgnl.f
$(d0root)/cdc_util/source/gzdhit.f:\
  $(d0root)/cdc_util/source/gzdhit.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izdhit.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdhit.for | vmstounix > $(d0root)/cdc_util/source/gzdhit.f
$(d0root)/cdc_util/source/gzditr.f:\
  $(d0root)/cdc_util/source/gzditr.for\
  $(d0library)/links/izditr.link\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzditr.for | vmstounix > $(d0root)/cdc_util/source/gzditr.f
$(d0root)/cdc_util/source/gzdlyr.f:\
  $(d0root)/cdc_util/source/gzdlyr.for\
  $(d0library)/links/izdlyr.link\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdlyr.for | vmstounix > $(d0root)/cdc_util/source/gzdlyr.f
$(d0root)/cdc_util/source/gzdmat.f:\
  $(d0root)/cdc_util/source/gzdmat.for\
  $(d0library)/links/izdmat.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdmat.for | vmstounix > $(d0root)/cdc_util/source/gzdmat.f
$(d0root)/cdc_util/source/gzdnli.f:\
  $(d0root)/cdc_util/source/gzdnli.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdnli.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdnli.for | vmstounix > $(d0root)/cdc_util/source/gzdnli.f
$(d0root)/cdc_util/source/gzdnlo.f:\
  $(d0root)/cdc_util/source/gzdnlo.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdnlo.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdnlo.for | vmstounix > $(d0root)/cdc_util/source/gzdnlo.f
$(d0root)/cdc_util/source/gzdpdh.f:\
  $(d0root)/cdc_util/source/gzdpdh.for\
  $(d0library)/links/izdpdh.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdpdh.for | vmstounix > $(d0root)/cdc_util/source/gzdpdh.f
$(d0root)/cdc_util/source/gzdpdl.f:\
  $(d0root)/cdc_util/source/gzdpdl.for\
  $(d0library)/links/izdpdl.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdpdl.for | vmstounix > $(d0root)/cdc_util/source/gzdpdl.f
$(d0root)/cdc_util/source/gzdrft.f:\
  $(d0root)/cdc_util/source/gzdrft.for\
  $(d0library)/links/izdrft.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdrft.for | vmstounix > $(d0root)/cdc_util/source/gzdrft.f
$(d0root)/cdc_util/source/gzdsec.f:\
  $(d0root)/cdc_util/source/gzdsec.for\
  $(d0library)/links/izdsec.link\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdsec.for | vmstounix > $(d0root)/cdc_util/source/gzdsec.f
$(d0root)/cdc_util/source/gzdswp.f:\
  $(d0root)/cdc_util/source/gzdswp.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdswp.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdswp.for | vmstounix > $(d0root)/cdc_util/source/gzdswp.f
$(d0root)/cdc_util/source/gzdtmd.f:\
  $(d0root)/cdc_util/source/gzdtmd.for\
  $(d0library)/links/izdtmd.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdtmd.for | vmstounix > $(d0root)/cdc_util/source/gzdtmd.f
$(d0root)/cdc_util/source/gzdtmh.f:\
  $(d0root)/cdc_util/source/gzdtmh.for\
  $(d0library)/links/izdtmh.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdtmh.for | vmstounix > $(d0root)/cdc_util/source/gzdtmh.f
$(d0root)/cdc_util/source/gzdtmw.f:\
  $(d0root)/cdc_util/source/gzdtmw.for\
  $(d0library)/links/izdtmw.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdtmw.for | vmstounix > $(d0root)/cdc_util/source/gzdtmw.f
$(d0root)/cdc_util/source/gzdtrh.f:\
  $(d0root)/cdc_util/source/gzdtrh.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izdtrh.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdtrh.for | vmstounix > $(d0root)/cdc_util/source/gzdtrh.f
$(d0root)/cdc_util/source/gzdtrk.f:\
  $(d0root)/cdc_util/source/gzdtrk.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izdtrh.link\
  $(d0library)/links/izdtrk.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdtrk.for | vmstounix > $(d0root)/cdc_util/source/gzdtrk.f
$(d0root)/cdc_util/source/gzdtsg.f:\
  $(d0root)/cdc_util/source/gzdtsg.for\
  $(d0library)/links/izdtsg.link\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdtsg.for | vmstounix > $(d0root)/cdc_util/source/gzdtsg.f
$(d0root)/cdc_util/source/gzdtva.f:\
  $(d0root)/cdc_util/source/gzdtva.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdtva.for | vmstounix > $(d0root)/cdc_util/source/gzdtva.f
$(d0root)/cdc_util/source/gzdtvh.f:\
  $(d0root)/cdc_util/source/gzdtvh.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izdtvh.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdtvh.for | vmstounix > $(d0root)/cdc_util/source/gzdtvh.f
$(d0root)/cdc_util/source/gzdwal.f:\
  $(d0root)/cdc_util/source/gzdwal.for\
  $(d0library)/links/izdwal.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzdwal.for | vmstounix > $(d0root)/cdc_util/source/gzdwal.f
$(d0root)/cdc_util/source/gzl2ch.f:\
  $(d0root)/cdc_util/source/gzl2ch.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izl2ch.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzl2ch.for | vmstounix > $(d0root)/cdc_util/source/gzl2ch.f
$(d0root)/cdc_util/source/gzpsht.f:\
  $(d0root)/cdc_util/source/gzpsht.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpsht.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzpsht.for | vmstounix > $(d0root)/cdc_util/source/gzpsht.f
$(d0root)/cdc_util/source/gzpsrw.f:\
  $(d0root)/cdc_util/source/gzpsrw.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpsrw.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzpsrw.for | vmstounix > $(d0root)/cdc_util/source/gzpsrw.f
$(d0root)/cdc_util/source/gzrtst.f:\
  $(d0root)/cdc_util/source/gzrtst.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izrtst.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzrtst.for | vmstounix > $(d0root)/cdc_util/source/gzrtst.f
$(d0root)/cdc_util/source/gzscdc.f:\
  $(d0root)/cdc_util/source/gzscdc.for\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izscdc.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzscdc.for | vmstounix > $(d0root)/cdc_util/source/gzscdc.f
$(d0root)/cdc_util/source/gzt0dh.f:\
  $(d0root)/cdc_util/source/gzt0dh.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0dh.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzt0dh.for | vmstounix > $(d0root)/cdc_util/source/gzt0dh.f
$(d0root)/cdc_util/source/gzt0fh.f:\
  $(d0root)/cdc_util/source/gzt0fh.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0fh.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzt0fh.for | vmstounix > $(d0root)/cdc_util/source/gzt0fh.f
$(d0root)/cdc_util/source/gzt0ht.f:\
  $(d0root)/cdc_util/source/gzt0ht.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0ht.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzt0ht.for | vmstounix > $(d0root)/cdc_util/source/gzt0ht.f
$(d0root)/cdc_util/source/gzt0rw.f:\
  $(d0root)/cdc_util/source/gzt0rw.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0rw.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzt0rw.for | vmstounix > $(d0root)/cdc_util/source/gzt0rw.f
$(d0root)/cdc_util/source/gzt0td.f:\
  $(d0root)/cdc_util/source/gzt0td.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0td.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzt0td.for | vmstounix > $(d0root)/cdc_util/source/gzt0td.f
$(d0root)/cdc_util/source/gzt0th.f:\
  $(d0root)/cdc_util/source/gzt0th.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0th.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzt0th.for | vmstounix > $(d0root)/cdc_util/source/gzt0th.f
$(d0root)/cdc_util/source/gzt0tr.f:\
  $(d0root)/cdc_util/source/gzt0tr.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0tr.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzt0tr.for | vmstounix > $(d0root)/cdc_util/source/gzt0tr.f
$(d0root)/cdc_util/source/gzt0tz.f:\
  $(d0root)/cdc_util/source/gzt0tz.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izt0tz.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzt0tz.for | vmstounix > $(d0root)/cdc_util/source/gzt0tz.f
$(d0root)/cdc_util/source/gzupgd.f:\
  $(d0root)/cdc_util/source/gzupgd.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izupgd.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzupgd.for | vmstounix > $(d0root)/cdc_util/source/gzupgd.f
$(d0root)/cdc_util/source/gzzdht.f:\
  $(d0root)/cdc_util/source/gzzdht.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izzdht.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzzdht.for | vmstounix > $(d0root)/cdc_util/source/gzzdht.f
$(d0root)/cdc_util/source/gzzdrw.f:\
  $(d0root)/cdc_util/source/gzzdrw.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izzdrw.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/gzzdrw.for | vmstounix > $(d0root)/cdc_util/source/gzzdrw.f
$(d0root)/cdc_util/source/hbaver.f:\
  $(d0root)/cdc_util/source/hbaver.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/hbaver.for | vmstounix > $(d0root)/cdc_util/source/hbaver.f
$(d0root)/cdc_util/source/icell.f:\
  $(d0root)/cdc_util/source/icell.for\
  $(d0library)/inc/pi.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/icell.for | vmstounix > $(d0root)/cdc_util/source/icell.f
$(d0root)/cdc_util/source/lcheck.f:\
  $(d0root)/cdc_util/source/lcheck.for\
  $(d0library)/inc/pi.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/lcheck.for | vmstounix > $(d0root)/cdc_util/source/lcheck.f
$(d0root)/cdc_util/source/nwires_hit.f:\
  $(d0root)/cdc_util/source/nwires_hit.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/nwires_hit.for | vmstounix > $(d0root)/cdc_util/source/nwires_hit.f
$(d0root)/cdc_util/source/prcdch.f:\
  $(d0root)/cdc_util/source/prcdch.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/prcdch.for | vmstounix > $(d0root)/cdc_util/source/prcdch.f
$(d0root)/cdc_util/source/prcdd2.f:\
  $(d0root)/cdc_util/source/prcdd2.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcdd2.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/prcdd2.for | vmstounix > $(d0root)/cdc_util/source/prcdd2.f
$(d0root)/cdc_util/source/prdcda.f:\
  $(d0root)/cdc_util/source/prdcda.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/prdcda.for | vmstounix > $(d0root)/cdc_util/source/prdcda.f
$(d0root)/cdc_util/source/prditr.f:\
  $(d0root)/cdc_util/source/prditr.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdltrk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/prditr.for | vmstounix > $(d0root)/cdc_util/source/prditr.f
$(d0root)/cdc_util/source/prdsec.f:\
  $(d0root)/cdc_util/source/prdsec.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/prdsec.for | vmstounix > $(d0root)/cdc_util/source/prdsec.f
$(d0root)/cdc_util/source/prdtrh.f:\
  $(d0root)/cdc_util/source/prdtrh.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/prdtrh.for | vmstounix > $(d0root)/cdc_util/source/prdtrh.f
$(d0root)/cdc_util/source/prdtrk.f:\
  $(d0root)/cdc_util/source/prdtrk.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdltrk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/prdtrk.for | vmstounix > $(d0root)/cdc_util/source/prdtrk.f
$(d0root)/cdc_util/source/prdtsg.f:\
  $(d0root)/cdc_util/source/prdtsg.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdltrk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/prdtsg.for | vmstounix > $(d0root)/cdc_util/source/prdtsg.f
$(d0root)/cdc_util/source/prrtst.f:\
  $(d0root)/cdc_util/source/prrtst.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izrtst.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/prrtst.for | vmstounix > $(d0root)/cdc_util/source/prrtst.f
$(d0root)/cdc_util/source/prt0dh.f:\
  $(d0root)/cdc_util/source/prt0dh.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/prt0dh.for | vmstounix > $(d0root)/cdc_util/source/prt0dh.f
$(d0root)/cdc_util/source/prt0ht.f:\
  $(d0root)/cdc_util/source/prt0ht.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/prt0ht.for | vmstounix > $(d0root)/cdc_util/source/prt0ht.f
$(d0root)/cdc_util/source/prt0rw.f:\
  $(d0root)/cdc_util/source/prt0rw.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/prt0rw.for | vmstounix > $(d0root)/cdc_util/source/prt0rw.f
$(d0root)/cdc_util/source/prt0td.f:\
  $(d0root)/cdc_util/source/prt0td.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/prt0td.for | vmstounix > $(d0root)/cdc_util/source/prt0td.f
$(d0root)/cdc_util/source/prt0th.f:\
  $(d0root)/cdc_util/source/prt0th.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/prt0th.for | vmstounix > $(d0root)/cdc_util/source/prt0th.f
$(d0root)/cdc_util/source/prt0tz.f:\
  $(d0root)/cdc_util/source/prt0tz.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/prt0tz.for | vmstounix > $(d0root)/cdc_util/source/prt0tz.f
$(d0root)/cdc_util/source/psrwfl.f:\
  $(d0root)/cdc_util/source/psrwfl.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcdd1.link\
  $(d0library)/links/izpsht.link\
  $(d0library)/links/izpsrw.link\
  $(d0library)/params/preshower.params
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/psrwfl.for | vmstounix > $(d0root)/cdc_util/source/psrwfl.f
$(d0root)/cdc_util/source/pudcda.f:\
  $(d0root)/cdc_util/source/pudcda.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/pudcda.for | vmstounix > $(d0root)/cdc_util/source/pudcda.f
$(d0root)/cdc_util/source/puditr.f:\
  $(d0root)/cdc_util/source/puditr.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/puditr.for | vmstounix > $(d0root)/cdc_util/source/puditr.f
$(d0root)/cdc_util/source/pudsec.f:\
  $(d0root)/cdc_util/source/pudsec.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/pudsec.for | vmstounix > $(d0root)/cdc_util/source/pudsec.f
$(d0root)/cdc_util/source/recdd2.f:\
  $(d0root)/cdc_util/source/recdd2.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/gcunit.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/links/izcdd2.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/recdd2.for | vmstounix > $(d0root)/cdc_util/source/recdd2.f
$(d0root)/cdc_util/source/rtstfl.f:\
  $(d0root)/cdc_util/source/rtstfl.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcdd1.link\
  $(d0library)/links/izrtst.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/rtstfl.for | vmstounix > $(d0root)/cdc_util/source/rtstfl.f
$(d0root)/cdc_util/source/squeeze.f:\
  $(d0root)/cdc_util/source/squeeze.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/squeeze.for | vmstounix > $(d0root)/cdc_util/source/squeeze.f
$(d0root)/cdc_util/source/t0devt.f:\
  $(d0root)/cdc_util/source/t0devt.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0drec.inc\
  $(d0library)/inc/t0dlnt1.inc\
  $(d0library)/inc/t0dlnt2.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/t0devt.for | vmstounix > $(d0root)/cdc_util/source/t0devt.f
$(d0root)/cdc_util/source/t0dhfl.f:\
  $(d0root)/cdc_util/source/t0dhfl.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0dlnt2.inc\
  $(d0library)/links/izt0dh.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/t0dhfl.for | vmstounix > $(d0root)/cdc_util/source/t0dhfl.f
$(d0root)/cdc_util/source/t0dini.f:\
  $(d0root)/cdc_util/source/t0dini.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/flags.inc\
  $(d0library)/inc/t0dlnt1.inc\
  $(d0library)/inc/t0dlnt2.inc\
  $(d0library)/inc/t0dlnt3.inc\
  $(d0library)/inc/t0dpos.inc\
  $(d0library)/inc/t0drec.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/t0dini.for | vmstounix > $(d0root)/cdc_util/source/t0dini.f
$(d0root)/cdc_util/source/t0dpar.f:\
  $(d0root)/cdc_util/source/t0dpar.for
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/t0dpar.for | vmstounix > $(d0root)/cdc_util/source/t0dpar.f
$(d0root)/cdc_util/source/t0dtrk.f:\
  $(d0root)/cdc_util/source/t0dtrk.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0drec.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/t0dtrk.for | vmstounix > $(d0root)/cdc_util/source/t0dtrk.f
$(d0root)/cdc_util/source/t0hitc.f:\
  $(d0root)/cdc_util/source/t0hitc.for\
  $(d0library)/inc/t0dpos.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/t0hitc.for | vmstounix > $(d0root)/cdc_util/source/t0hitc.f
$(d0root)/cdc_util/source/t0hits.f:\
  $(d0root)/cdc_util/source/t0hits.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0drec.inc\
  $(d0library)/params/t0d.params
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/t0hits.for | vmstounix > $(d0root)/cdc_util/source/t0hits.f
$(d0root)/cdc_util/source/t0rwfl.f:\
  $(d0root)/cdc_util/source/t0rwfl.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0dlnt2.inc\
  $(d0library)/links/izt0rw.link\
  $(d0library)/params/t0d.params
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/t0rwfl.for | vmstounix > $(d0root)/cdc_util/source/t0rwfl.f
$(d0root)/cdc_util/source/t0tdfl.f:\
  $(d0root)/cdc_util/source/t0tdfl.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0dlnt2.inc\
  $(d0library)/links/izt0td.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/t0tdfl.for | vmstounix > $(d0root)/cdc_util/source/t0tdfl.f
$(d0root)/cdc_util/source/t0trks.f:\
  $(d0root)/cdc_util/source/t0trks.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0drec.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/t0trks.for | vmstounix > $(d0root)/cdc_util/source/t0trks.f
$(d0root)/cdc_util/source/t0tzfl.f:\
  $(d0root)/cdc_util/source/t0tzfl.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/t0drec.inc\
  $(d0library)/inc/t0dlnt2.inc\
  $(d0library)/inc/t0dlnt3.inc\
  $(d0library)/links/izt0tz.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/t0tzfl.for | vmstounix > $(d0root)/cdc_util/source/t0tzfl.f
$(d0root)/cdc_util/source/zcdevt.f:\
  $(d0root)/cdc_util/source/zcdevt.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zcdrec.inc\
  $(d0library)/inc/zcdln1.inc\
  $(d0library)/inc/zcdln2.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/zcdevt.for | vmstounix > $(d0root)/cdc_util/source/zcdevt.f
$(d0root)/cdc_util/source/zcdhits.f:\
  $(d0root)/cdc_util/source/zcdhits.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zcdrec.inc\
  $(d0library)/params/zcd.params
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/zcdhits.for | vmstounix > $(d0root)/cdc_util/source/zcdhits.f
$(d0root)/cdc_util/source/zcdini.f:\
  $(d0root)/cdc_util/source/zcdini.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zcdln1.inc\
  $(d0library)/inc/zcdln2.inc\
  $(d0library)/inc/zcdln3.inc\
  $(d0library)/inc/zcdrec.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/zcdini.for | vmstounix > $(d0root)/cdc_util/source/zcdini.f
$(d0root)/cdc_util/source/zdrwfl.f:\
  $(d0root)/cdc_util/source/zdrwfl.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zcdln2.inc\
  $(d0library)/links/izzdrw.link\
  $(d0library)/links/izcdd1.link\
  $(d0library)/params/zcd.params
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/zdrwfl.for | vmstounix > $(d0root)/cdc_util/source/zdrwfl.f
$(d0root)/cdc_util/source/zfdcda.f:\
  $(d0root)/cdc_util/source/zfdcda.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/zfdcda.for | vmstounix > $(d0root)/cdc_util/source/zfdcda.f
$(d0root)/cdc_util/source/zfditr.f:\
  $(d0root)/cdc_util/source/zfditr.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/links/izditr.link
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/zfditr.for | vmstounix > $(d0root)/cdc_util/source/zfditr.f
$(d0root)/cdc_util/source/zfdsec.f:\
  $(d0root)/cdc_util/source/zfdsec.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/cdclnk.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/zfdsec.for | vmstounix > $(d0root)/cdc_util/source/zfdsec.f
$(d0root)/cdc_util/source/zfdtrk.f:\
  $(d0root)/cdc_util/source/zfdtrk.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdltrk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/zfdtrk.for | vmstounix > $(d0root)/cdc_util/source/zfdtrk.f
$(d0root)/cdc_util/source/zfdtsg.f:\
  $(d0root)/cdc_util/source/zfdtsg.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdpara.inc\
  $(d0library)/inc/cdltrk.inc\
  $(d0library)/inc/cdloca.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/zfdtsg.for | vmstounix > $(d0root)/cdc_util/source/zfdtsg.f
$(d0root)/cdc_util/source/zgdcda.f:\
  $(d0root)/cdc_util/source/zgdcda.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/zgdcda.for | vmstounix > $(d0root)/cdc_util/source/zgdcda.f
$(d0root)/cdc_util/source/zgdsec.f:\
  $(d0root)/cdc_util/source/zgdsec.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdclnk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/zgdsec.for | vmstounix > $(d0root)/cdc_util/source/zgdsec.f
$(d0root)/cdc_util/source/zgdtsg.f:\
  $(d0root)/cdc_util/source/zgdtsg.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/cdltrk.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/zgdtsg.for | vmstounix > $(d0root)/cdc_util/source/zgdtsg.f
$(d0root)/cdc_util/source/zhit.f:\
  $(d0root)/cdc_util/source/zhit.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/cdc_util/source/zhit.for | vmstounix > $(d0root)/cdc_util/source/zhit.f
for:
	touch $(d0root)/cdc_util/source/bkcdch.for
	touch $(d0root)/cdc_util/source/bkcdd2.for
	touch $(d0root)/cdc_util/source/bkdcda.for
	touch $(d0root)/cdc_util/source/bkdgbd.for
	touch $(d0root)/cdc_util/source/bkdgch.for
	touch $(d0root)/cdc_util/source/bkdgnh.for
	touch $(d0root)/cdc_util/source/bkdgns.for
	touch $(d0root)/cdc_util/source/bkdhit.for
	touch $(d0root)/cdc_util/source/bkdlyr.for
	touch $(d0root)/cdc_util/source/bkdpbd.for
	touch $(d0root)/cdc_util/source/bkdpch.for
	touch $(d0root)/cdc_util/source/bkdpdh.for
	touch $(d0root)/cdc_util/source/bkdped.for
	touch $(d0root)/cdc_util/source/bkdpzs.for
	touch $(d0root)/cdc_util/source/bkdsec.for
	touch $(d0root)/cdc_util/source/bkdtbd.for
	touch $(d0root)/cdc_util/source/bkdtch.for
	touch $(d0root)/cdc_util/source/bkdtmh.for
	touch $(d0root)/cdc_util/source/bkdtms.for
	touch $(d0root)/cdc_util/source/bkdtrh.for
	touch $(d0root)/cdc_util/source/bkdtrk.for
	touch $(d0root)/cdc_util/source/bkpsht.for
	touch $(d0root)/cdc_util/source/bkpsrw.for
	touch $(d0root)/cdc_util/source/bkrtst.for
	touch $(d0root)/cdc_util/source/bkscdc.for
	touch $(d0root)/cdc_util/source/bkt0ht.for
	touch $(d0root)/cdc_util/source/bkt0th.for
	touch $(d0root)/cdc_util/source/bkupgd.for
	touch $(d0root)/cdc_util/source/bkzdht.for
	touch $(d0root)/cdc_util/source/cdalgn.for
	touch $(d0root)/cdc_util/source/cdc_exm_anal.for
	touch $(d0root)/cdc_util/source/cdc_fix.for
	touch $(d0root)/cdc_util/source/cdc_hst_dispatch.for
	touch $(d0root)/cdc_util/source/cdc_hst_menu.for
	touch $(d0root)/cdc_util/source/cdchit_cell.for
	touch $(d0root)/cdc_util/source/cdchit_hits.for
	touch $(d0root)/cdc_util/source/cdchit_info.for
	touch $(d0root)/cdc_util/source/cdchit_segm.for
	touch $(d0root)/cdc_util/source/cdchit_strip.for
	touch $(d0root)/cdc_util/source/cdcisa.for
	touch $(d0root)/cdc_util/source/cdclos.for
	touch $(d0root)/cdc_util/source/cdcvel.for
	touch $(d0root)/cdc_util/source/cdgetz.for
	touch $(d0root)/cdc_util/source/cdhfil.for
	touch $(d0root)/cdc_util/source/cdhitl.for
	touch $(d0root)/cdc_util/source/cdhits.for
	touch $(d0root)/cdc_util/source/cdhtrk.for
	touch $(d0root)/cdc_util/source/cdinit.for
	touch $(d0root)/cdc_util/source/cdistp.for
	touch $(d0root)/cdc_util/source/cdistr.for
	touch $(d0root)/cdc_util/source/cdnctr.for
	touch $(d0root)/cdc_util/source/cdpuls.for
	touch $(d0root)/cdc_util/source/cdroad.for
	touch $(d0root)/cdc_util/source/cdsave.for
	touch $(d0root)/cdc_util/source/cdsres.for
	touch $(d0root)/cdc_util/source/cdtfit.for
	touch $(d0root)/cdc_util/source/cdtrak.for
	touch $(d0root)/cdc_util/source/cdtseg.for
	touch $(d0root)/cdc_util/source/cdtsto.for
	touch $(d0root)/cdc_util/source/cdunpk.for
	touch $(d0root)/cdc_util/source/cdxyzl.for
	touch $(d0root)/cdc_util/source/crhitl.for
	touch $(d0root)/cdc_util/source/crhits.for
	touch $(d0root)/cdc_util/source/crtseg.for
	touch $(d0root)/cdc_util/source/darea_correct.for
	touch $(d0root)/cdc_util/source/dbkchd.for
	touch $(d0root)/cdc_util/source/dbkchi.for
	touch $(d0root)/cdc_util/source/dbkchz.for
	touch $(d0root)/cdc_util/source/dbkdex.for
	touch $(d0root)/cdc_util/source/dbkdlr.for
	touch $(d0root)/cdc_util/source/dbkdsc.for
	touch $(d0root)/cdc_util/source/dbkeff.for
	touch $(d0root)/cdc_util/source/dbkped.for
	touch $(d0root)/cdc_util/source/dbkpul.for
	touch $(d0root)/cdc_util/source/dbkswr.for
	touch $(d0root)/cdc_util/source/dbnchk.for
	touch $(d0root)/cdc_util/source/dc_envadj.for
	touch $(d0root)/cdc_util/source/dcdedx.for
	touch $(d0root)/cdc_util/source/dchkt_edge.for
	touch $(d0root)/cdc_util/source/dchkz_edge.for
	touch $(d0root)/cdc_util/source/dclimb.for
	touch $(d0root)/cdc_util/source/dcoder.for
	touch $(d0root)/cdc_util/source/dcrtmp.for
	touch $(d0root)/cdc_util/source/ddbini.for
	touch $(d0root)/cdc_util/source/ddelet.for
	touch $(d0root)/cdc_util/source/dfitch.for
	touch $(d0root)/cdc_util/source/dflhst.for
	touch $(d0root)/cdc_util/source/dflped.for
	touch $(d0root)/cdc_util/source/dflpul.for
	touch $(d0root)/cdc_util/source/dflseg.for
	touch $(d0root)/cdc_util/source/dfltrk.for
	touch $(d0root)/cdc_util/source/dftlin.for
	touch $(d0root)/cdc_util/source/dgnini.for
	touch $(d0root)/cdc_util/source/dgtgns.for
	touch $(d0root)/cdc_util/source/dgtpds.for
	touch $(d0root)/cdc_util/source/dgttms.for
	touch $(d0root)/cdc_util/source/dhit_mc_to_data.for
	touch $(d0root)/cdc_util/source/dhit_unpack.for
	touch $(d0root)/cdc_util/source/dhitfl.for
	touch $(d0root)/cdc_util/source/dhitst.for
	touch $(d0root)/cdc_util/source/dhstbk.for
	touch $(d0root)/cdc_util/source/dhstfl.for
	touch $(d0root)/cdc_util/source/dhstou.for
	touch $(d0root)/cdc_util/source/dhtchk.for
	touch $(d0root)/cdc_util/source/disa_mark.for
	touch $(d0root)/cdc_util/source/dldtsg.for
	touch $(d0root)/cdc_util/source/dlinfo.for
	touch $(d0root)/cdc_util/source/dmdstg.for
	touch $(d0root)/cdc_util/source/dmonpl.for
	touch $(d0root)/cdc_util/source/dnlcor.for
	touch $(d0root)/cdc_util/source/dpdini.for
	touch $(d0root)/cdc_util/source/drddbg.for
	touch $(d0root)/cdc_util/source/dsec_from_dhit.for
	touch $(d0root)/cdc_util/source/dsecht.for
	touch $(d0root)/cdc_util/source/dseglt.for
	touch $(d0root)/cdc_util/source/dsegme.for
	touch $(d0root)/cdc_util/source/dsgsec.for
	touch $(d0root)/cdc_util/source/dstp_fetch.for
	touch $(d0root)/cdc_util/source/dsvchn.for
	touch $(d0root)/cdc_util/source/dtdefd.for
	touch $(d0root)/cdc_util/source/dtlink.for
	touch $(d0root)/cdc_util/source/dtmini.for
	touch $(d0root)/cdc_util/source/dtrdia.for
	touch $(d0root)/cdc_util/source/dtrdmp.for
	touch $(d0root)/cdc_util/source/dtrdrp.for
	touch $(d0root)/cdc_util/source/dtrevt.for
	touch $(d0root)/cdc_util/source/dtrhbk.for
	touch $(d0root)/cdc_util/source/dtrini.for
	touch $(d0root)/cdc_util/source/dtrk_edge.for
	touch $(d0root)/cdc_util/source/dtrk_fix.for
	touch $(d0root)/cdc_util/source/dtrk_fix_dedx.for
	touch $(d0root)/cdc_util/source/dtrk_fixz.for
	touch $(d0root)/cdc_util/source/dtrkhits.for
	touch $(d0root)/cdc_util/source/dtrkht.for
	touch $(d0root)/cdc_util/source/dtrksg.for
	touch $(d0root)/cdc_util/source/dtrpar.for
	touch $(d0root)/cdc_util/source/dtrusm.for
	touch $(d0root)/cdc_util/source/dtsg_z.for
	touch $(d0root)/cdc_util/source/dxyerr.for
	touch $(d0root)/cdc_util/source/fill_cdc_l2hitlist.for
	touch $(d0root)/cdc_util/source/fill_dhitlist.for
	touch $(d0root)/cdc_util/source/fldsec.for
	touch $(d0root)/cdc_util/source/gtdhit.for
	touch $(d0root)/cdc_util/source/gtditr.for
	touch $(d0root)/cdc_util/source/gtdtrh.for
	touch $(d0root)/cdc_util/source/gtdtrk.for
	touch $(d0root)/cdc_util/source/gtdtrk_link.for
	touch $(d0root)/cdc_util/source/gzcdch.for
	touch $(d0root)/cdc_util/source/gzcdd2.for
	touch $(d0root)/cdc_util/source/gzcdh2.for
	touch $(d0root)/cdc_util/source/gzdalh.for
	touch $(d0root)/cdc_util/source/gzdall.for
	touch $(d0root)/cdc_util/source/gzdals.for
	touch $(d0root)/cdc_util/source/gzdcbd.for
	touch $(d0root)/cdc_util/source/gzdcda.for
	touch $(d0root)/cdc_util/source/gzddlp.for
	touch $(d0root)/cdc_util/source/gzdgeh.for
	touch $(d0root)/cdc_util/source/gzdgnh.for
	touch $(d0root)/cdc_util/source/gzdgnl.for
	touch $(d0root)/cdc_util/source/gzdhit.for
	touch $(d0root)/cdc_util/source/gzditr.for
	touch $(d0root)/cdc_util/source/gzdlyr.for
	touch $(d0root)/cdc_util/source/gzdmat.for
	touch $(d0root)/cdc_util/source/gzdnli.for
	touch $(d0root)/cdc_util/source/gzdnlo.for
	touch $(d0root)/cdc_util/source/gzdpdh.for
	touch $(d0root)/cdc_util/source/gzdpdl.for
	touch $(d0root)/cdc_util/source/gzdrft.for
	touch $(d0root)/cdc_util/source/gzdsec.for
	touch $(d0root)/cdc_util/source/gzdswp.for
	touch $(d0root)/cdc_util/source/gzdtmd.for
	touch $(d0root)/cdc_util/source/gzdtmh.for
	touch $(d0root)/cdc_util/source/gzdtmw.for
	touch $(d0root)/cdc_util/source/gzdtrh.for
	touch $(d0root)/cdc_util/source/gzdtrk.for
	touch $(d0root)/cdc_util/source/gzdtsg.for
	touch $(d0root)/cdc_util/source/gzdtva.for
	touch $(d0root)/cdc_util/source/gzdtvh.for
	touch $(d0root)/cdc_util/source/gzdwal.for
	touch $(d0root)/cdc_util/source/gzl2ch.for
	touch $(d0root)/cdc_util/source/gzpsht.for
	touch $(d0root)/cdc_util/source/gzpsrw.for
	touch $(d0root)/cdc_util/source/gzrtst.for
	touch $(d0root)/cdc_util/source/gzscdc.for
	touch $(d0root)/cdc_util/source/gzt0dh.for
	touch $(d0root)/cdc_util/source/gzt0fh.for
	touch $(d0root)/cdc_util/source/gzt0ht.for
	touch $(d0root)/cdc_util/source/gzt0rw.for
	touch $(d0root)/cdc_util/source/gzt0td.for
	touch $(d0root)/cdc_util/source/gzt0th.for
	touch $(d0root)/cdc_util/source/gzt0tr.for
	touch $(d0root)/cdc_util/source/gzt0tz.for
	touch $(d0root)/cdc_util/source/gzupgd.for
	touch $(d0root)/cdc_util/source/gzzdht.for
	touch $(d0root)/cdc_util/source/gzzdrw.for
	touch $(d0root)/cdc_util/source/hbaver.for
	touch $(d0root)/cdc_util/source/icell.for
	touch $(d0root)/cdc_util/source/lcheck.for
	touch $(d0root)/cdc_util/source/nwires_hit.for
	touch $(d0root)/cdc_util/source/prcdch.for
	touch $(d0root)/cdc_util/source/prcdd2.for
	touch $(d0root)/cdc_util/source/prdcda.for
	touch $(d0root)/cdc_util/source/prditr.for
	touch $(d0root)/cdc_util/source/prdsec.for
	touch $(d0root)/cdc_util/source/prdtrh.for
	touch $(d0root)/cdc_util/source/prdtrk.for
	touch $(d0root)/cdc_util/source/prdtsg.for
	touch $(d0root)/cdc_util/source/prrtst.for
	touch $(d0root)/cdc_util/source/prt0dh.for
	touch $(d0root)/cdc_util/source/prt0ht.for
	touch $(d0root)/cdc_util/source/prt0rw.for
	touch $(d0root)/cdc_util/source/prt0td.for
	touch $(d0root)/cdc_util/source/prt0th.for
	touch $(d0root)/cdc_util/source/prt0tz.for
	touch $(d0root)/cdc_util/source/psrwfl.for
	touch $(d0root)/cdc_util/source/pudcda.for
	touch $(d0root)/cdc_util/source/puditr.for
	touch $(d0root)/cdc_util/source/pudsec.for
	touch $(d0root)/cdc_util/source/recdd2.for
	touch $(d0root)/cdc_util/source/rtstfl.for
	touch $(d0root)/cdc_util/source/squeeze.for
	touch $(d0root)/cdc_util/source/t0devt.for
	touch $(d0root)/cdc_util/source/t0dhfl.for
	touch $(d0root)/cdc_util/source/t0dini.for
	touch $(d0root)/cdc_util/source/t0dpar.for
	touch $(d0root)/cdc_util/source/t0dtrk.for
	touch $(d0root)/cdc_util/source/t0hitc.for
	touch $(d0root)/cdc_util/source/t0hits.for
	touch $(d0root)/cdc_util/source/t0rwfl.for
	touch $(d0root)/cdc_util/source/t0tdfl.for
	touch $(d0root)/cdc_util/source/t0trks.for
	touch $(d0root)/cdc_util/source/t0tzfl.for
	touch $(d0root)/cdc_util/source/zcdevt.for
	touch $(d0root)/cdc_util/source/zcdhits.for
	touch $(d0root)/cdc_util/source/zcdini.for
	touch $(d0root)/cdc_util/source/zdrwfl.for
	touch $(d0root)/cdc_util/source/zfdcda.for
	touch $(d0root)/cdc_util/source/zfditr.for
	touch $(d0root)/cdc_util/source/zfdsec.for
	touch $(d0root)/cdc_util/source/zfdtrk.for
	touch $(d0root)/cdc_util/source/zfdtsg.for
	touch $(d0root)/cdc_util/source/zgdcda.for
	touch $(d0root)/cdc_util/source/zgdsec.for
	touch $(d0root)/cdc_util/source/zgdtsg.for
	touch $(d0root)/cdc_util/source/zhit.for
