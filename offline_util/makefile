SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0test)/c_inc -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Olimit 2500 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/admin/tmp/d0librar/mmstomake/offline_util
FLAVOR = SIUNIX
F77 = f77
.IGNORE:
.SUFFIXES:

opt :\
  offline_util.a
offline_util.a : $(d0root)/offline_util/offline_util.a
	@ echo offline_util.a is up to date
$(d0root)/offline_util/offline_util.a:: $(SCRATCH)/offline_util/nothing.nl
$(d0root)/offline_util/offline_util.a::\
  $(d0root)/offline_util/offline_util.a(add_alarm.o)\
  $(d0root)/offline_util/offline_util.a(add_chr.o)\
  $(d0root)/offline_util/offline_util.a(addstr.o)\
  $(d0root)/offline_util/offline_util.a(allwords.o)\
  $(d0root)/offline_util/offline_util.a(angnor.o)\
  $(d0root)/offline_util/offline_util.a(assign_name.o)\
  $(d0root)/offline_util/offline_util.a(binsea.o)\
  $(d0root)/offline_util/offline_util.a(cal_th.o)\
  $(d0root)/offline_util/offline_util.a(cartes.o)\
  $(d0root)/offline_util/offline_util.a(cdtime.o)\
  $(d0root)/offline_util/offline_util.a(chop.o)\
  $(d0root)/offline_util/offline_util.a(clofil.o)\
  $(d0root)/offline_util/offline_util.a(connect_init.o)\
  $(d0root)/offline_util/offline_util.a(cputim.o)\
  $(d0root)/offline_util/offline_util.a(cvsectime685.o)\
  $(d0root)/offline_util/offline_util.a(cylind.o)\
  $(d0root)/offline_util/offline_util.a(d0_abort.o)\
  $(d0root)/offline_util/offline_util.a(d0checksum.o)\
  $(d0root)/offline_util/offline_util.a(d0csbuf.o)\
  $(d0root)/offline_util/offline_util.a(d0csopen.o)\
  $(d0root)/offline_util/offline_util.a(d0csohook334.o)\
  $(d0root)/offline_util/offline_util.a(d0hcmp_add.o)\
  $(d0root)/offline_util/offline_util.a(d0hcmdirs848.o)\
  $(d0root)/offline_util/offline_util.a(d0hcmscan417.o)\
  $(d0root)/offline_util/offline_util.a(d0hcmhpar519.o)\
  $(d0root)/offline_util/offline_util.a(d0hcmdirs856.o)\
  $(d0root)/offline_util/offline_util.a(d0hcm_rcp432.o)\
  $(d0root)/offline_util/offline_util.a(d0hcmp_veri.o)\
  $(d0root)/offline_util/offline_util.a(d0open.o)\
  $(d0root)/offline_util/offline_util.a(d0rzopen.o)\
  $(d0root)/offline_util/offline_util.a(d0tloct.o)\
  $(d0root)/offline_util/offline_util.a(d0veri.o)\
  $(d0root)/offline_util/offline_util.a(dbl3isrvr473.o)\
  $(d0root)/offline_util/offline_util.a(dcos__phi791.o)\
  $(d0root)/offline_util/offline_util.a(dctoh.o)\
  $(d0root)/offline_util/offline_util.a(ddget.o)\
  $(d0root)/offline_util/offline_util.a(decode_paths.o)\
  $(d0root)/offline_util/offline_util.a(descr_header.o)\
  $(d0root)/offline_util/offline_util.a(det_eta.o)\
  $(d0root)/offline_util/offline_util.a(dgn.o)\
  $(d0root)/offline_util/offline_util.a(dgn_begin.o)\
  $(d0root)/offline_util/offline_util.a(dgn_gtags214.o)\
  $(d0root)/offline_util/offline_util.a(dgn_gixes703.o)\
  $(d0root)/offline_util/offline_util.a(dgn_gales468.o)\
  $(d0root)/offline_util/offline_util.a(dgn_get_tags.o)\
  $(d0root)/offline_util/offline_util.a(dhdir.o)\
  $(d0root)/offline_util/offline_util.a(dhdirectory.o)\
  $(d0root)/offline_util/offline_util.a(dhpath.o)\
  $(d0root)/offline_util/offline_util.a(dhsetdir.o)\
  $(d0root)/offline_util/offline_util.a(dhtoc.o)\
  $(d0root)/offline_util/offline_util.a(diff_phi.o)\
  $(d0root)/offline_util/offline_util.a(display_on.o)\
  $(d0root)/offline_util/offline_util.a(do_hbook.o)\
  $(d0root)/offline_util/offline_util.a(do_hbopen376.o)\
  $(d0root)/offline_util/offline_util.a(do_hbtest390.o)\
  $(d0root)/offline_util/offline_util.a(do_hbookn.o)\
  $(d0root)/offline_util/offline_util.a(do_hf1d.o)\
  $(d0root)/offline_util/offline_util.a(drcos.o)\
  $(d0root)/offline_util/offline_util.a(drop_banks.o)\
  $(d0root)/offline_util/offline_util.a(dump_quan621.o)\
  $(d0root)/offline_util/offline_util.a(dump_vent523.o)\
  $(d0root)/offline_util/offline_util.a(dump_w_event.o)\
  $(d0root)/offline_util/offline_util.a(dvmod.o)\
  $(d0root)/offline_util/offline_util.a(end_hstor.o)\
  $(d0root)/offline_util/offline_util.a(endet.o)\
  $(d0root)/offline_util/offline_util.a(enios.o)\
  $(d0root)/offline_util/offline_util.a(enlog.o)\
  $(d0root)/offline_util/offline_util.a(enlstp.o)\
  $(d0root)/offline_util/offline_util.a(enlun.o)\
  $(d0root)/offline_util/offline_util.a(enprog.o)\
  $(d0root)/offline_util/offline_util.a(enstoh.o)\
  $(d0root)/offline_util/offline_util.a(enstp.o)\
  $(d0root)/offline_util/offline_util.a(enstr.o)\
  $(d0root)/offline_util/offline_util.a(enzstp.o)\
  $(d0root)/offline_util/offline_util.a(errmsg_setup.o)\
  $(d0root)/offline_util/offline_util.a(errout.o)\
  $(d0root)/offline_util/offline_util.a(eta_zcorr.o)\
  $(d0root)/offline_util/offline_util.a(etad.o)\
  $(d0root)/offline_util/offline_util.a(etoeta.o)\
  $(d0root)/offline_util/offline_util.a(eventtrol420.o)\
  $(d0root)/offline_util/offline_util.a(evtoft.o)\
  $(d0root)/offline_util/offline_util.a(extcyl.o)\
  $(d0root)/offline_util/offline_util.a(file_name.o)\
  $(d0root)/offline_util/offline_util.a(filenarse477.o)\
  $(d0root)/offline_util/offline_util.a(filesize.o)\
  $(d0root)/offline_util/offline_util.a(find_token.o)\
  $(d0root)/offline_util/offline_util.a(gearot.o)\
  $(d0root)/offline_util/offline_util.a(get_aruns612.o)\
  $(d0root)/offline_util/offline_util.a(get_etype516.o)\
  $(d0root)/offline_util/offline_util.a(get_limits.o)\
  $(d0root)/offline_util/offline_util.a(get_noken512.o)\
  $(d0root)/offline_util/offline_util.a(get_pames819.o)\
  $(d0root)/offline_util/offline_util.a(get_rters622.o)\
  $(d0root)/offline_util/offline_util.a(get_reams628.o)\
  $(d0root)/offline_util/offline_util.a(getdbg.o)\
  $(d0root)/offline_util/offline_util.a(getofs.o)\
  $(d0root)/offline_util/offline_util.a(getword.o)\
  $(d0root)/offline_util/offline_util.a(gtsite.o)\
  $(d0root)/offline_util/offline_util.a(gtunit.o)\
  $(d0root)/offline_util/offline_util.a(gzhdir.o)\
  $(d0root)/offline_util/offline_util.a(gzhidn.o)\
  $(d0root)/offline_util/offline_util.a(hedit.o)\
  $(d0root)/offline_util/offline_util.a(hfast.o)\
  $(d0root)/offline_util/offline_util.a(hfff1.o)\
  $(d0root)/offline_util/offline_util.a(hinlib.o)\
  $(d0root)/offline_util/offline_util.a(histit.o)\
  $(d0root)/offline_util/offline_util.a(hlpusr.o)\
  $(d0root)/offline_util/offline_util.a(hmatrix.o)\
  $(d0root)/offline_util/offline_util.a(hmatrix_anal.o)\
  $(d0root)/offline_util/offline_util.a(hmatrnal1331.o)\
  $(d0root)/offline_util/offline_util.a(hmatrrray413.o)\
  $(d0root)/offline_util/offline_util.a(hmatranks919.o)\
  $(d0root)/offline_util/offline_util.a(hmatrared935.o)\
  $(d0root)/offline_util/offline_util.a(hmatrrray513.o)\
  $(d0root)/offline_util/offline_util.a(hmatrtran807.o)\
  $(d0root)/offline_util/offline_util.a(hmatrages022.o)\
  $(d0root)/offline_util/offline_util.a(hmatrix_drop.o)\
  $(d0root)/offline_util/offline_util.a(hmatranks930.o)\
  $(d0root)/offline_util/offline_util.a(hmatrvect404.o)\
  $(d0root)/offline_util/offline_util.a(hmatrvent416.o)\
  $(d0root)/offline_util/offline_util.a(hmatrix_fill.o)\
  $(d0root)/offline_util/offline_util.a(hmatrix_fin.o)\
  $(d0root)/offline_util/offline_util.a(hmatrix_find.o)\
  $(d0root)/offline_util/offline_util.a(hmatrix_get.o)\
  $(d0root)/offline_util/offline_util.a(hmatrix_ini.o)\
  $(d0root)/offline_util/offline_util.a(hmatrarea313.o)\
  $(d0root)/offline_util/offline_util.a(hmatruple043.o)\
  $(d0root)/offline_util/offline_util.a(hmatrix_name.o)\
  $(d0root)/offline_util/offline_util.a(hmatrdict617.o)\
  $(d0root)/offline_util/offline_util.a(hmatrlose735.o)\
  $(d0root)/offline_util/offline_util.a(hmatrtrix277.o)\
  $(d0root)/offline_util/offline_util.a(hmatrlist645.o)\
  $(d0root)/offline_util/offline_util.a(hmatrmake615.o)\
  $(d0root)/offline_util/offline_util.a(hmatrsave632.o)\
  $(d0root)/offline_util/offline_util.a(hmatrix_set.o)\
  $(d0root)/offline_util/offline_util.a(hmatrinks842.o)\
  $(d0root)/offline_util/offline_util.a(hmatrizes855.o)\
  $(d0root)/offline_util/offline_util.a(hmatrix_vect.o)\
  $(d0root)/offline_util/offline_util.a(hmindex.o)\
  $(d0root)/offline_util/offline_util.a(hsearc.o)\
  $(d0root)/offline_util/offline_util.a(ifind2.o)\
  $(d0root)/offline_util/offline_util.a(in_iendow498.o)\
  $(d0root)/offline_util/offline_util.a(in_ipndow505.o)\
  $(d0root)/offline_util/offline_util.a(inftmn.o)\
  $(d0root)/offline_util/offline_util.a(inihbk.o)\
  $(d0root)/offline_util/offline_util.a(intmsg.o)\
  $(d0root)/offline_util/offline_util.a(intstr.o)\
  $(d0root)/offline_util/offline_util.a(inzbra.o)\
  $(d0root)/offline_util/offline_util.a(ioserr.o)\
  $(d0root)/offline_util/offline_util.a(isort2.o)\
  $(d0root)/offline_util/offline_util.a(isorter.o)\
  $(d0root)/offline_util/offline_util.a(itest_bank.o)\
  $(d0root)/offline_util/offline_util.a(keep_banks.o)\
  $(d0root)/offline_util/offline_util.a(l1_passed.o)\
  $(d0root)/offline_util/offline_util.a(l1bit_passed.o)\
  $(d0root)/offline_util/offline_util.a(l2_passed.o)\
  $(d0root)/offline_util/offline_util.a(l2_vert.o)\
  $(d0root)/offline_util/offline_util.a(l2bit_on.o)\
  $(d0root)/offline_util/offline_util.a(l2bit_passed.o)\
  $(d0root)/offline_util/offline_util.a(l2bit_set.o)\
  $(d0root)/offline_util/offline_util.a(l2bit_tried.o)\
  $(d0root)/offline_util/offline_util.a(l2bitased425.o)\
  $(d0root)/offline_util/offline_util.a(lchain.o)\
  $(d0root)/offline_util/offline_util.a(lcpath.o)\
  $(d0root)/offline_util/offline_util.a(lctree.o)\
  $(d0root)/offline_util/offline_util.a(locnum.o)\
  $(d0root)/offline_util/offline_util.a(locstr.o)\
  $(d0root)/offline_util/offline_util.a(locstr1.o)\
  $(d0root)/offline_util/offline_util.a(loctd0t.o)\
  $(d0root)/offline_util/offline_util.a(lsq_anal.o)\
  $(d0root)/offline_util/offline_util.a(lsq_bkmatrix.o)\
  $(d0root)/offline_util/offline_util.a(lsq_cbage991.o)\
  $(d0root)/offline_util/offline_util.a(lsq_dtran378.o)\
  $(d0root)/offline_util/offline_util.a(lsq_event.o)\
  $(d0root)/offline_util/offline_util.a(lsq_embda691.o)\
  $(d0root)/offline_util/offline_util.a(lsq_ebda1740.o)\
  $(d0root)/offline_util/offline_util.a(lsq_ebda2741.o)\
  $(d0root)/offline_util/offline_util.a(lsq_ebda3742.o)\
  $(d0root)/offline_util/offline_util.a(lsq_fin.o)\
  $(d0root)/offline_util/offline_util.a(lsq_gumns179.o)\
  $(d0root)/offline_util/offline_util.a(lsq_get_val.o)\
  $(d0root)/offline_util/offline_util.a(lsq_ini.o)\
  $(d0root)/offline_util/offline_util.a(lsq_marea884.o)\
  $(d0root)/offline_util/offline_util.a(lsq_m_add494.o)\
  $(d0root)/offline_util/offline_util.a(lsq_mment335.o)\
  $(d0root)/offline_util/offline_util.a(lsq_mcopy640.o)\
  $(d0root)/offline_util/offline_util.a(lsq_mlete824.o)\
  $(d0root)/offline_util/offline_util.a(lsq_mdiag602.o)\
  $(d0root)/offline_util/offline_util.a(lsq_mxist754.o)\
  $(d0root)/offline_util/offline_util.a(lsq_mfill620.o)\
  $(d0root)/offline_util/offline_util.a(lsq_m_get517.o)\
  $(d0root)/offline_util/offline_util.a(lsq_mvert861.o)\
  $(d0root)/offline_util/offline_util.a(lsq_mtest404.o)\
  $(d0root)/offline_util/offline_util.a(lsq_miply093.o)\
  $(d0root)/offline_util/offline_util.a(lsq_mrint754.o)\
  $(d0root)/offline_util/offline_util.a(lsq_mform185.o)\
  $(d0root)/offline_util/offline_util.a(lsq_mpose188.o)\
  $(d0root)/offline_util/offline_util.a(lsq_poly_fit.o)\
  $(d0root)/offline_util/offline_util.a(lsq_pinit424.o)\
  $(d0root)/offline_util/offline_util.a(lsq_pload404.o)\
  $(d0root)/offline_util/offline_util.a(lsq_rvent494.o)\
  $(d0root)/offline_util/offline_util.a(lsq_rz_get.o)\
  $(d0root)/offline_util/offline_util.a(lsq_rz_make.o)\
  $(d0root)/offline_util/offline_util.a(lsq_rz_save.o)\
  $(d0root)/offline_util/offline_util.a(lsq_siply062.o)\
  $(d0root)/offline_util/offline_util.a(lsq_set_val.o)\
  $(d0root)/offline_util/offline_util.a(lsq_test.o)\
  $(d0root)/offline_util/offline_util.a(lsq_vduct694.o)\
  $(d0root)/offline_util/offline_util.a(lsq_vduct386.o)\
  $(d0root)/offline_util/offline_util.a(lstoft.o)\
  $(d0root)/offline_util/offline_util.a(match_wild.o)\
  $(d0root)/offline_util/offline_util.a(matchjets.o)\
  $(d0root)/offline_util/offline_util.a(matrirt_1574.o)\
  $(d0root)/offline_util/offline_util.a(matrirt_2575.o)\
  $(d0root)/offline_util/offline_util.a(matriest1022.o)\
  $(d0root)/offline_util/offline_util.a(matriest2023.o)\
  $(d0root)/offline_util/offline_util.a(mrveto.o)\
  $(d0root)/offline_util/offline_util.a(mxprnd.o)\
  $(d0root)/offline_util/offline_util.a(mxprnt.o)\
  $(d0root)/offline_util/offline_util.a(nextid.o)\
  $(d0root)/offline_util/offline_util.a(ntuple_book.o)\
  $(d0root)/offline_util/offline_util.a(ntuple_close.o)\
  $(d0root)/offline_util/offline_util.a(ntuple_end.o)\
  $(d0root)/offline_util/offline_util.a(ntuplopen714.o)\
  $(d0root)/offline_util/offline_util.a(ntupllect920.o)\
  $(d0root)/offline_util/offline_util.a(ntuple_fill.o)\
  $(d0root)/offline_util/offline_util.a(ntuple_flush.o)\
  $(d0root)/offline_util/offline_util.a(ntuple_get.o)\
  $(d0root)/offline_util/offline_util.a(ntupl_ids504.o)\
  $(d0root)/offline_util/offline_util.a(ntuplinfo612.o)\
  $(d0root)/offline_util/offline_util.a(ntuplinfo736.o)\
  $(d0root)/offline_util/offline_util.a(ntuple_out.o)\
  $(d0root)/offline_util/offline_util.a(ntuple_save.o)\
  $(d0root)/offline_util/offline_util.a(offtim.o)\
  $(d0root)/offline_util/offline_util.a(oftevt.o)\
  $(d0root)/offline_util/offline_util.a(oftstr.o)\
  $(d0root)/offline_util/offline_util.a(oftvax.o)\
  $(d0root)/offline_util/offline_util.a(opnfil.o)\
  $(d0root)/offline_util/offline_util.a(outmsg.o)\
  $(d0root)/offline_util/offline_util.a(outpuream446.o)\
  $(d0root)/offline_util/offline_util.a(outpuedit963.o)\
  $(d0root)/offline_util/offline_util.a(parse.o)\
  $(d0root)/offline_util/offline_util.a(parse_files.o)\
  $(d0root)/offline_util/offline_util.a(parsfi.o)\
  $(d0root)/offline_util/offline_util.a(paxgrm.o)\
  $(d0root)/offline_util/offline_util.a(paxrot.o)\
  $(d0root)/offline_util/offline_util.a(pbd_gflag782.o)\
  $(d0root)/offline_util/offline_util.a(pbd_slags775.o)\
  $(d0root)/offline_util/offline_util.a(pbd_tflag358.o)\
  $(d0root)/offline_util/offline_util.a(peta_to_deta.o)\
  $(d0root)/offline_util/offline_util.a(pm_info.o)\
  $(d0root)/offline_util/offline_util.a(pm_message.o)\
  $(d0root)/offline_util/offline_util.a(polang.o)\
  $(d0root)/offline_util/offline_util.a(posixsize527.o)\
  $(d0root)/offline_util/offline_util.a(produc.o)\
  $(d0root)/offline_util/offline_util.a(prunit.o)\
  $(d0root)/offline_util/offline_util.a(purge_banks.o)\
  $(d0root)/offline_util/offline_util.a(qprint.o)\
  $(d0root)/offline_util/offline_util.a(reorder.o)\
  $(d0root)/offline_util/offline_util.a(rfind2.o)\
  $(d0root)/offline_util/offline_util.a(rflrot.o)\
  $(d0root)/offline_util/offline_util.a(rotgea.o)\
  $(d0root)/offline_util/offline_util.a(rotmlt.o)\
  $(d0root)/offline_util/offline_util.a(rotpax.o)\
  $(d0root)/offline_util/offline_util.a(rpaxok.o)\
  $(d0root)/offline_util/offline_util.a(rsort2.o)\
  $(d0root)/offline_util/offline_util.a(rsorter.o)\
  $(d0root)/offline_util/offline_util.a(rtest_bank.o)\
  $(d0root)/offline_util/offline_util.a(run_type.o)\
  $(d0root)/offline_util/offline_util.a(runsum_info.o)\
  $(d0root)/offline_util/offline_util.a(scrap.o)\
  $(d0root)/offline_util/offline_util.a(show_ages385.o)\
  $(d0root)/offline_util/offline_util.a(sort_dist410.o)\
  $(d0root)/offline_util/offline_util.a(spltch.o)\
  $(d0root)/offline_util/offline_util.a(srtchr.o)\
  $(d0root)/offline_util/offline_util.a(srtflt.o)\
  $(d0root)/offline_util/offline_util.a(srtint.o)\
  $(d0root)/offline_util/offline_util.a(ssopen.o)\
  $(d0root)/offline_util/offline_util.a(ssunit.o)\
  $(d0root)/offline_util/offline_util.a(strint.o)\
  $(d0root)/offline_util/offline_util.a(strip_event.o)\
  $(d0root)/offline_util/offline_util.a(strip_ini.o)\
  $(d0root)/offline_util/offline_util.a(stroft.o)\
  $(d0root)/offline_util/offline_util.a(sub_chr.o)\
  $(d0root)/offline_util/offline_util.a(swap_token.o)\
  $(d0root)/offline_util/offline_util.a(swords.o)\
  $(d0root)/offline_util/offline_util.a(token.o)\
  $(d0root)/offline_util/offline_util.a(tranup.o)\
  $(d0root)/offline_util/offline_util.a(trnlnm.o)\
  $(d0root)/offline_util/offline_util.a(trulen.o)\
  $(d0root)/offline_util/offline_util.a(ucopydd.o)\
  $(d0root)/offline_util/offline_util.a(ucopyds.o)\
  $(d0root)/offline_util/offline_util.a(ucopysd.o)\
  $(d0root)/offline_util/offline_util.a(upcase.o)\
  $(d0root)/offline_util/offline_util.a(uquit.o)\
  $(d0root)/offline_util/offline_util.a(usunit.o)\
  $(d0root)/offline_util/offline_util.a(value.o)\
  $(d0root)/offline_util/offline_util.a(values.o)\
  $(d0root)/offline_util/offline_util.a(valuex.o)\
  $(d0root)/offline_util/offline_util.a(valusx.o)\
  $(d0root)/offline_util/offline_util.a(valusy.o)\
  $(d0root)/offline_util/offline_util.a(vaxoft.o)\
  $(d0root)/offline_util/offline_util.a(vec_print.o)\
  $(d0root)/offline_util/offline_util.a(vnumi.o)\
  $(d0root)/offline_util/offline_util.a(vnumr.o)\
  $(d0root)/offline_util/offline_util.a(word.o)\
  $(d0root)/offline_util/offline_util.a(word_d_by600.o)\
  $(d0root)/offline_util/offline_util.a(xchker.o)\
  $(d0root)/offline_util/offline_util.a(zchain.o)
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a2/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a3/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a4/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a5/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a6/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a7/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a8/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a9/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a10/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a11/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a12/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a13/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a14/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a15/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/offline_util/offline_util.a `ls $(SCRATCH)/offline_util/a16/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/offline_util
$(SCRATCH)/offline_util/nothing.nl:
	rmdirp $(SCRATCH)/offline_util
	mkdirp $(SCRATCH)/offline_util/a1
	mkdirp $(SCRATCH)/offline_util/a2
	mkdirp $(SCRATCH)/offline_util/a3
	mkdirp $(SCRATCH)/offline_util/a4
	mkdirp $(SCRATCH)/offline_util/a5
	mkdirp $(SCRATCH)/offline_util/a6
	mkdirp $(SCRATCH)/offline_util/a7
	mkdirp $(SCRATCH)/offline_util/a8
	mkdirp $(SCRATCH)/offline_util/a9
	mkdirp $(SCRATCH)/offline_util/a10
	mkdirp $(SCRATCH)/offline_util/a11
	mkdirp $(SCRATCH)/offline_util/a12
	mkdirp $(SCRATCH)/offline_util/a13
	mkdirp $(SCRATCH)/offline_util/a14
	mkdirp $(SCRATCH)/offline_util/a15
	mkdirp $(SCRATCH)/offline_util/a16
$(d0root)/offline_util/offline_util.a(add_alarm.o):\
  $(d0root)/offline_util/general/add_alarm.f\
  $(d0library)/inc/examine2_alarm.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/add_alarm.f ;\
	mv add_alarm.o $(SCRATCH)/offline_util/a1/add_alarm.o ;\
	)
$(d0root)/offline_util/offline_util.a(add_chr.o):\
  $(d0root)/offline_util/general/add_chr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/add_chr.f ;\
	mv add_chr.o $(SCRATCH)/offline_util/a1/add_chr.o ;\
	)
$(d0root)/offline_util/offline_util.a(addstr.o):\
  $(d0root)/offline_util/general/addstr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/addstr.f ;\
	mv addstr.o $(SCRATCH)/offline_util/a1/addstr.o ;\
	)
$(d0root)/offline_util/offline_util.a(allwords.o):\
  $(d0root)/offline_util/general/allwords.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/allwords.f ;\
	mv allwords.o $(SCRATCH)/offline_util/a1/allwords.o ;\
	)
$(d0root)/offline_util/offline_util.a(angnor.o):\
  $(d0root)/offline_util/rotate/angnor.f\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/rotate/angnor.f ;\
	mv angnor.o $(SCRATCH)/offline_util/a1/angnor.o ;\
	)
$(d0root)/offline_util/offline_util.a(assign_name.o):\
  $(d0root)/offline_util/general/assign_name.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/assign_name.f ;\
	mv assign_name.o $(SCRATCH)/offline_util/a1/assign_name.o ;\
	)
$(d0root)/offline_util/offline_util.a(binsea.o):\
  $(d0root)/offline_util/general/binsea.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/binsea.f ;\
	mv binsea.o $(SCRATCH)/offline_util/a1/binsea.o ;\
	)
$(d0root)/offline_util/offline_util.a(cal_th.o):\
  $(d0root)/offline_util/general/cal_th.f\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/cal_th.f ;\
	mv cal_th.o $(SCRATCH)/offline_util/a1/cal_th.o ;\
	)
$(d0root)/offline_util/offline_util.a(cartes.o):\
  $(d0root)/offline_util/rotate/cartes.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/rotate/cartes.f ;\
	mv cartes.o $(SCRATCH)/offline_util/a1/cartes.o ;\
	)
$(d0root)/offline_util/offline_util.a(cdtime.o):\
  $(d0root)/offline_util/general/cdtime.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/cdtime.f ;\
	mv cdtime.o $(SCRATCH)/offline_util/a1/cdtime.o ;\
	)
$(d0root)/offline_util/offline_util.a(chop.o):\
  $(d0root)/offline_util/general/chop.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/chop.f ;\
	mv chop.o $(SCRATCH)/offline_util/a1/chop.o ;\
	)
$(d0root)/offline_util/offline_util.a(clofil.o):\
  $(d0root)/offline_util/general/clofil.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/clofil.f ;\
	mv clofil.o $(SCRATCH)/offline_util/a1/clofil.o ;\
	)
$(d0root)/offline_util/offline_util.a(connect_init.o):\
  $(d0root)/offline_util/general/connect_init.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/connect_init.f ;\
	mv connect_init.o $(SCRATCH)/offline_util/a1/connect_init.o ;\
	)
$(d0root)/offline_util/offline_util.a(cputim.o):\
  $(d0root)/offline_util/general/cputim.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/cputim.f ;\
	mv cputim.o $(SCRATCH)/offline_util/a1/cputim.o ;\
	)
$(d0root)/offline_util/offline_util.a(cvsectime685.o):\
  $(d0root)/offline_util/general/cvsec_delta_time.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/cvsec_delta_time.f ;\
	mv cvsec_delta_time.o $(SCRATCH)/offline_util/a1/cvsectime685.o ;\
	)
$(d0root)/offline_util/offline_util.a(cylind.o):\
  $(d0root)/offline_util/rotate/cylind.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/rotate/cylind.f ;\
	mv cylind.o $(SCRATCH)/offline_util/a1/cylind.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0_abort.o):\
  $(d0root)/offline_util/general/d0_abort.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/d0_abort.f ;\
	mv d0_abort.o $(SCRATCH)/offline_util/a1/d0_abort.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0checksum.o):\
  $(d0root)/offline_util/general/d0checksum.c
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) offline_util/general/d0checksum.c ;\
	mv d0checksum.o $(SCRATCH)/offline_util/a1/d0checksum.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0csbuf.o):\
  $(d0root)/offline_util/general/d0csbuf.f\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/d0csbuf.f ;\
	mv d0csbuf.o $(SCRATCH)/offline_util/a1/d0csbuf.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0csopen.o):\
  $(d0root)/offline_util/general/d0csopen.f\
  $(d0library)/inc/czsock.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/d0csopen.f ;\
	mv d0csopen.o $(SCRATCH)/offline_util/a1/d0csopen.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0csohook334.o):\
  $(d0root)/offline_util/general/d0csopen_hook.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/d0csopen_hook.f ;\
	mv d0csopen_hook.o $(SCRATCH)/offline_util/a2/d0csohook334.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0hcmp_add.o):\
  $(d0root)/offline_util/general/d0hcmp_add.f\
  $(d0library)/inc/pawc.inc\
  $(d0library)/params/d0hcmp.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/d0hcmp_add.f ;\
	mv d0hcmp_add.o $(SCRATCH)/offline_util/a2/d0hcmp_add.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0hcmdirs848.o):\
  $(d0root)/offline_util/general/d0hcmp_cre_pawdirs.f\
  $(d0library)/params/d0hcmp.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/d0hcmp_cre_pawdirs.f ;\
	mv d0hcmp_cre_pawdirs.o $(SCRATCH)/offline_util/a2/d0hcmdirs848.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0hcmscan417.o):\
  $(d0root)/offline_util/general/d0hcmp_dirscan.f\
  $(d0library)/params/d0hcmp.def\
  $(d0library)/inc/d0hcmp_hst_rcp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/d0hcmp_dirscan.f ;\
	mv d0hcmp_dirscan.o $(SCRATCH)/offline_util/a2/d0hcmscan417.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0hcmhpar519.o):\
  $(d0root)/offline_util/general/d0hcmp_get_hpar.f\
  $(d0library)/params/d0hcmp.def\
  $(d0root)/offline_util/general/value.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/d0hcmp_get_hpar.f ;\
	mv d0hcmp_get_hpar.o $(SCRATCH)/offline_util/a2/d0hcmhpar519.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0hcmdirs856.o):\
  $(d0root)/offline_util/general/d0hcmp_get_subdirs.f\
  $(d0library)/params/d0hcmp.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/d0hcmp_get_subdirs.f ;\
	mv d0hcmp_get_subdirs.o $(SCRATCH)/offline_util/a2/d0hcmdirs856.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0hcm_rcp432.o):\
  $(d0root)/offline_util/general/d0hcmp_hst_rcp.f\
  $(d0library)/inc/pawc.inc\
  $(d0library)/params/d0hcmp.def\
  $(d0library)/inc/d0hcmp_hst_rcp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/d0hcmp_hst_rcp.f ;\
	mv d0hcmp_hst_rcp.o $(SCRATCH)/offline_util/a2/d0hcm_rcp432.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0hcmp_veri.o):\
  $(d0root)/offline_util/general/d0hcmp_veri.f\
  $(d0library)/inc/pawc.inc\
  $(d0library)/params/d0hcmp.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/d0hcmp_veri.f ;\
	mv d0hcmp_veri.o $(SCRATCH)/offline_util/a2/d0hcmp_veri.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0open.o):\
  $(d0root)/offline_util/general/d0open.f\
  $(d0library)/inc/fatcom.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/d0open.f ;\
	mv d0open.o $(SCRATCH)/offline_util/a2/d0open.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0rzopen.o):\
  $(d0root)/offline_util/general/d0rzopen.f\
  $(d0library)/inc/fatcom.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/d0rzopen.f ;\
	mv d0rzopen.o $(SCRATCH)/offline_util/a2/d0rzopen.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0tloct.o):\
  $(d0root)/offline_util/general/d0tloct.f\
  $(d0library)/params/base_date.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/d0tloct.f ;\
	mv d0tloct.o $(SCRATCH)/offline_util/a2/d0tloct.o ;\
	)
$(d0root)/offline_util/offline_util.a(d0veri.o):\
  $(d0root)/offline_util/hbook/d0veri.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/d0veri.f ;\
	mv d0veri.o $(SCRATCH)/offline_util/a2/d0veri.o ;\
	)
$(d0root)/offline_util/offline_util.a(dbl3isrvr473.o):\
  $(d0root)/offline_util/general/dbl3init_rsrvr.f\
  $(d0library)/inc/dbstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/dbl3init_rsrvr.f ;\
	mv dbl3init_rsrvr.o $(SCRATCH)/offline_util/a2/dbl3isrvr473.o ;\
	)
$(d0root)/offline_util/offline_util.a(dcos__phi791.o):\
  $(d0root)/offline_util/general/dcos_from_eta_phi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/dcos_from_eta_phi.f ;\
	mv dcos_from_eta_phi.o $(SCRATCH)/offline_util/a2/dcos__phi791.o ;\
	)
$(d0root)/offline_util/offline_util.a(dctoh.o):\
  $(d0root)/offline_util/general/dctoh.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/dctoh.f ;\
	mv dctoh.o $(SCRATCH)/offline_util/a2/dctoh.o ;\
	)
$(d0root)/offline_util/offline_util.a(ddget.o):\
  $(d0root)/offline_util/hmatrix/ddget.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/ddget.f ;\
	mv ddget.o $(SCRATCH)/offline_util/a2/ddget.o ;\
	)
$(d0root)/offline_util/offline_util.a(decode_paths.o):\
  $(d0root)/offline_util/general/decode_paths.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/decode_paths.f ;\
	mv decode_paths.o $(SCRATCH)/offline_util/a2/decode_paths.o ;\
	)
$(d0root)/offline_util/offline_util.a(descr_header.o):\
  $(d0root)/offline_util/general/descr_header.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/descr_header.f ;\
	mv descr_header.o $(SCRATCH)/offline_util/a2/descr_header.o ;\
	)
$(d0root)/offline_util/offline_util.a(det_eta.o):\
  $(d0root)/offline_util/general/det_eta.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/det_eta.f ;\
	mv det_eta.o $(SCRATCH)/offline_util/a2/det_eta.o ;\
	)
$(d0root)/offline_util/offline_util.a(dgn.o):\
  $(d0root)/offline_util/hbook/dgn.f\
  $(d0library)/inc/dgncom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/dgn.f ;\
	mv dgn.o $(SCRATCH)/offline_util/a2/dgn.o ;\
	)
$(d0root)/offline_util/offline_util.a(dgn_begin.o):\
  $(d0root)/offline_util/hbook/dgn_begin.f\
  $(d0library)/inc/dgncom.inc\
  $(d0library)/inc/dgncom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/dgn_begin.f ;\
	mv dgn_begin.o $(SCRATCH)/offline_util/a3/dgn_begin.o ;\
	)
$(d0root)/offline_util/offline_util.a(dgn_gtags214.o):\
  $(d0root)/offline_util/hbook/dgn_get_prefixed_tags.f\
  $(d0library)/inc/dgncom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/dgn_get_prefixed_tags.f ;\
	mv dgn_get_prefixed_tags.o $(SCRATCH)/offline_util/a3/dgn_gtags214.o ;\
	)
$(d0root)/offline_util/offline_util.a(dgn_gixes703.o):\
  $(d0root)/offline_util/hbook/dgn_get_prefixes.f\
  $(d0library)/inc/dgncom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/dgn_get_prefixes.f ;\
	mv dgn_get_prefixes.o $(SCRATCH)/offline_util/a3/dgn_gixes703.o ;\
	)
$(d0root)/offline_util/offline_util.a(dgn_gales468.o):\
  $(d0root)/offline_util/hbook/dgn_get_scales.f\
  $(d0library)/inc/dgncom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/dgn_get_scales.f ;\
	mv dgn_get_scales.o $(SCRATCH)/offline_util/a3/dgn_gales468.o ;\
	)
$(d0root)/offline_util/offline_util.a(dgn_get_tags.o):\
  $(d0root)/offline_util/hbook/dgn_get_tags.f\
  $(d0library)/inc/dgncom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/dgn_get_tags.f ;\
	mv dgn_get_tags.o $(SCRATCH)/offline_util/a3/dgn_get_tags.o ;\
	)
$(d0root)/offline_util/offline_util.a(dhdir.o):\
  $(d0root)/offline_util/hbook/dhdir.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/dhdir.f ;\
	mv dhdir.o $(SCRATCH)/offline_util/a3/dhdir.o ;\
	)
$(d0root)/offline_util/offline_util.a(dhdirectory.o):\
  $(d0root)/offline_util/hbook/dhdirectory.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/dhdirectory.f ;\
	mv dhdirectory.o $(SCRATCH)/offline_util/a3/dhdirectory.o ;\
	)
$(d0root)/offline_util/offline_util.a(dhpath.o):\
  $(d0root)/offline_util/hbook/dhpath.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/dhpath.f ;\
	mv dhpath.o $(SCRATCH)/offline_util/a3/dhpath.o ;\
	)
$(d0root)/offline_util/offline_util.a(dhsetdir.o):\
  $(d0root)/offline_util/hbook/dhsetdir.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/dhsetdir.f ;\
	mv dhsetdir.o $(SCRATCH)/offline_util/a3/dhsetdir.o ;\
	)
$(d0root)/offline_util/offline_util.a(dhtoc.o):\
  $(d0root)/offline_util/general/dhtoc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/dhtoc.f ;\
	mv dhtoc.o $(SCRATCH)/offline_util/a3/dhtoc.o ;\
	)
$(d0root)/offline_util/offline_util.a(diff_phi.o):\
  $(d0root)/offline_util/general/diff_phi.f\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/diff_phi.f ;\
	mv diff_phi.o $(SCRATCH)/offline_util/a3/diff_phi.o ;\
	)
$(d0root)/offline_util/offline_util.a(display_on.o):\
  $(d0root)/offline_util/general/display_on.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/display_on.f ;\
	mv display_on.o $(SCRATCH)/offline_util/a3/display_on.o ;\
	)
$(d0root)/offline_util/offline_util.a(do_hbook.o):\
  $(d0root)/offline_util/hbook/do_hbook.f\
  $(d0root)/offline_util/general/value.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/do_hbook.f ;\
	mv do_hbook.o $(SCRATCH)/offline_util/a3/do_hbook.o ;\
	)
$(d0root)/offline_util/offline_util.a(do_hbopen376.o):\
  $(d0root)/offline_util/hbook/do_hbook_open.f\
  $(d0root)/offline_util/general/value.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/do_hbook_open.f ;\
	mv do_hbook_open.o $(SCRATCH)/offline_util/a3/do_hbopen376.o ;\
	)
$(d0root)/offline_util/offline_util.a(do_hbtest390.o):\
  $(d0root)/offline_util/hbook/do_hbook_test.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/do_hbook_test.f ;\
	mv do_hbook_test.o $(SCRATCH)/offline_util/a3/do_hbtest390.o ;\
	)
$(d0root)/offline_util/offline_util.a(do_hbookn.o):\
  $(d0root)/offline_util/hbook/do_hbookn.f\
  $(d0root)/offline_util/general/value.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/do_hbookn.f ;\
	mv do_hbookn.o $(SCRATCH)/offline_util/a3/do_hbookn.o ;\
	)
$(d0root)/offline_util/offline_util.a(do_hf1d.o):\
  $(d0root)/offline_util/hbook/do_hf1d.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/do_hf1d.f ;\
	mv do_hf1d.o $(SCRATCH)/offline_util/a3/do_hf1d.o ;\
	)
$(d0root)/offline_util/offline_util.a(drcos.o):\
  $(d0root)/offline_util/general/drcos.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/drcos.f ;\
	mv drcos.o $(SCRATCH)/offline_util/a3/drcos.o ;\
	)
$(d0root)/offline_util/offline_util.a(drop_banks.o):\
  $(d0root)/offline_util/general/drop_banks.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/drop_banks.f ;\
	mv drop_banks.o $(SCRATCH)/offline_util/a3/drop_banks.o ;\
	)
$(d0root)/offline_util/offline_util.a(dump_quan621.o):\
  $(d0root)/offline_util/hmatrix/dump_event_quan.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/event_quan.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/dump_event_quan.f ;\
	mv dump_event_quan.o $(SCRATCH)/offline_util/a3/dump_quan621.o ;\
	)
$(d0root)/offline_util/offline_util.a(dump_vent523.o):\
  $(d0root)/offline_util/hmatrix/dump_top_event.f\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/event_quan.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/dump_top_event.f ;\
	mv dump_top_event.o $(SCRATCH)/offline_util/a4/dump_vent523.o ;\
	)
$(d0root)/offline_util/offline_util.a(dump_w_event.o):\
  $(d0root)/offline_util/hmatrix/dump_w_event.f\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/event_quan.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/dump_w_event.f ;\
	mv dump_w_event.o $(SCRATCH)/offline_util/a4/dump_w_event.o ;\
	)
$(d0root)/offline_util/offline_util.a(dvmod.o):\
  $(d0root)/offline_util/general/dvmod.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/dvmod.f ;\
	mv dvmod.o $(SCRATCH)/offline_util/a4/dvmod.o ;\
	)
$(d0root)/offline_util/offline_util.a(end_hstor.o):\
  $(d0root)/offline_util/general/end_hstor.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/end_hstor.f ;\
	mv end_hstor.o $(SCRATCH)/offline_util/a4/end_hstor.o ;\
	)
$(d0root)/offline_util/offline_util.a(endet.o):\
  $(d0root)/offline_util/endtsk/endet.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/endtsk/endet.f ;\
	mv endet.o $(SCRATCH)/offline_util/a4/endet.o ;\
	)
$(d0root)/offline_util/offline_util.a(enios.o):\
  $(d0root)/offline_util/endtsk/enios.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/endtsk/enios.f ;\
	mv enios.o $(SCRATCH)/offline_util/a4/enios.o ;\
	)
$(d0root)/offline_util/offline_util.a(enlog.o):\
  $(d0root)/offline_util/endtsk/enlog.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/endtsk/enlog.f ;\
	mv enlog.o $(SCRATCH)/offline_util/a4/enlog.o ;\
	)
$(d0root)/offline_util/offline_util.a(enlstp.o):\
  $(d0root)/offline_util/endtsk/enlstp.f\
  $(d0library)/links/izslv0.link\
  $(d0library)/links/izsmuo.link\
  $(d0library)/links/izscal.link\
  $(d0library)/links/izsvtx.link\
  $(d0library)/links/izscdc.link\
  $(d0library)/links/izsfdc.link\
  $(d0library)/links/izstrd.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/endtsk/enlstp.f ;\
	mv enlstp.o $(SCRATCH)/offline_util/a4/enlstp.o ;\
	)
$(d0root)/offline_util/offline_util.a(enlun.o):\
  $(d0root)/offline_util/endtsk/enlun.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/endtsk/enlun.f ;\
	mv enlun.o $(SCRATCH)/offline_util/a4/enlun.o ;\
	)
$(d0root)/offline_util/offline_util.a(enprog.o):\
  $(d0root)/offline_util/endtsk/enprog.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/endtsk/enprog.f ;\
	mv enprog.o $(SCRATCH)/offline_util/a4/enprog.o ;\
	)
$(d0root)/offline_util/offline_util.a(enstoh.o):\
  $(d0root)/offline_util/endtsk/enstoh.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/endtsk/enstoh.f ;\
	mv enstoh.o $(SCRATCH)/offline_util/a4/enstoh.o ;\
	)
$(d0root)/offline_util/offline_util.a(enstp.o):\
  $(d0root)/offline_util/endtsk/enstp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/endtsk/enstp.f ;\
	mv enstp.o $(SCRATCH)/offline_util/a4/enstp.o ;\
	)
$(d0root)/offline_util/offline_util.a(enstr.o):\
  $(d0root)/offline_util/endtsk/enstr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/endtsk/enstr.f ;\
	mv enstr.o $(SCRATCH)/offline_util/a4/enstr.o ;\
	)
$(d0root)/offline_util/offline_util.a(enzstp.o):\
  $(d0root)/offline_util/endtsk/enzstp.f\
  $(d0library)/links/izstpo.link\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izstpn.link\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/endtsk/enzstp.f ;\
	mv enzstp.o $(SCRATCH)/offline_util/a4/enzstp.o ;\
	)
$(d0root)/offline_util/offline_util.a(errmsg_setup.o):\
  $(d0root)/offline_util/general/errmsg_setup.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/errmsg_setup.f ;\
	mv errmsg_setup.o $(SCRATCH)/offline_util/a4/errmsg_setup.o ;\
	)
$(d0root)/offline_util/offline_util.a(errout.o):\
  $(d0root)/offline_util/general/errout.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/errout.f ;\
	mv errout.o $(SCRATCH)/offline_util/a4/errout.o ;\
	)
$(d0root)/offline_util/offline_util.a(eta_zcorr.o):\
  $(d0root)/offline_util/general/eta_zcorr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/eta_zcorr.f ;\
	mv eta_zcorr.o $(SCRATCH)/offline_util/a4/eta_zcorr.o ;\
	)
$(d0root)/offline_util/offline_util.a(etad.o):\
  $(d0root)/offline_util/general/etad.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/etad.f ;\
	mv etad.o $(SCRATCH)/offline_util/a4/etad.o ;\
	)
$(d0root)/offline_util/offline_util.a(etoeta.o):\
  $(d0root)/offline_util/general/etoeta.f\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/etoeta.f ;\
	mv etoeta.o $(SCRATCH)/offline_util/a4/etoeta.o ;\
	)
$(d0root)/offline_util/offline_util.a(eventtrol420.o):\
  $(d0root)/offline_util/general/event_control.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/event_control.f ;\
	mv event_control.o $(SCRATCH)/offline_util/a4/eventtrol420.o ;\
	)
$(d0root)/offline_util/offline_util.a(evtoft.o):\
  $(d0root)/offline_util/general/evtoft.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/evtoft.f ;\
	mv evtoft.o $(SCRATCH)/offline_util/a5/evtoft.o ;\
	)
$(d0root)/offline_util/offline_util.a(extcyl.o):\
  $(d0root)/offline_util/general/extcyl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/extcyl.f ;\
	mv extcyl.o $(SCRATCH)/offline_util/a5/extcyl.o ;\
	)
$(d0root)/offline_util/offline_util.a(file_name.o):\
  $(d0root)/offline_util/general/file_name.f\
  $(d0library)/inc/fatcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/file_name.f ;\
	mv file_name.o $(SCRATCH)/offline_util/a5/file_name.o ;\
	)
$(d0root)/offline_util/offline_util.a(filenarse477.o):\
  $(d0root)/offline_util/general/filename_parse.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/filename_parse.f ;\
	mv filename_parse.o $(SCRATCH)/offline_util/a5/filenarse477.o ;\
	)
$(d0root)/offline_util/offline_util.a(filesize.o):\
  $(d0root)/offline_util/general/filesize.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/filesize.f ;\
	mv filesize.o $(SCRATCH)/offline_util/a5/filesize.o ;\
	)
$(d0root)/offline_util/offline_util.a(find_token.o):\
  $(d0root)/offline_util/general/find_token.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/find_token.f ;\
	mv find_token.o $(SCRATCH)/offline_util/a5/find_token.o ;\
	)
$(d0root)/offline_util/offline_util.a(gearot.o):\
  $(d0root)/offline_util/general/gearot.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/gearot.f ;\
	mv gearot.o $(SCRATCH)/offline_util/a5/gearot.o ;\
	)
$(d0root)/offline_util/offline_util.a(get_aruns612.o):\
  $(d0root)/offline_util/general/get_active_runs.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/get_active_runs.f ;\
	mv get_active_runs.o $(SCRATCH)/offline_util/a5/get_aruns612.o ;\
	)
$(d0root)/offline_util/offline_util.a(get_etype516.o):\
  $(d0root)/offline_util/general/get_event_type.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/get_event_type.f ;\
	mv get_event_type.o $(SCRATCH)/offline_util/a5/get_etype516.o ;\
	)
$(d0root)/offline_util/offline_util.a(get_limits.o):\
  $(d0root)/offline_util/hbook/get_limits.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/get_limits.f ;\
	mv get_limits.o $(SCRATCH)/offline_util/a5/get_limits.o ;\
	)
$(d0root)/offline_util/offline_util.a(get_noken512.o):\
  $(d0root)/offline_util/general/get_next_token.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/get_next_token.f ;\
	mv get_next_token.o $(SCRATCH)/offline_util/a5/get_noken512.o ;\
	)
$(d0root)/offline_util/offline_util.a(get_pames819.o):\
  $(d0root)/offline_util/general/get_process_names.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/get_process_names.f ;\
	mv get_process_names.o $(SCRATCH)/offline_util/a5/get_pames819.o ;\
	)
$(d0root)/offline_util/offline_util.a(get_rters622.o):\
  $(d0root)/offline_util/general/get_run_filters.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/get_run_filters.f ;\
	mv get_run_filters.o $(SCRATCH)/offline_util/a5/get_rters622.o ;\
	)
$(d0root)/offline_util/offline_util.a(get_reams628.o):\
  $(d0root)/offline_util/general/get_run_streams.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/get_run_streams.f ;\
	mv get_run_streams.o $(SCRATCH)/offline_util/a5/get_reams628.o ;\
	)
$(d0root)/offline_util/offline_util.a(getdbg.o):\
  $(d0root)/offline_util/general/getdbg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/getdbg.f ;\
	mv getdbg.o $(SCRATCH)/offline_util/a5/getdbg.o ;\
	)
$(d0root)/offline_util/offline_util.a(getofs.o):\
  $(d0root)/offline_util/general/getofs.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/getofs.f ;\
	mv getofs.o $(SCRATCH)/offline_util/a5/getofs.o ;\
	)
$(d0root)/offline_util/offline_util.a(getword.o):\
  $(d0root)/offline_util/general/getword.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/getword.f ;\
	mv getword.o $(SCRATCH)/offline_util/a5/getword.o ;\
	)
$(d0root)/offline_util/offline_util.a(gtsite.o):\
  $(d0root)/offline_util/general/gtsite.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/gtsite.f ;\
	mv gtsite.o $(SCRATCH)/offline_util/a5/gtsite.o ;\
	)
$(d0root)/offline_util/offline_util.a(gtunit.o):\
  $(d0root)/offline_util/general/gtunit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/gtunit.f ;\
	mv gtunit.o $(SCRATCH)/offline_util/a5/gtunit.o ;\
	)
$(d0root)/offline_util/offline_util.a(gzhdir.o):\
  $(d0root)/offline_util/hbook/gzhdir.f\
  $(d0library)/inc/hcbook.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/gzhdir.f ;\
	mv gzhdir.o $(SCRATCH)/offline_util/a5/gzhdir.o ;\
	)
$(d0root)/offline_util/offline_util.a(gzhidn.o):\
  $(d0root)/offline_util/hbook/gzhidn.f\
  $(d0library)/inc/hcbook.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/gzhidn.f ;\
	mv gzhidn.o $(SCRATCH)/offline_util/a6/gzhidn.o ;\
	)
$(d0root)/offline_util/offline_util.a(hedit.o):\
  $(d0root)/offline_util/hbook/hedit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/hedit.f ;\
	mv hedit.o $(SCRATCH)/offline_util/a6/hedit.o ;\
	)
$(d0root)/offline_util/offline_util.a(hfast.o):\
  $(d0root)/offline_util/hbook/hfast.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/hfast.f ;\
	mv hfast.o $(SCRATCH)/offline_util/a6/hfast.o ;\
	)
$(d0root)/offline_util/offline_util.a(hfff1.o):\
  $(d0root)/offline_util/hbook/hfff1.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/hfff1.f ;\
	mv hfff1.o $(SCRATCH)/offline_util/a6/hfff1.o ;\
	)
$(d0root)/offline_util/offline_util.a(hinlib.o):\
  $(d0root)/offline_util/hbook/hinlib.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/hinlib.f ;\
	mv hinlib.o $(SCRATCH)/offline_util/a6/hinlib.o ;\
	)
$(d0root)/offline_util/offline_util.a(histit.o):\
  $(d0root)/offline_util/endtsk/histit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/endtsk/histit.f ;\
	mv histit.o $(SCRATCH)/offline_util/a6/histit.o ;\
	)
$(d0root)/offline_util/offline_util.a(hlpusr.o):\
  $(d0root)/offline_util/general/hlpusr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/hlpusr.f ;\
	mv hlpusr.o $(SCRATCH)/offline_util/a6/hlpusr.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrix.o):\
  $(d0root)/offline_util/hmatrix/hmatrix.f\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix.f ;\
	mv hmatrix.o $(SCRATCH)/offline_util/a6/hmatrix.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrix_anal.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_anal.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/event_quan.inc\
  $(d0library)/inc/zlinkc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_anal.f ;\
	mv hmatrix_anal.o $(SCRATCH)/offline_util/a6/hmatrix_anal.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrnal1331.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_anal1.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_anal1.f ;\
	mv hmatrix_anal1.o $(SCRATCH)/offline_util/a6/hmatrnal1331.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrrray413.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_array.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_array.f ;\
	mv hmatrix_array.o $(SCRATCH)/offline_util/a6/hmatrrray413.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatranks919.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_book_banks.f\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zhmatrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_book_banks.f ;\
	mv hmatrix_book_banks.o $(SCRATCH)/offline_util/a6/hmatranks919.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrared935.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_chisquared.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/hmatrix_pars.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_chisquared.f ;\
	mv hmatrix_chisquared.o $(SCRATCH)/offline_util/a6/hmatrared935.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrrray513.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_darray.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_darray.f ;\
	mv hmatrix_darray.o $(SCRATCH)/offline_util/a6/hmatrrray513.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrtran807.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_diag_tran.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/hmatrix_pars.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_diag_tran.f ;\
	mv hmatrix_diag_tran.o $(SCRATCH)/offline_util/a6/hmatrtran807.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrages022.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_do_averages.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zhmatrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_do_averages.f ;\
	mv hmatrix_do_averages.o $(SCRATCH)/offline_util/a6/hmatrages022.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrix_drop.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_drop.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_drop.f ;\
	mv hmatrix_drop.o $(SCRATCH)/offline_util/a6/hmatrix_drop.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatranks930.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_dump_banks.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_dump_banks.f ;\
	mv hmatrix_dump_banks.o $(SCRATCH)/offline_util/a6/hmatranks930.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrvect404.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_dvect.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_dvect.f ;\
	mv hmatrix_dvect.o $(SCRATCH)/offline_util/a6/hmatrvect404.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrvent416.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_event.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/use_hmatrix.inc\
  $(d0library)/links/izquan.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_event.f ;\
	mv hmatrix_event.o $(SCRATCH)/offline_util/a6/hmatrvent416.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrix_fill.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_fill.f\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_fill.f ;\
	mv hmatrix_fill.o $(SCRATCH)/offline_util/a7/hmatrix_fill.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrix_fin.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_fin.f\
  $(d0library)/inc/hmatrix_pars.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_fin.f ;\
	mv hmatrix_fin.o $(SCRATCH)/offline_util/a7/hmatrix_fin.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrix_find.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_find.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/nhmatrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_find.f ;\
	mv hmatrix_find.o $(SCRATCH)/offline_util/a7/hmatrix_find.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrix_get.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_get.f\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_get.f ;\
	mv hmatrix_get.o $(SCRATCH)/offline_util/a7/hmatrix_get.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrix_ini.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_ini.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/use_hmatrix.inc\
  $(d0library)/links/izhrcp.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_ini.f ;\
	mv hmatrix_ini.o $(SCRATCH)/offline_util/a7/hmatrix_ini.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrarea313.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_make_link_area.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_make_link_area.f ;\
	mv hmatrix_make_link_area.o $(SCRATCH)/offline_util/a7/hmatrarea313.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatruple043.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_make_ntuple.f\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/zlinkc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_make_ntuple.f ;\
	mv hmatrix_make_ntuple.o $(SCRATCH)/offline_util/a7/hmatruple043.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrix_name.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_name.f\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/nhmatrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_name.f ;\
	mv hmatrix_name.o $(SCRATCH)/offline_util/a7/hmatrix_name.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrdict617.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_predict.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/hmatrix_pars.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_predict.f ;\
	mv hmatrix_predict.o $(SCRATCH)/offline_util/a7/hmatrdict617.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrlose735.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_rz_close.f\
  $(d0library)/inc/hmatrix_pars.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_rz_close.f ;\
	mv hmatrix_rz_close.o $(SCRATCH)/offline_util/a7/hmatrlose735.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrtrix277.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_rz_get_matrix.f\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/links/izhrcp.link\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_rz_get_matrix.f ;\
	mv hmatrix_rz_get_matrix.o $(SCRATCH)/offline_util/a7/hmatrtrix277.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrlist645.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_rz_list.f\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_rz_list.f ;\
	mv hmatrix_rz_list.o $(SCRATCH)/offline_util/a7/hmatrlist645.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrmake615.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_rz_make.f\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_rz_make.f ;\
	mv hmatrix_rz_make.o $(SCRATCH)/offline_util/a7/hmatrmake615.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrsave632.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_rz_save.f\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_rz_save.f ;\
	mv hmatrix_rz_save.o $(SCRATCH)/offline_util/a7/hmatrsave632.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrix_set.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_set.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/nhmatrix.inc\
  $(d0library)/inc/ihmatrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_set.f ;\
	mv hmatrix_set.o $(SCRATCH)/offline_util/a7/hmatrix_set.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrinks842.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_set_links.f\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izaver.link\
  $(d0library)/links/izemat.link\
  $(d0library)/links/izhmat.link\
  $(d0library)/links/izhvis.link\
  $(d0library)/links/izhinv.link\
  $(d0library)/links/izeign.link\
  $(d0library)/links/izumat.link\
  $(d0library)/links/izhrcp.link\
  $(d0library)/links/izquan.link\
  $(d0library)/links/izdiag.link\
  $(d0library)/links/izwork.link\
  $(d0library)/links/izprod.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_set_links.f ;\
	mv hmatrix_set_links.o $(SCRATCH)/offline_util/a7/hmatrinks842.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrizes855.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_set_sizes.f\
  $(d0library)/inc/hmatrix_pars.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_set_sizes.f ;\
	mv hmatrix_set_sizes.o $(SCRATCH)/offline_util/a7/hmatrizes855.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmatrix_vect.o):\
  $(d0root)/offline_util/hmatrix/hmatrix_vect.f\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmatrix_vect.f ;\
	mv hmatrix_vect.o $(SCRATCH)/offline_util/a7/hmatrix_vect.o ;\
	)
$(d0root)/offline_util/offline_util.a(hmindex.o):\
  $(d0root)/offline_util/hmatrix/hmindex.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hmatrix/hmindex.f ;\
	mv hmindex.o $(SCRATCH)/offline_util/a7/hmindex.o ;\
	)
$(d0root)/offline_util/offline_util.a(hsearc.o):\
  $(d0root)/offline_util/hbook/hsearc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/hsearc.f ;\
	mv hsearc.o $(SCRATCH)/offline_util/a7/hsearc.o ;\
	)
$(d0root)/offline_util/offline_util.a(ifind2.o):\
  $(d0root)/offline_util/general/ifind2.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/ifind2.f ;\
	mv ifind2.o $(SCRATCH)/offline_util/a8/ifind2.o ;\
	)
$(d0root)/offline_util/offline_util.a(in_iendow498.o):\
  $(d0root)/offline_util/general/in_ieta_window.f\
  $(d0library)/params/cal_offline.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/in_ieta_window.f ;\
	mv in_ieta_window.o $(SCRATCH)/offline_util/a8/in_iendow498.o ;\
	)
$(d0root)/offline_util/offline_util.a(in_ipndow505.o):\
  $(d0root)/offline_util/general/in_iphi_window.f\
  $(d0library)/params/cal_offline.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/in_iphi_window.f ;\
	mv in_iphi_window.o $(SCRATCH)/offline_util/a8/in_ipndow505.o ;\
	)
$(d0root)/offline_util/offline_util.a(inftmn.o):\
  $(d0root)/offline_util/general/inftmn.f\
  $(d0library)/inc/fatcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/inftmn.f ;\
	mv inftmn.o $(SCRATCH)/offline_util/a8/inftmn.o ;\
	)
$(d0root)/offline_util/offline_util.a(inihbk.o):\
  $(d0root)/offline_util/general/inihbk.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/inihbk.f ;\
	mv inihbk.o $(SCRATCH)/offline_util/a8/inihbk.o ;\
	)
$(d0root)/offline_util/offline_util.a(intmsg.o):\
  $(d0root)/offline_util/general/intmsg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/intmsg.f ;\
	mv intmsg.o $(SCRATCH)/offline_util/a8/intmsg.o ;\
	)
$(d0root)/offline_util/offline_util.a(intstr.o):\
  $(d0root)/offline_util/general/intstr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/intstr.f ;\
	mv intstr.o $(SCRATCH)/offline_util/a8/intstr.o ;\
	)
$(d0root)/offline_util/offline_util.a(inzbra.o):\
  $(d0root)/offline_util/general/inzbra.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/inzbra.f ;\
	mv inzbra.o $(SCRATCH)/offline_util/a8/inzbra.o ;\
	)
$(d0root)/offline_util/offline_util.a(ioserr.o):\
  $(d0root)/offline_util/general/ioserr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/ioserr.f ;\
	mv ioserr.o $(SCRATCH)/offline_util/a8/ioserr.o ;\
	)
$(d0root)/offline_util/offline_util.a(isort2.o):\
  $(d0root)/offline_util/general/isort2.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/isort2.f ;\
	mv isort2.o $(SCRATCH)/offline_util/a8/isort2.o ;\
	)
$(d0root)/offline_util/offline_util.a(isorter.o):\
  $(d0root)/offline_util/general/isorter.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/isorter.f ;\
	mv isorter.o $(SCRATCH)/offline_util/a8/isorter.o ;\
	)
$(d0root)/offline_util/offline_util.a(itest_bank.o):\
  $(d0root)/offline_util/general/itest_bank.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/itest_bank.f ;\
	mv itest_bank.o $(SCRATCH)/offline_util/a8/itest_bank.o ;\
	)
$(d0root)/offline_util/offline_util.a(keep_banks.o):\
  $(d0root)/offline_util/general/keep_banks.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/keep_banks.f ;\
	mv keep_banks.o $(SCRATCH)/offline_util/a8/keep_banks.o ;\
	)
$(d0root)/offline_util/offline_util.a(l1_passed.o):\
  $(d0root)/offline_util/general/l1_passed.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/l1_passed.f ;\
	mv l1_passed.o $(SCRATCH)/offline_util/a8/l1_passed.o ;\
	)
$(d0root)/offline_util/offline_util.a(l1bit_passed.o):\
  $(d0root)/offline_util/general/l1bit_passed.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/l1bit_passed.f ;\
	mv l1bit_passed.o $(SCRATCH)/offline_util/a8/l1bit_passed.o ;\
	)
$(d0root)/offline_util/offline_util.a(l2_passed.o):\
  $(d0root)/offline_util/general/l2_passed.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/l2_passed.f ;\
	mv l2_passed.o $(SCRATCH)/offline_util/a8/l2_passed.o ;\
	)
$(d0root)/offline_util/offline_util.a(l2_vert.o):\
  $(d0root)/offline_util/general/l2_vert.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/l2_vert.f ;\
	mv l2_vert.o $(SCRATCH)/offline_util/a8/l2_vert.o ;\
	)
$(d0root)/offline_util/offline_util.a(l2bit_on.o):\
  $(d0root)/offline_util/general/l2bit_on.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/l2bit_on.f ;\
	mv l2bit_on.o $(SCRATCH)/offline_util/a8/l2bit_on.o ;\
	)
$(d0root)/offline_util/offline_util.a(l2bit_passed.o):\
  $(d0root)/offline_util/general/l2bit_passed.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izfilt.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/l2bit_passed.f ;\
	mv l2bit_passed.o $(SCRATCH)/offline_util/a8/l2bit_passed.o ;\
	)
$(d0root)/offline_util/offline_util.a(l2bit_set.o):\
  $(d0root)/offline_util/general/l2bit_set.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izfilt.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/l2bit_set.f ;\
	mv l2bit_set.o $(SCRATCH)/offline_util/a8/l2bit_set.o ;\
	)
$(d0root)/offline_util/offline_util.a(l2bit_tried.o):\
  $(d0root)/offline_util/general/l2bit_tried.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izfilt.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/l2bit_tried.f ;\
	mv l2bit_tried.o $(SCRATCH)/offline_util/a9/l2bit_tried.o ;\
	)
$(d0root)/offline_util/offline_util.a(l2bitased425.o):\
  $(d0root)/offline_util/general/l2bit_unbiased.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izfilt.link\
  $(d0library)/params/monitor_bit.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/l2bit_unbiased.f ;\
	mv l2bit_unbiased.o $(SCRATCH)/offline_util/a9/l2bitased425.o ;\
	)
$(d0root)/offline_util/offline_util.a(lchain.o):\
  $(d0root)/offline_util/general/lchain.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/lchain.f ;\
	mv lchain.o $(SCRATCH)/offline_util/a9/lchain.o ;\
	)
$(d0root)/offline_util/offline_util.a(lcpath.o):\
  $(d0root)/offline_util/general/lcpath.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/lcpath.f ;\
	mv lcpath.o $(SCRATCH)/offline_util/a9/lcpath.o ;\
	)
$(d0root)/offline_util/offline_util.a(lctree.o):\
  $(d0root)/offline_util/general/lctree.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/lctree.f ;\
	mv lctree.o $(SCRATCH)/offline_util/a9/lctree.o ;\
	)
$(d0root)/offline_util/offline_util.a(locnum.o):\
  $(d0root)/offline_util/general/locnum.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/locnum.f ;\
	mv locnum.o $(SCRATCH)/offline_util/a9/locnum.o ;\
	)
$(d0root)/offline_util/offline_util.a(locstr.o):\
  $(d0root)/offline_util/general/locstr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/locstr.f ;\
	mv locstr.o $(SCRATCH)/offline_util/a9/locstr.o ;\
	)
$(d0root)/offline_util/offline_util.a(locstr1.o):\
  $(d0root)/offline_util/general/locstr1.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/locstr1.f ;\
	mv locstr1.o $(SCRATCH)/offline_util/a9/locstr1.o ;\
	)
$(d0root)/offline_util/offline_util.a(loctd0t.o):\
  $(d0root)/offline_util/general/loctd0t.f\
  $(d0library)/params/base_date.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/loctd0t.f ;\
	mv loctd0t.o $(SCRATCH)/offline_util/a9/loctd0t.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_anal.o):\
  $(d0root)/offline_util/least_sq/lsq_anal.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_anal.f ;\
	mv lsq_anal.o $(SCRATCH)/offline_util/a9/lsq_anal.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_bkmatrix.o):\
  $(d0root)/offline_util/least_sq/lsq_bkmatrix.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_bkmatrix.f ;\
	mv lsq_bkmatrix.o $(SCRATCH)/offline_util/a9/lsq_bkmatrix.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_cbage991.o):\
  $(d0root)/offline_util/least_sq/lsq_collect_garbage.f\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_collect_garbage.f ;\
	mv lsq_collect_garbage.o $(SCRATCH)/offline_util/a9/lsq_cbage991.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_dtran378.o):\
  $(d0root)/offline_util/least_sq/lsq_diag_tran.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_diag_tran.f ;\
	mv lsq_diag_tran.o $(SCRATCH)/offline_util/a9/lsq_dtran378.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_event.o):\
  $(d0root)/offline_util/least_sq/lsq_event.f\
  $(d0library)/inc/dump.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_event.f ;\
	mv lsq_event.o $(SCRATCH)/offline_util/a9/lsq_event.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_embda691.o):\
  $(d0root)/offline_util/least_sq/lsq_event_lambda.f\
  $(d0library)/inc/lsq_event.inc\
  $(d0library)/inc/dump.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_event_lambda.f ;\
	mv lsq_event_lambda.o $(SCRATCH)/offline_util/a9/lsq_embda691.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_ebda1740.o):\
  $(d0root)/offline_util/least_sq/lsq_event_lambda1.f\
  $(d0library)/inc/dump.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_event_lambda1.f ;\
	mv lsq_event_lambda1.o $(SCRATCH)/offline_util/a9/lsq_ebda1740.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_ebda2741.o):\
  $(d0root)/offline_util/least_sq/lsq_event_lambda2.f\
  $(d0library)/inc/dump.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_event_lambda2.f ;\
	mv lsq_event_lambda2.o $(SCRATCH)/offline_util/a9/lsq_ebda2741.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_ebda3742.o):\
  $(d0root)/offline_util/least_sq/lsq_event_lambda3.f\
  $(d0library)/inc/dump.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_event_lambda3.f ;\
	mv lsq_event_lambda3.o $(SCRATCH)/offline_util/a9/lsq_ebda3742.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_fin.o):\
  $(d0root)/offline_util/least_sq/lsq_fin.f\
  $(d0library)/inc/lsq_pars.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_fin.f ;\
	mv lsq_fin.o $(SCRATCH)/offline_util/a9/lsq_fin.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_gumns179.o):\
  $(d0root)/offline_util/least_sq/lsq_get_rows_columns.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_get_rows_columns.f ;\
	mv lsq_get_rows_columns.o $(SCRATCH)/offline_util/a9/lsq_gumns179.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_get_val.o):\
  $(d0root)/offline_util/least_sq/lsq_get_val.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_get_val.f ;\
	mv lsq_get_val.o $(SCRATCH)/offline_util/a10/lsq_get_val.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_ini.o):\
  $(d0root)/offline_util/least_sq/lsq_ini.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_pars.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_ini.f ;\
	mv lsq_ini.o $(SCRATCH)/offline_util/a10/lsq_ini.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_marea884.o):\
  $(d0root)/offline_util/least_sq/lsq_make_link_area.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_make_link_area.f ;\
	mv lsq_make_link_area.o $(SCRATCH)/offline_util/a10/lsq_marea884.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_m_add494.o):\
  $(d0root)/offline_util/least_sq/lsq_matrix_add.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_matrix_add.f ;\
	mv lsq_matrix_add.o $(SCRATCH)/offline_util/a10/lsq_m_add494.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_mment335.o):\
  $(d0root)/offline_util/least_sq/lsq_matrix_add_element.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_matrix_add_element.f ;\
	mv lsq_matrix_add_element.o $(SCRATCH)/offline_util/a10/lsq_mment335.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_mcopy640.o):\
  $(d0root)/offline_util/least_sq/lsq_matrix_copy.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_matrix_copy.f ;\
	mv lsq_matrix_copy.o $(SCRATCH)/offline_util/a10/lsq_mcopy640.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_mlete824.o):\
  $(d0root)/offline_util/least_sq/lsq_matrix_delete.f\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_matrix_delete.f ;\
	mv lsq_matrix_delete.o $(SCRATCH)/offline_util/a10/lsq_mlete824.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_mdiag602.o):\
  $(d0root)/offline_util/least_sq/lsq_matrix_diag.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_matrix_diag.f ;\
	mv lsq_matrix_diag.o $(SCRATCH)/offline_util/a10/lsq_mdiag602.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_mxist754.o):\
  $(d0root)/offline_util/least_sq/lsq_matrix_exist.f\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_matrix_exist.f ;\
	mv lsq_matrix_exist.o $(SCRATCH)/offline_util/a10/lsq_mxist754.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_mfill620.o):\
  $(d0root)/offline_util/least_sq/lsq_matrix_fill.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_matrix_fill.f ;\
	mv lsq_matrix_fill.o $(SCRATCH)/offline_util/a10/lsq_mfill620.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_m_get517.o):\
  $(d0root)/offline_util/least_sq/lsq_matrix_get.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_matrix_get.f ;\
	mv lsq_matrix_get.o $(SCRATCH)/offline_util/a10/lsq_m_get517.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_mvert861.o):\
  $(d0root)/offline_util/least_sq/lsq_matrix_invert.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_matrix_invert.f ;\
	mv lsq_matrix_invert.o $(SCRATCH)/offline_util/a10/lsq_mvert861.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_mtest404.o):\
  $(d0root)/offline_util/least_sq/lsq_matrix_invert_test.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_matrix_invert_test.f ;\
	mv lsq_matrix_invert_test.o $(SCRATCH)/offline_util/a10/lsq_mtest404.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_miply093.o):\
  $(d0root)/offline_util/least_sq/lsq_matrix_multiply.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_matrix_multiply.f ;\
	mv lsq_matrix_multiply.o $(SCRATCH)/offline_util/a10/lsq_miply093.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_mrint754.o):\
  $(d0root)/offline_util/least_sq/lsq_matrix_print.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_matrix_print.f ;\
	mv lsq_matrix_print.o $(SCRATCH)/offline_util/a10/lsq_mrint754.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_mform185.o):\
  $(d0root)/offline_util/least_sq/lsq_matrix_transform.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_matrix_transform.f ;\
	mv lsq_matrix_transform.o $(SCRATCH)/offline_util/a10/lsq_mform185.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_mpose188.o):\
  $(d0root)/offline_util/least_sq/lsq_matrix_transpose.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_matrix_transpose.f ;\
	mv lsq_matrix_transpose.o $(SCRATCH)/offline_util/a10/lsq_mpose188.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_poly_fit.o):\
  $(d0root)/offline_util/least_sq/lsq_poly_fit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_poly_fit.f ;\
	mv lsq_poly_fit.o $(SCRATCH)/offline_util/a10/lsq_poly_fit.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_pinit424.o):\
  $(d0root)/offline_util/least_sq/lsq_poly_init.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_poly_init.f ;\
	mv lsq_poly_init.o $(SCRATCH)/offline_util/a10/lsq_pinit424.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_pload404.o):\
  $(d0root)/offline_util/least_sq/lsq_poly_load.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_poly_load.f ;\
	mv lsq_poly_load.o $(SCRATCH)/offline_util/a10/lsq_pload404.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_rvent494.o):\
  $(d0root)/offline_util/least_sq/lsq_read_event.f\
  $(d0library)/inc/lsq_event.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_read_event.f ;\
	mv lsq_read_event.o $(SCRATCH)/offline_util/a11/lsq_rvent494.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_rz_get.o):\
  $(d0root)/offline_util/least_sq/lsq_rz_get.f\
  $(d0library)/inc/lsq_pars.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_rz_get.f ;\
	mv lsq_rz_get.o $(SCRATCH)/offline_util/a11/lsq_rz_get.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_rz_make.o):\
  $(d0root)/offline_util/least_sq/lsq_rz_make.f\
  $(d0library)/inc/lsq_pars.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_rz_make.f ;\
	mv lsq_rz_make.o $(SCRATCH)/offline_util/a11/lsq_rz_make.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_rz_save.o):\
  $(d0root)/offline_util/least_sq/lsq_rz_save.f\
  $(d0library)/inc/lsq_pars.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_rz_save.f ;\
	mv lsq_rz_save.o $(SCRATCH)/offline_util/a11/lsq_rz_save.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_siply062.o):\
  $(d0root)/offline_util/least_sq/lsq_scalar_multiply.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_scalar_multiply.f ;\
	mv lsq_scalar_multiply.o $(SCRATCH)/offline_util/a11/lsq_siply062.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_set_val.o):\
  $(d0root)/offline_util/least_sq/lsq_set_val.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_set_val.f ;\
	mv lsq_set_val.o $(SCRATCH)/offline_util/a11/lsq_set_val.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_test.o):\
  $(d0root)/offline_util/least_sq/lsq_test.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_test.f ;\
	mv lsq_test.o $(SCRATCH)/offline_util/a11/lsq_test.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_vduct694.o):\
  $(d0root)/offline_util/least_sq/lsq_vector_direct_product.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_vector_direct_product.f ;\
	mv lsq_vector_direct_product.o $(SCRATCH)/offline_util/a11/lsq_vduct694.o ;\
	)
$(d0root)/offline_util/offline_util.a(lsq_vduct386.o):\
  $(d0root)/offline_util/least_sq/lsq_vector_dot_product.f\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/least_sq/lsq_vector_dot_product.f ;\
	mv lsq_vector_dot_product.o $(SCRATCH)/offline_util/a11/lsq_vduct386.o ;\
	)
$(d0root)/offline_util/offline_util.a(lstoft.o):\
  $(d0root)/offline_util/general/lstoft.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/lstoft.f ;\
	mv lstoft.o $(SCRATCH)/offline_util/a11/lstoft.o ;\
	)
$(d0root)/offline_util/offline_util.a(match_wild.o):\
  $(d0root)/offline_util/general/match_wild.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/match_wild.f ;\
	mv match_wild.o $(SCRATCH)/offline_util/a11/match_wild.o ;\
	)
$(d0root)/offline_util/offline_util.a(matchjets.o):\
  $(d0root)/offline_util/general/matchjets.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/matchjets.f ;\
	mv matchjets.o $(SCRATCH)/offline_util/a11/matchjets.o ;\
	)
$(d0root)/offline_util/offline_util.a(matrirt_1574.o):\
  $(d0root)/offline_util/general/matrix_invert_1.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/matrix_invert_1.f ;\
	mv matrix_invert_1.o $(SCRATCH)/offline_util/a11/matrirt_1574.o ;\
	)
$(d0root)/offline_util/offline_util.a(matrirt_2575.o):\
  $(d0root)/offline_util/general/matrix_invert_2.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/matrix_invert_2.f ;\
	mv matrix_invert_2.o $(SCRATCH)/offline_util/a11/matrirt_2575.o ;\
	)
$(d0root)/offline_util/offline_util.a(matriest1022.o):\
  $(d0root)/offline_util/general/matrix_invert_test1.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/matrix_invert_test1.f ;\
	mv matrix_invert_test1.o $(SCRATCH)/offline_util/a11/matriest1022.o ;\
	)
$(d0root)/offline_util/offline_util.a(matriest2023.o):\
  $(d0root)/offline_util/general/matrix_invert_test2.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/matrix_invert_test2.f ;\
	mv matrix_invert_test2.o $(SCRATCH)/offline_util/a11/matriest2023.o ;\
	)
$(d0root)/offline_util/offline_util.a(mrveto.o):\
  $(d0root)/offline_util/general/mrveto.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/mrveto.f ;\
	mv mrveto.o $(SCRATCH)/offline_util/a11/mrveto.o ;\
	)
$(d0root)/offline_util/offline_util.a(mxprnd.o):\
  $(d0root)/offline_util/general/mxprnd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/mxprnd.f ;\
	mv mxprnd.o $(SCRATCH)/offline_util/a11/mxprnd.o ;\
	)
$(d0root)/offline_util/offline_util.a(mxprnt.o):\
  $(d0root)/offline_util/general/mxprnt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/mxprnt.f ;\
	mv mxprnt.o $(SCRATCH)/offline_util/a11/mxprnt.o ;\
	)
$(d0root)/offline_util/offline_util.a(nextid.o):\
  $(d0root)/offline_util/endtsk/nextid.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/endtsk/nextid.f ;\
	mv nextid.o $(SCRATCH)/offline_util/a11/nextid.o ;\
	)
$(d0root)/offline_util/offline_util.a(ntuple_book.o):\
  $(d0root)/offline_util/ntuple/ntuple_book.f\
  $(d0library)/inc/nt_housekeep.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/ntuple/ntuple_book.f ;\
	mv ntuple_book.o $(SCRATCH)/offline_util/a12/ntuple_book.o ;\
	)
$(d0root)/offline_util/offline_util.a(ntuple_close.o):\
  $(d0root)/offline_util/ntuple/ntuple_close.f\
  $(d0library)/inc/nt_housekeep.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/ntuple/ntuple_close.f ;\
	mv ntuple_close.o $(SCRATCH)/offline_util/a12/ntuple_close.o ;\
	)
$(d0root)/offline_util/offline_util.a(ntuple_end.o):\
  $(d0root)/offline_util/ntuple/ntuple_end.f\
  $(d0library)/inc/nt_housekeep.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/ntuple/ntuple_end.f ;\
	mv ntuple_end.o $(SCRATCH)/offline_util/a12/ntuple_end.o ;\
	)
$(d0root)/offline_util/offline_util.a(ntuplopen714.o):\
  $(d0root)/offline_util/ntuple/ntuple_file_open.f\
  $(d0library)/inc/nt_housekeep.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/ntuple/ntuple_file_open.f ;\
	mv ntuple_file_open.o $(SCRATCH)/offline_util/a12/ntuplopen714.o ;\
	)
$(d0root)/offline_util/offline_util.a(ntupllect920.o):\
  $(d0root)/offline_util/ntuple/ntuple_file_select.f\
  $(d0library)/inc/nt_housekeep.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/ntuple/ntuple_file_select.f ;\
	mv ntuple_file_select.o $(SCRATCH)/offline_util/a12/ntupllect920.o ;\
	)
$(d0root)/offline_util/offline_util.a(ntuple_fill.o):\
  $(d0root)/offline_util/ntuple/ntuple_fill.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/ntuple/ntuple_fill.f ;\
	mv ntuple_fill.o $(SCRATCH)/offline_util/a12/ntuple_fill.o ;\
	)
$(d0root)/offline_util/offline_util.a(ntuple_flush.o):\
  $(d0root)/offline_util/ntuple/ntuple_flush.f\
  $(d0library)/inc/nt_housekeep.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/ntuple/ntuple_flush.f ;\
	mv ntuple_flush.o $(SCRATCH)/offline_util/a12/ntuple_flush.o ;\
	)
$(d0root)/offline_util/offline_util.a(ntuple_get.o):\
  $(d0root)/offline_util/ntuple/ntuple_get.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/ntuple/ntuple_get.f ;\
	mv ntuple_get.o $(SCRATCH)/offline_util/a12/ntuple_get.o ;\
	)
$(d0root)/offline_util/offline_util.a(ntupl_ids504.o):\
  $(d0root)/offline_util/hbook/ntuple_get_ids.f\
  $(d0library)/inc/pawc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/ntuple_get_ids.f ;\
	mv ntuple_get_ids.o $(SCRATCH)/offline_util/a12/ntupl_ids504.o ;\
	)
$(d0root)/offline_util/offline_util.a(ntuplinfo612.o):\
  $(d0root)/offline_util/hbook/ntuple_get_info.f\
  $(d0library)/inc/pawc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/hbook/ntuple_get_info.f ;\
	mv ntuple_get_info.o $(SCRATCH)/offline_util/a12/ntuplinfo612.o ;\
	)
$(d0root)/offline_util/offline_util.a(ntuplinfo736.o):\
  $(d0root)/offline_util/ntuple/ntuple_list_info.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/ntuple/ntuple_list_info.f ;\
	mv ntuple_list_info.o $(SCRATCH)/offline_util/a12/ntuplinfo736.o ;\
	)
$(d0root)/offline_util/offline_util.a(ntuple_out.o):\
  $(d0root)/offline_util/ntuple/ntuple_out.f\
  $(d0library)/inc/nt_housekeep.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/ntuple/ntuple_out.f ;\
	mv ntuple_out.o $(SCRATCH)/offline_util/a12/ntuple_out.o ;\
	)
$(d0root)/offline_util/offline_util.a(ntuple_save.o):\
  $(d0root)/offline_util/ntuple/ntuple_save.f\
  $(d0library)/inc/nt_housekeep.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/ntuple/ntuple_save.f ;\
	mv ntuple_save.o $(SCRATCH)/offline_util/a12/ntuple_save.o ;\
	)
$(d0root)/offline_util/offline_util.a(offtim.o):\
  $(d0root)/offline_util/general/offtim.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/offtim.f ;\
	mv offtim.o $(SCRATCH)/offline_util/a12/offtim.o ;\
	)
$(d0root)/offline_util/offline_util.a(oftevt.o):\
  $(d0root)/offline_util/general/oftevt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/oftevt.f ;\
	mv oftevt.o $(SCRATCH)/offline_util/a12/oftevt.o ;\
	)
$(d0root)/offline_util/offline_util.a(oftstr.o):\
  $(d0root)/offline_util/general/oftstr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/oftstr.f ;\
	mv oftstr.o $(SCRATCH)/offline_util/a12/oftstr.o ;\
	)
$(d0root)/offline_util/offline_util.a(oftvax.o):\
  $(d0root)/offline_util/general/oftvax.f\
  $(d0library)/params/base_date.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/oftvax.f ;\
	mv oftvax.o $(SCRATCH)/offline_util/a12/oftvax.o ;\
	)
$(d0root)/offline_util/offline_util.a(opnfil.o):\
  $(d0root)/offline_util/general/opnfil.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/opnfil.f ;\
	mv opnfil.o $(SCRATCH)/offline_util/a12/opnfil.o ;\
	)
$(d0root)/offline_util/offline_util.a(outmsg.o):\
  $(d0root)/offline_util/general/outmsg.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/outmsg.f ;\
	mv outmsg.o $(SCRATCH)/offline_util/a12/outmsg.o ;\
	)
$(d0root)/offline_util/offline_util.a(outpuream446.o):\
  $(d0root)/offline_util/general/output_stream.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/output_stream.f ;\
	mv output_stream.o $(SCRATCH)/offline_util/a12/outpuream446.o ;\
	)
$(d0root)/offline_util/offline_util.a(outpuedit963.o):\
  $(d0root)/offline_util/general/output_stream_edit.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/output_stream_edit.f ;\
	mv output_stream_edit.o $(SCRATCH)/offline_util/a13/outpuedit963.o ;\
	)
$(d0root)/offline_util/offline_util.a(parse.o):\
  $(d0root)/offline_util/general/parse.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/parse.f ;\
	mv parse.o $(SCRATCH)/offline_util/a13/parse.o ;\
	)
$(d0root)/offline_util/offline_util.a(parse_files.o):\
  $(d0root)/offline_util/general/parse_files.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/parse_files.f ;\
	mv parse_files.o $(SCRATCH)/offline_util/a13/parse_files.o ;\
	)
$(d0root)/offline_util/offline_util.a(parsfi.o):\
  $(d0root)/offline_util/general/parsfi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/parsfi.f ;\
	mv parsfi.o $(SCRATCH)/offline_util/a13/parsfi.o ;\
	)
$(d0root)/offline_util/offline_util.a(paxgrm.o):\
  $(d0root)/offline_util/rotate/paxgrm.f\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/rotate/paxgrm.f ;\
	mv paxgrm.o $(SCRATCH)/offline_util/a13/paxgrm.o ;\
	)
$(d0root)/offline_util/offline_util.a(paxrot.o):\
  $(d0root)/offline_util/rotate/paxrot.f\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/rotate/paxrot.f ;\
	mv paxrot.o $(SCRATCH)/offline_util/a13/paxrot.o ;\
	)
$(d0root)/offline_util/offline_util.a(pbd_gflag782.o):\
  $(d0root)/offline_util/general/pbd_get_next_flag.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/pbd_get_next_flag.f ;\
	mv pbd_get_next_flag.o $(SCRATCH)/offline_util/a13/pbd_gflag782.o ;\
	)
$(d0root)/offline_util/offline_util.a(pbd_slags775.o):\
  $(d0root)/offline_util/general/pbd_set_all_flags.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/pbd_set_all_flags.f ;\
	mv pbd_set_all_flags.o $(SCRATCH)/offline_util/a13/pbd_slags775.o ;\
	)
$(d0root)/offline_util/offline_util.a(pbd_tflag358.o):\
  $(d0root)/offline_util/general/pbd_temp_flag.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/pbd_temp_flag.f ;\
	mv pbd_temp_flag.o $(SCRATCH)/offline_util/a13/pbd_tflag358.o ;\
	)
$(d0root)/offline_util/offline_util.a(peta_to_deta.o):\
  $(d0root)/offline_util/general/peta_to_deta.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/peta_to_deta.f ;\
	mv peta_to_deta.o $(SCRATCH)/offline_util/a13/peta_to_deta.o ;\
	)
$(d0root)/offline_util/offline_util.a(pm_info.o):\
  $(d0root)/offline_util/general/pm_info.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/pm_info.f ;\
	mv pm_info.o $(SCRATCH)/offline_util/a13/pm_info.o ;\
	)
$(d0root)/offline_util/offline_util.a(pm_message.o):\
  $(d0root)/offline_util/general/pm_message.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/pm_message.f ;\
	mv pm_message.o $(SCRATCH)/offline_util/a13/pm_message.o ;\
	)
$(d0root)/offline_util/offline_util.a(polang.o):\
  $(d0root)/offline_util/rotate/polang.f\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/rotate/polang.f ;\
	mv polang.o $(SCRATCH)/offline_util/a13/polang.o ;\
	)
$(d0root)/offline_util/offline_util.a(posixsize527.o):\
  $(d0root)/offline_util/general/posix_filesize.c
	(cd $(d0root);\
	$(CC) $(CCFLAGS) $(OPT) offline_util/general/posix_filesize.c ;\
	mv posix_filesize.o $(SCRATCH)/offline_util/a13/posixsize527.o ;\
	)
$(d0root)/offline_util/offline_util.a(produc.o):\
  $(d0root)/offline_util/general/produc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/produc.f ;\
	mv produc.o $(SCRATCH)/offline_util/a13/produc.o ;\
	)
$(d0root)/offline_util/offline_util.a(prunit.o):\
  $(d0root)/offline_util/endtsk/prunit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/endtsk/prunit.f ;\
	mv prunit.o $(SCRATCH)/offline_util/a13/prunit.o ;\
	)
$(d0root)/offline_util/offline_util.a(purge_banks.o):\
  $(d0root)/offline_util/general/purge_banks.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/purge_banks.f ;\
	mv purge_banks.o $(SCRATCH)/offline_util/a13/purge_banks.o ;\
	)
$(d0root)/offline_util/offline_util.a(qprint.o):\
  $(d0root)/offline_util/general/qprint.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/qprint.f ;\
	mv qprint.o $(SCRATCH)/offline_util/a13/qprint.o ;\
	)
$(d0root)/offline_util/offline_util.a(reorder.o):\
  $(d0root)/offline_util/general/reorder.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/reorder.f ;\
	mv reorder.o $(SCRATCH)/offline_util/a13/reorder.o ;\
	)
$(d0root)/offline_util/offline_util.a(rfind2.o):\
  $(d0root)/offline_util/general/rfind2.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/rfind2.f ;\
	mv rfind2.o $(SCRATCH)/offline_util/a13/rfind2.o ;\
	)
$(d0root)/offline_util/offline_util.a(rflrot.o):\
  $(d0root)/offline_util/rotate/rflrot.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/rotate/rflrot.f ;\
	mv rflrot.o $(SCRATCH)/offline_util/a14/rflrot.o ;\
	)
$(d0root)/offline_util/offline_util.a(rotgea.o):\
  $(d0root)/offline_util/rotate/rotgea.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/rotate/rotgea.f ;\
	mv rotgea.o $(SCRATCH)/offline_util/a14/rotgea.o ;\
	)
$(d0root)/offline_util/offline_util.a(rotmlt.o):\
  $(d0root)/offline_util/rotate/rotmlt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/rotate/rotmlt.f ;\
	mv rotmlt.o $(SCRATCH)/offline_util/a14/rotmlt.o ;\
	)
$(d0root)/offline_util/offline_util.a(rotpax.o):\
  $(d0root)/offline_util/rotate/rotpax.f\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/rotate/rotpax.f ;\
	mv rotpax.o $(SCRATCH)/offline_util/a14/rotpax.o ;\
	)
$(d0root)/offline_util/offline_util.a(rpaxok.o):\
  $(d0root)/offline_util/rotate/rpaxok.f\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/rotate/rpaxok.f ;\
	mv rpaxok.o $(SCRATCH)/offline_util/a14/rpaxok.o ;\
	)
$(d0root)/offline_util/offline_util.a(rsort2.o):\
  $(d0root)/offline_util/general/rsort2.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/rsort2.f ;\
	mv rsort2.o $(SCRATCH)/offline_util/a14/rsort2.o ;\
	)
$(d0root)/offline_util/offline_util.a(rsorter.o):\
  $(d0root)/offline_util/general/rsorter.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/rsorter.f ;\
	mv rsorter.o $(SCRATCH)/offline_util/a14/rsorter.o ;\
	)
$(d0root)/offline_util/offline_util.a(rtest_bank.o):\
  $(d0root)/offline_util/general/rtest_bank.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/rtest_bank.f ;\
	mv rtest_bank.o $(SCRATCH)/offline_util/a14/rtest_bank.o ;\
	)
$(d0root)/offline_util/offline_util.a(run_type.o):\
  $(d0root)/offline_util/general/run_type.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/run_type.f ;\
	mv run_type.o $(SCRATCH)/offline_util/a14/run_type.o ;\
	)
$(d0root)/offline_util/offline_util.a(runsum_info.o):\
  $(d0root)/offline_util/general/runsum_info.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/runsum_info.f ;\
	mv runsum_info.o $(SCRATCH)/offline_util/a14/runsum_info.o ;\
	)
$(d0root)/offline_util/offline_util.a(scrap.o):\
  $(d0root)/offline_util/general/scrap.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/scrap.f ;\
	mv scrap.o $(SCRATCH)/offline_util/a14/scrap.o ;\
	)
$(d0root)/offline_util/offline_util.a(show_ages385.o):\
  $(d0root)/offline_util/general/show_packages.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/show_packages.f ;\
	mv show_packages.o $(SCRATCH)/offline_util/a14/show_ages385.o ;\
	)
$(d0root)/offline_util/offline_util.a(sort_dist410.o):\
  $(d0root)/offline_util/general/sort_vec_dist.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/sort_vec_dist.f ;\
	mv sort_vec_dist.o $(SCRATCH)/offline_util/a14/sort_dist410.o ;\
	)
$(d0root)/offline_util/offline_util.a(spltch.o):\
  $(d0root)/offline_util/general/spltch.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/spltch.f ;\
	mv spltch.o $(SCRATCH)/offline_util/a14/spltch.o ;\
	)
$(d0root)/offline_util/offline_util.a(srtchr.o):\
  $(d0root)/offline_util/general/srtchr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/srtchr.f ;\
	mv srtchr.o $(SCRATCH)/offline_util/a14/srtchr.o ;\
	)
$(d0root)/offline_util/offline_util.a(srtflt.o):\
  $(d0root)/offline_util/general/srtflt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/srtflt.f ;\
	mv srtflt.o $(SCRATCH)/offline_util/a14/srtflt.o ;\
	)
$(d0root)/offline_util/offline_util.a(srtint.o):\
  $(d0root)/offline_util/general/srtint.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/srtint.f ;\
	mv srtint.o $(SCRATCH)/offline_util/a14/srtint.o ;\
	)
$(d0root)/offline_util/offline_util.a(ssopen.o):\
  $(d0root)/offline_util/general/ssopen.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/ssopen.f ;\
	mv ssopen.o $(SCRATCH)/offline_util/a14/ssopen.o ;\
	)
$(d0root)/offline_util/offline_util.a(ssunit.o):\
  $(d0root)/offline_util/general/ssunit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/ssunit.f ;\
	mv ssunit.o $(SCRATCH)/offline_util/a14/ssunit.o ;\
	)
$(d0root)/offline_util/offline_util.a(strint.o):\
  $(d0root)/offline_util/general/strint.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/strint.f ;\
	mv strint.o $(SCRATCH)/offline_util/a14/strint.o ;\
	)
$(d0root)/offline_util/offline_util.a(strip_event.o):\
  $(d0root)/offline_util/general/strip_event.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/strip_event.f ;\
	mv strip_event.o $(SCRATCH)/offline_util/a15/strip_event.o ;\
	)
$(d0root)/offline_util/offline_util.a(strip_ini.o):\
  $(d0root)/offline_util/general/strip_ini.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/strip_ini.f ;\
	mv strip_ini.o $(SCRATCH)/offline_util/a15/strip_ini.o ;\
	)
$(d0root)/offline_util/offline_util.a(stroft.o):\
  $(d0root)/offline_util/general/stroft.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/stroft.f ;\
	mv stroft.o $(SCRATCH)/offline_util/a15/stroft.o ;\
	)
$(d0root)/offline_util/offline_util.a(sub_chr.o):\
  $(d0root)/offline_util/general/sub_chr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/sub_chr.f ;\
	mv sub_chr.o $(SCRATCH)/offline_util/a15/sub_chr.o ;\
	)
$(d0root)/offline_util/offline_util.a(swap_token.o):\
  $(d0root)/offline_util/general/swap_token.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/swap_token.f ;\
	mv swap_token.o $(SCRATCH)/offline_util/a15/swap_token.o ;\
	)
$(d0root)/offline_util/offline_util.a(swords.o):\
  $(d0root)/offline_util/general/swords.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/swords.f ;\
	mv swords.o $(SCRATCH)/offline_util/a15/swords.o ;\
	)
$(d0root)/offline_util/offline_util.a(token.o):\
  $(d0root)/offline_util/general/token.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/token.f ;\
	mv token.o $(SCRATCH)/offline_util/a15/token.o ;\
	)
$(d0root)/offline_util/offline_util.a(tranup.o):\
  $(d0root)/offline_util/general/tranup.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/tranup.f ;\
	mv tranup.o $(SCRATCH)/offline_util/a15/tranup.o ;\
	)
$(d0root)/offline_util/offline_util.a(trnlnm.o):\
  $(d0root)/offline_util/general/trnlnm.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/trnlnm.f ;\
	mv trnlnm.o $(SCRATCH)/offline_util/a15/trnlnm.o ;\
	)
$(d0root)/offline_util/offline_util.a(trulen.o):\
  $(d0root)/offline_util/general/trulen.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/trulen.f ;\
	mv trulen.o $(SCRATCH)/offline_util/a15/trulen.o ;\
	)
$(d0root)/offline_util/offline_util.a(ucopydd.o):\
  $(d0root)/offline_util/general/ucopydd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/ucopydd.f ;\
	mv ucopydd.o $(SCRATCH)/offline_util/a15/ucopydd.o ;\
	)
$(d0root)/offline_util/offline_util.a(ucopyds.o):\
  $(d0root)/offline_util/general/ucopyds.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/ucopyds.f ;\
	mv ucopyds.o $(SCRATCH)/offline_util/a15/ucopyds.o ;\
	)
$(d0root)/offline_util/offline_util.a(ucopysd.o):\
  $(d0root)/offline_util/general/ucopysd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/ucopysd.f ;\
	mv ucopysd.o $(SCRATCH)/offline_util/a15/ucopysd.o ;\
	)
$(d0root)/offline_util/offline_util.a(upcase.o):\
  $(d0root)/offline_util/general/upcase.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/upcase.f ;\
	mv upcase.o $(SCRATCH)/offline_util/a15/upcase.o ;\
	)
$(d0root)/offline_util/offline_util.a(uquit.o):\
  $(d0root)/offline_util/general/uquit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/uquit.f ;\
	mv uquit.o $(SCRATCH)/offline_util/a15/uquit.o ;\
	)
$(d0root)/offline_util/offline_util.a(usunit.o):\
  $(d0root)/offline_util/general/usunit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/usunit.f ;\
	mv usunit.o $(SCRATCH)/offline_util/a15/usunit.o ;\
	)
$(d0root)/offline_util/offline_util.a(value.o):\
  $(d0root)/offline_util/general/value.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/value.f ;\
	mv value.o $(SCRATCH)/offline_util/a15/value.o ;\
	)
$(d0root)/offline_util/offline_util.a(values.o):\
  $(d0root)/offline_util/general/values.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/values.f ;\
	mv values.o $(SCRATCH)/offline_util/a15/values.o ;\
	)
$(d0root)/offline_util/offline_util.a(valuex.o):\
  $(d0root)/offline_util/general/valuex.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/valuex.f ;\
	mv valuex.o $(SCRATCH)/offline_util/a15/valuex.o ;\
	)
$(d0root)/offline_util/offline_util.a(valusx.o):\
  $(d0root)/offline_util/general/valusx.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/valusx.f ;\
	mv valusx.o $(SCRATCH)/offline_util/a15/valusx.o ;\
	)
$(d0root)/offline_util/offline_util.a(valusy.o):\
  $(d0root)/offline_util/general/valusy.f\
  $(d0root)/offline_util/general/value.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/valusy.f ;\
	mv valusy.o $(SCRATCH)/offline_util/a16/valusy.o ;\
	)
$(d0root)/offline_util/offline_util.a(vaxoft.o):\
  $(d0root)/offline_util/general/vaxoft.f\
  $(d0library)/params/base_date.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/vaxoft.f ;\
	mv vaxoft.o $(SCRATCH)/offline_util/a16/vaxoft.o ;\
	)
$(d0root)/offline_util/offline_util.a(vec_print.o):\
  $(d0root)/offline_util/general/vec_print.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/vec_print.f ;\
	mv vec_print.o $(SCRATCH)/offline_util/a16/vec_print.o ;\
	)
$(d0root)/offline_util/offline_util.a(vnumi.o):\
  $(d0root)/offline_util/general/vnumi.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/vnumi.f ;\
	mv vnumi.o $(SCRATCH)/offline_util/a16/vnumi.o ;\
	)
$(d0root)/offline_util/offline_util.a(vnumr.o):\
  $(d0root)/offline_util/general/vnumr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/vnumr.f ;\
	mv vnumr.o $(SCRATCH)/offline_util/a16/vnumr.o ;\
	)
$(d0root)/offline_util/offline_util.a(word.o):\
  $(d0root)/offline_util/general/word.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/word.f ;\
	mv word.o $(SCRATCH)/offline_util/a16/word.o ;\
	)
$(d0root)/offline_util/offline_util.a(word_d_by600.o):\
  $(d0root)/offline_util/general/word_bounded_by.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/word_bounded_by.f ;\
	mv word_bounded_by.o $(SCRATCH)/offline_util/a16/word_d_by600.o ;\
	)
$(d0root)/offline_util/offline_util.a(xchker.o):\
  $(d0root)/offline_util/general/xchker.f\
  $(d0library)/params/byte_order.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/xchker.f ;\
	mv xchker.o $(SCRATCH)/offline_util/a16/xchker.o ;\
	)
$(d0root)/offline_util/offline_util.a(zchain.o):\
  $(d0root)/offline_util/general/zchain.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) offline_util/general/zchain.f ;\
	mv zchain.o $(SCRATCH)/offline_util/a16/zchain.o ;\
	)
pre:\
  $(d0root)/offline_util/general/add_alarm.f\
  $(d0root)/offline_util/general/add_chr.f\
  $(d0root)/offline_util/general/addstr.f\
  $(d0root)/offline_util/general/allwords.f\
  $(d0root)/offline_util/rotate/angnor.f\
  $(d0root)/offline_util/general/assign_name.f\
  $(d0root)/offline_util/general/binsea.f\
  $(d0root)/offline_util/general/cal_th.f\
  $(d0root)/offline_util/rotate/cartes.f\
  $(d0root)/offline_util/general/cdtime.f\
  $(d0root)/offline_util/general/chop.f\
  $(d0root)/offline_util/general/clofil.f\
  $(d0root)/offline_util/general/connect_init.f\
  $(d0root)/offline_util/general/cputim.f\
  $(d0root)/offline_util/general/cvsec_delta_time.f\
  $(d0root)/offline_util/rotate/cylind.f\
  $(d0root)/offline_util/general/d0_abort.f\
  $(d0root)/offline_util/general/d0csbuf.f\
  $(d0root)/offline_util/general/d0csopen.f\
  $(d0root)/offline_util/general/d0csopen_hook.f\
  $(d0root)/offline_util/general/d0hcmp_add.f\
  $(d0root)/offline_util/general/d0hcmp_cre_pawdirs.f\
  $(d0root)/offline_util/general/d0hcmp_dirscan.f\
  $(d0root)/offline_util/general/d0hcmp_get_hpar.f\
  $(d0root)/offline_util/general/d0hcmp_get_subdirs.f\
  $(d0root)/offline_util/general/d0hcmp_hst_rcp.f\
  $(d0root)/offline_util/general/d0hcmp_veri.f\
  $(d0root)/offline_util/general/d0open.f\
  $(d0root)/offline_util/general/d0rzopen.f\
  $(d0root)/offline_util/general/d0tloct.f\
  $(d0root)/offline_util/hbook/d0veri.f\
  $(d0root)/offline_util/general/dbl3init_rsrvr.f\
  $(d0root)/offline_util/general/dcos_from_eta_phi.f\
  $(d0root)/offline_util/general/dctoh.f\
  $(d0root)/offline_util/hmatrix/ddget.f\
  $(d0root)/offline_util/general/decode_paths.f\
  $(d0root)/offline_util/general/descr_header.f\
  $(d0root)/offline_util/general/det_eta.f\
  $(d0root)/offline_util/hbook/dgn.f\
  $(d0root)/offline_util/hbook/dgn_begin.f\
  $(d0root)/offline_util/hbook/dgn_get_prefixed_tags.f\
  $(d0root)/offline_util/hbook/dgn_get_prefixes.f\
  $(d0root)/offline_util/hbook/dgn_get_scales.f\
  $(d0root)/offline_util/hbook/dgn_get_tags.f\
  $(d0root)/offline_util/hbook/dhdir.f\
  $(d0root)/offline_util/hbook/dhdirectory.f\
  $(d0root)/offline_util/hbook/dhpath.f\
  $(d0root)/offline_util/hbook/dhsetdir.f\
  $(d0root)/offline_util/general/dhtoc.f\
  $(d0root)/offline_util/general/diff_phi.f\
  $(d0root)/offline_util/general/display_on.f\
  $(d0root)/offline_util/hbook/do_hbook.f\
  $(d0root)/offline_util/hbook/do_hbook_open.f\
  $(d0root)/offline_util/hbook/do_hbook_test.f\
  $(d0root)/offline_util/hbook/do_hbookn.f\
  $(d0root)/offline_util/hbook/do_hf1d.f\
  $(d0root)/offline_util/general/drcos.f\
  $(d0root)/offline_util/general/drop_banks.f\
  $(d0root)/offline_util/hmatrix/dump_event_quan.f\
  $(d0root)/offline_util/hmatrix/dump_top_event.f\
  $(d0root)/offline_util/hmatrix/dump_w_event.f\
  $(d0root)/offline_util/general/dvmod.f\
  $(d0root)/offline_util/general/end_hstor.f\
  $(d0root)/offline_util/endtsk/endet.f\
  $(d0root)/offline_util/endtsk/enios.f\
  $(d0root)/offline_util/endtsk/enlog.f\
  $(d0root)/offline_util/endtsk/enlstp.f\
  $(d0root)/offline_util/endtsk/enlun.f\
  $(d0root)/offline_util/endtsk/enprog.f\
  $(d0root)/offline_util/endtsk/enstoh.f\
  $(d0root)/offline_util/endtsk/enstp.f\
  $(d0root)/offline_util/endtsk/enstr.f\
  $(d0root)/offline_util/endtsk/enzstp.f\
  $(d0root)/offline_util/general/errmsg_setup.f\
  $(d0root)/offline_util/general/errout.f\
  $(d0root)/offline_util/general/eta_zcorr.f\
  $(d0root)/offline_util/general/etad.f\
  $(d0root)/offline_util/general/etoeta.f\
  $(d0root)/offline_util/general/event_control.f\
  $(d0root)/offline_util/general/evtoft.f\
  $(d0root)/offline_util/general/extcyl.f\
  $(d0root)/offline_util/general/file_name.f\
  $(d0root)/offline_util/general/filename_parse.f\
  $(d0root)/offline_util/general/filesize.f\
  $(d0root)/offline_util/general/find_token.f\
  $(d0root)/offline_util/general/gearot.f\
  $(d0root)/offline_util/general/get_active_runs.f\
  $(d0root)/offline_util/general/get_event_type.f\
  $(d0root)/offline_util/hbook/get_limits.f\
  $(d0root)/offline_util/general/get_next_token.f\
  $(d0root)/offline_util/general/get_process_names.f\
  $(d0root)/offline_util/general/get_run_filters.f\
  $(d0root)/offline_util/general/get_run_streams.f\
  $(d0root)/offline_util/general/getdbg.f\
  $(d0root)/offline_util/general/getofs.f\
  $(d0root)/offline_util/general/getword.f\
  $(d0root)/offline_util/general/gtsite.f\
  $(d0root)/offline_util/general/gtunit.f\
  $(d0root)/offline_util/hbook/gzhdir.f\
  $(d0root)/offline_util/hbook/gzhidn.f\
  $(d0root)/offline_util/hbook/hedit.f\
  $(d0root)/offline_util/hbook/hfast.f\
  $(d0root)/offline_util/hbook/hfff1.f\
  $(d0root)/offline_util/hbook/hinlib.f\
  $(d0root)/offline_util/endtsk/histit.f\
  $(d0root)/offline_util/general/hlpusr.f\
  $(d0root)/offline_util/hmatrix/hmatrix.f\
  $(d0root)/offline_util/hmatrix/hmatrix_anal.f\
  $(d0root)/offline_util/hmatrix/hmatrix_anal1.f\
  $(d0root)/offline_util/hmatrix/hmatrix_array.f\
  $(d0root)/offline_util/hmatrix/hmatrix_book_banks.f\
  $(d0root)/offline_util/hmatrix/hmatrix_chisquared.f\
  $(d0root)/offline_util/hmatrix/hmatrix_darray.f\
  $(d0root)/offline_util/hmatrix/hmatrix_diag_tran.f\
  $(d0root)/offline_util/hmatrix/hmatrix_do_averages.f\
  $(d0root)/offline_util/hmatrix/hmatrix_drop.f\
  $(d0root)/offline_util/hmatrix/hmatrix_dump_banks.f\
  $(d0root)/offline_util/hmatrix/hmatrix_dvect.f\
  $(d0root)/offline_util/hmatrix/hmatrix_event.f\
  $(d0root)/offline_util/hmatrix/hmatrix_fill.f\
  $(d0root)/offline_util/hmatrix/hmatrix_fin.f\
  $(d0root)/offline_util/hmatrix/hmatrix_find.f\
  $(d0root)/offline_util/hmatrix/hmatrix_get.f\
  $(d0root)/offline_util/hmatrix/hmatrix_ini.f\
  $(d0root)/offline_util/hmatrix/hmatrix_make_link_area.f\
  $(d0root)/offline_util/hmatrix/hmatrix_make_ntuple.f\
  $(d0root)/offline_util/hmatrix/hmatrix_name.f\
  $(d0root)/offline_util/hmatrix/hmatrix_predict.f\
  $(d0root)/offline_util/hmatrix/hmatrix_rz_close.f\
  $(d0root)/offline_util/hmatrix/hmatrix_rz_get_matrix.f\
  $(d0root)/offline_util/hmatrix/hmatrix_rz_list.f\
  $(d0root)/offline_util/hmatrix/hmatrix_rz_make.f\
  $(d0root)/offline_util/hmatrix/hmatrix_rz_save.f\
  $(d0root)/offline_util/hmatrix/hmatrix_set.f\
  $(d0root)/offline_util/hmatrix/hmatrix_set_links.f\
  $(d0root)/offline_util/hmatrix/hmatrix_set_sizes.f\
  $(d0root)/offline_util/hmatrix/hmatrix_vect.f\
  $(d0root)/offline_util/hmatrix/hmindex.f\
  $(d0root)/offline_util/hbook/hsearc.f\
  $(d0root)/offline_util/general/ifind2.f\
  $(d0root)/offline_util/general/in_ieta_window.f\
  $(d0root)/offline_util/general/in_iphi_window.f\
  $(d0root)/offline_util/general/inftmn.f\
  $(d0root)/offline_util/general/inihbk.f\
  $(d0root)/offline_util/general/intmsg.f\
  $(d0root)/offline_util/general/intstr.f\
  $(d0root)/offline_util/general/inzbra.f\
  $(d0root)/offline_util/general/ioserr.f\
  $(d0root)/offline_util/general/isort2.f\
  $(d0root)/offline_util/general/isorter.f\
  $(d0root)/offline_util/general/itest_bank.f\
  $(d0root)/offline_util/general/keep_banks.f\
  $(d0root)/offline_util/general/l1_passed.f\
  $(d0root)/offline_util/general/l1bit_passed.f\
  $(d0root)/offline_util/general/l2_passed.f\
  $(d0root)/offline_util/general/l2_vert.f\
  $(d0root)/offline_util/general/l2bit_on.f\
  $(d0root)/offline_util/general/l2bit_passed.f\
  $(d0root)/offline_util/general/l2bit_set.f\
  $(d0root)/offline_util/general/l2bit_tried.f\
  $(d0root)/offline_util/general/l2bit_unbiased.f\
  $(d0root)/offline_util/general/lchain.f\
  $(d0root)/offline_util/general/lcpath.f\
  $(d0root)/offline_util/general/lctree.f\
  $(d0root)/offline_util/general/locnum.f\
  $(d0root)/offline_util/general/locstr.f\
  $(d0root)/offline_util/general/locstr1.f\
  $(d0root)/offline_util/general/loctd0t.f\
  $(d0root)/offline_util/least_sq/lsq_anal.f\
  $(d0root)/offline_util/least_sq/lsq_bkmatrix.f\
  $(d0root)/offline_util/least_sq/lsq_collect_garbage.f\
  $(d0root)/offline_util/least_sq/lsq_diag_tran.f\
  $(d0root)/offline_util/least_sq/lsq_event.f\
  $(d0root)/offline_util/least_sq/lsq_event_lambda.f\
  $(d0root)/offline_util/least_sq/lsq_event_lambda1.f\
  $(d0root)/offline_util/least_sq/lsq_event_lambda2.f\
  $(d0root)/offline_util/least_sq/lsq_event_lambda3.f\
  $(d0root)/offline_util/least_sq/lsq_fin.f\
  $(d0root)/offline_util/least_sq/lsq_get_rows_columns.f\
  $(d0root)/offline_util/least_sq/lsq_get_val.f\
  $(d0root)/offline_util/least_sq/lsq_ini.f\
  $(d0root)/offline_util/least_sq/lsq_make_link_area.f\
  $(d0root)/offline_util/least_sq/lsq_matrix_add.f\
  $(d0root)/offline_util/least_sq/lsq_matrix_add_element.f\
  $(d0root)/offline_util/least_sq/lsq_matrix_copy.f\
  $(d0root)/offline_util/least_sq/lsq_matrix_delete.f\
  $(d0root)/offline_util/least_sq/lsq_matrix_diag.f\
  $(d0root)/offline_util/least_sq/lsq_matrix_exist.f\
  $(d0root)/offline_util/least_sq/lsq_matrix_fill.f\
  $(d0root)/offline_util/least_sq/lsq_matrix_get.f\
  $(d0root)/offline_util/least_sq/lsq_matrix_invert.f\
  $(d0root)/offline_util/least_sq/lsq_matrix_invert_test.f\
  $(d0root)/offline_util/least_sq/lsq_matrix_multiply.f\
  $(d0root)/offline_util/least_sq/lsq_matrix_print.f\
  $(d0root)/offline_util/least_sq/lsq_matrix_transform.f\
  $(d0root)/offline_util/least_sq/lsq_matrix_transpose.f\
  $(d0root)/offline_util/least_sq/lsq_poly_fit.f\
  $(d0root)/offline_util/least_sq/lsq_poly_init.f\
  $(d0root)/offline_util/least_sq/lsq_poly_load.f\
  $(d0root)/offline_util/least_sq/lsq_read_event.f\
  $(d0root)/offline_util/least_sq/lsq_rz_get.f\
  $(d0root)/offline_util/least_sq/lsq_rz_make.f\
  $(d0root)/offline_util/least_sq/lsq_rz_save.f\
  $(d0root)/offline_util/least_sq/lsq_scalar_multiply.f\
  $(d0root)/offline_util/least_sq/lsq_set_val.f\
  $(d0root)/offline_util/least_sq/lsq_test.f\
  $(d0root)/offline_util/least_sq/lsq_vector_direct_product.f\
  $(d0root)/offline_util/least_sq/lsq_vector_dot_product.f\
  $(d0root)/offline_util/general/lstoft.f\
  $(d0root)/offline_util/general/match_wild.f\
  $(d0root)/offline_util/general/matchjets.f\
  $(d0root)/offline_util/general/matrix_invert_1.f\
  $(d0root)/offline_util/general/matrix_invert_2.f\
  $(d0root)/offline_util/general/matrix_invert_test1.f\
  $(d0root)/offline_util/general/matrix_invert_test2.f\
  $(d0root)/offline_util/general/mrveto.f\
  $(d0root)/offline_util/general/mxprnd.f\
  $(d0root)/offline_util/general/mxprnt.f\
  $(d0root)/offline_util/endtsk/nextid.f\
  $(d0root)/offline_util/ntuple/ntuple_book.f\
  $(d0root)/offline_util/ntuple/ntuple_close.f\
  $(d0root)/offline_util/ntuple/ntuple_end.f\
  $(d0root)/offline_util/ntuple/ntuple_file_open.f\
  $(d0root)/offline_util/ntuple/ntuple_file_select.f\
  $(d0root)/offline_util/ntuple/ntuple_fill.f\
  $(d0root)/offline_util/ntuple/ntuple_flush.f\
  $(d0root)/offline_util/ntuple/ntuple_get.f\
  $(d0root)/offline_util/hbook/ntuple_get_ids.f\
  $(d0root)/offline_util/hbook/ntuple_get_info.f\
  $(d0root)/offline_util/ntuple/ntuple_list_info.f\
  $(d0root)/offline_util/ntuple/ntuple_out.f\
  $(d0root)/offline_util/ntuple/ntuple_save.f\
  $(d0root)/offline_util/general/offtim.f\
  $(d0root)/offline_util/general/oftevt.f\
  $(d0root)/offline_util/general/oftstr.f\
  $(d0root)/offline_util/general/oftvax.f\
  $(d0root)/offline_util/general/opnfil.f\
  $(d0root)/offline_util/general/outmsg.f\
  $(d0root)/offline_util/general/output_stream.f\
  $(d0root)/offline_util/general/output_stream_edit.f\
  $(d0root)/offline_util/general/parse.f\
  $(d0root)/offline_util/general/parse_files.f\
  $(d0root)/offline_util/general/parsfi.f\
  $(d0root)/offline_util/rotate/paxgrm.f\
  $(d0root)/offline_util/rotate/paxrot.f\
  $(d0root)/offline_util/general/pbd_get_next_flag.f\
  $(d0root)/offline_util/general/pbd_set_all_flags.f\
  $(d0root)/offline_util/general/pbd_temp_flag.f\
  $(d0root)/offline_util/general/peta_to_deta.f\
  $(d0root)/offline_util/general/pm_info.f\
  $(d0root)/offline_util/general/pm_message.f\
  $(d0root)/offline_util/rotate/polang.f\
  $(d0root)/offline_util/general/produc.f\
  $(d0root)/offline_util/endtsk/prunit.f\
  $(d0root)/offline_util/general/purge_banks.f\
  $(d0root)/offline_util/general/qprint.f\
  $(d0root)/offline_util/general/reorder.f\
  $(d0root)/offline_util/general/rfind2.f\
  $(d0root)/offline_util/rotate/rflrot.f\
  $(d0root)/offline_util/rotate/rotgea.f\
  $(d0root)/offline_util/rotate/rotmlt.f\
  $(d0root)/offline_util/rotate/rotpax.f\
  $(d0root)/offline_util/rotate/rpaxok.f\
  $(d0root)/offline_util/general/rsort2.f\
  $(d0root)/offline_util/general/rsorter.f\
  $(d0root)/offline_util/general/rtest_bank.f\
  $(d0root)/offline_util/general/run_type.f\
  $(d0root)/offline_util/general/runsum_info.f\
  $(d0root)/offline_util/general/scrap.f\
  $(d0root)/offline_util/general/show_packages.f\
  $(d0root)/offline_util/general/sort_vec_dist.f\
  $(d0root)/offline_util/general/spltch.f\
  $(d0root)/offline_util/general/srtchr.f\
  $(d0root)/offline_util/general/srtflt.f\
  $(d0root)/offline_util/general/srtint.f\
  $(d0root)/offline_util/general/ssopen.f\
  $(d0root)/offline_util/general/ssunit.f\
  $(d0root)/offline_util/general/strint.f\
  $(d0root)/offline_util/general/strip_event.f\
  $(d0root)/offline_util/general/strip_ini.f\
  $(d0root)/offline_util/general/stroft.f\
  $(d0root)/offline_util/general/sub_chr.f\
  $(d0root)/offline_util/general/swap_token.f\
  $(d0root)/offline_util/general/swords.f\
  $(d0root)/offline_util/general/token.f\
  $(d0root)/offline_util/general/tranup.f\
  $(d0root)/offline_util/general/trnlnm.f\
  $(d0root)/offline_util/general/trulen.f\
  $(d0root)/offline_util/general/ucopydd.f\
  $(d0root)/offline_util/general/ucopyds.f\
  $(d0root)/offline_util/general/ucopysd.f\
  $(d0root)/offline_util/general/upcase.f\
  $(d0root)/offline_util/general/uquit.f\
  $(d0root)/offline_util/general/usunit.f\
  $(d0root)/offline_util/general/value.f\
  $(d0root)/offline_util/general/values.f\
  $(d0root)/offline_util/general/valuex.f\
  $(d0root)/offline_util/general/valusx.f\
  $(d0root)/offline_util/general/valusy.f\
  $(d0root)/offline_util/general/vaxoft.f\
  $(d0root)/offline_util/general/vec_print.f\
  $(d0root)/offline_util/general/vnumi.f\
  $(d0root)/offline_util/general/vnumr.f\
  $(d0root)/offline_util/general/word.f\
  $(d0root)/offline_util/general/word_bounded_by.f\
  $(d0root)/offline_util/general/xchker.f\
  $(d0root)/offline_util/general/zchain.f
$(d0root)/offline_util/general/add_alarm.f:\
  $(d0root)/offline_util/general/add_alarm.for\
  $(d0library)/inc/examine2_alarm.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/add_alarm.for | vmstounix > $(d0root)/offline_util/general/add_alarm.f
$(d0root)/offline_util/general/add_chr.f:\
  $(d0root)/offline_util/general/add_chr.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/add_chr.for | vmstounix > $(d0root)/offline_util/general/add_chr.f
$(d0root)/offline_util/general/addstr.f:\
  $(d0root)/offline_util/general/addstr.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/addstr.for | vmstounix > $(d0root)/offline_util/general/addstr.f
$(d0root)/offline_util/general/allwords.f:\
  $(d0root)/offline_util/general/allwords.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/allwords.for | vmstounix > $(d0root)/offline_util/general/allwords.f
$(d0root)/offline_util/rotate/angnor.f:\
  $(d0root)/offline_util/rotate/angnor.for\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/rotate/angnor.for | vmstounix > $(d0root)/offline_util/rotate/angnor.f
$(d0root)/offline_util/general/assign_name.f:\
  $(d0root)/offline_util/general/assign_name.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/assign_name.for | vmstounix > $(d0root)/offline_util/general/assign_name.f
$(d0root)/offline_util/general/binsea.f:\
  $(d0root)/offline_util/general/binsea.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/binsea.for | vmstounix > $(d0root)/offline_util/general/binsea.f
$(d0root)/offline_util/general/cal_th.f:\
  $(d0root)/offline_util/general/cal_th.for\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/cal_th.for | vmstounix > $(d0root)/offline_util/general/cal_th.f
$(d0root)/offline_util/rotate/cartes.f:\
  $(d0root)/offline_util/rotate/cartes.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/rotate/cartes.for | vmstounix > $(d0root)/offline_util/rotate/cartes.f
$(d0root)/offline_util/general/cdtime.f:\
  $(d0root)/offline_util/general/cdtime.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/cdtime.for | vmstounix > $(d0root)/offline_util/general/cdtime.f
$(d0root)/offline_util/general/chop.f:\
  $(d0root)/offline_util/general/chop.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/chop.for | vmstounix > $(d0root)/offline_util/general/chop.f
$(d0root)/offline_util/general/clofil.f:\
  $(d0root)/offline_util/general/clofil.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/clofil.for | vmstounix > $(d0root)/offline_util/general/clofil.f
$(d0root)/offline_util/general/connect_init.f:\
  $(d0root)/offline_util/general/connect_init.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/connect_init.for | vmstounix > $(d0root)/offline_util/general/connect_init.f
$(d0root)/offline_util/general/cputim.f:\
  $(d0root)/offline_util/general/cputim.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/cputim.for | vmstounix > $(d0root)/offline_util/general/cputim.f
$(d0root)/offline_util/general/cvsec_delta_time.f:\
  $(d0root)/offline_util/general/cvsec_delta_time.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/cvsec_delta_time.for | vmstounix > $(d0root)/offline_util/general/cvsec_delta_time.f
$(d0root)/offline_util/rotate/cylind.f:\
  $(d0root)/offline_util/rotate/cylind.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/rotate/cylind.for | vmstounix > $(d0root)/offline_util/rotate/cylind.f
$(d0root)/offline_util/general/d0_abort.f:\
  $(d0root)/offline_util/general/d0_abort.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/d0_abort.for | vmstounix > $(d0root)/offline_util/general/d0_abort.f
$(d0root)/offline_util/general/d0csbuf.f:\
  $(d0root)/offline_util/general/d0csbuf.for\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/d0csbuf.for | vmstounix > $(d0root)/offline_util/general/d0csbuf.f
$(d0root)/offline_util/general/d0csopen.f:\
  $(d0root)/offline_util/general/d0csopen.for\
  $(d0library)/inc/czsock.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/d0csopen.for | vmstounix > $(d0root)/offline_util/general/d0csopen.f
$(d0root)/offline_util/general/d0csopen_hook.f:\
  $(d0root)/offline_util/general/d0csopen_hook.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/d0csopen_hook.for | vmstounix > $(d0root)/offline_util/general/d0csopen_hook.f
$(d0root)/offline_util/general/d0hcmp_add.f:\
  $(d0root)/offline_util/general/d0hcmp_add.for\
  $(d0library)/inc/pawc.inc\
  $(d0library)/params/d0hcmp.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/d0hcmp_add.for | vmstounix > $(d0root)/offline_util/general/d0hcmp_add.f
$(d0root)/offline_util/general/d0hcmp_cre_pawdirs.f:\
  $(d0root)/offline_util/general/d0hcmp_cre_pawdirs.for\
  $(d0library)/params/d0hcmp.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/d0hcmp_cre_pawdirs.for | vmstounix > $(d0root)/offline_util/general/d0hcmp_cre_pawdirs.f
$(d0root)/offline_util/general/d0hcmp_dirscan.f:\
  $(d0root)/offline_util/general/d0hcmp_dirscan.for\
  $(d0library)/params/d0hcmp.def\
  $(d0library)/inc/d0hcmp_hst_rcp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/d0hcmp_dirscan.for | vmstounix > $(d0root)/offline_util/general/d0hcmp_dirscan.f
$(d0root)/offline_util/general/d0hcmp_get_hpar.f:\
  $(d0root)/offline_util/general/d0hcmp_get_hpar.for\
  $(d0library)/params/d0hcmp.def\
  $(d0root)/offline_util/general/value.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/d0hcmp_get_hpar.for | vmstounix > $(d0root)/offline_util/general/d0hcmp_get_hpar.f
$(d0root)/offline_util/general/d0hcmp_get_subdirs.f:\
  $(d0root)/offline_util/general/d0hcmp_get_subdirs.for\
  $(d0library)/params/d0hcmp.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/d0hcmp_get_subdirs.for | vmstounix > $(d0root)/offline_util/general/d0hcmp_get_subdirs.f
$(d0root)/offline_util/general/d0hcmp_hst_rcp.f:\
  $(d0root)/offline_util/general/d0hcmp_hst_rcp.for\
  $(d0library)/inc/pawc.inc\
  $(d0library)/params/d0hcmp.def\
  $(d0library)/inc/d0hcmp_hst_rcp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/d0hcmp_hst_rcp.for | vmstounix > $(d0root)/offline_util/general/d0hcmp_hst_rcp.f
$(d0root)/offline_util/general/d0hcmp_veri.f:\
  $(d0root)/offline_util/general/d0hcmp_veri.for\
  $(d0library)/inc/pawc.inc\
  $(d0library)/params/d0hcmp.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/d0hcmp_veri.for | vmstounix > $(d0root)/offline_util/general/d0hcmp_veri.f
$(d0root)/offline_util/general/d0open.f:\
  $(d0root)/offline_util/general/d0open.for\
  $(d0library)/inc/fatcom.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/d0open.for | vmstounix > $(d0root)/offline_util/general/d0open.f
$(d0root)/offline_util/general/d0rzopen.f:\
  $(d0root)/offline_util/general/d0rzopen.for\
  $(d0library)/inc/fatcom.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/d0rzopen.for | vmstounix > $(d0root)/offline_util/general/d0rzopen.f
$(d0root)/offline_util/general/d0tloct.f:\
  $(d0root)/offline_util/general/d0tloct.for\
  $(d0library)/params/base_date.params
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/d0tloct.for | vmstounix > $(d0root)/offline_util/general/d0tloct.f
$(d0root)/offline_util/hbook/d0veri.f:\
  $(d0root)/offline_util/hbook/d0veri.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/d0veri.for | vmstounix > $(d0root)/offline_util/hbook/d0veri.f
$(d0root)/offline_util/general/dbl3init_rsrvr.f:\
  $(d0root)/offline_util/general/dbl3init_rsrvr.for\
  $(d0library)/inc/dbstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/dbl3init_rsrvr.for | vmstounix > $(d0root)/offline_util/general/dbl3init_rsrvr.f
$(d0root)/offline_util/general/dcos_from_eta_phi.f:\
  $(d0root)/offline_util/general/dcos_from_eta_phi.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/dcos_from_eta_phi.for | vmstounix > $(d0root)/offline_util/general/dcos_from_eta_phi.f
$(d0root)/offline_util/general/dctoh.f:\
  $(d0root)/offline_util/general/dctoh.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/dctoh.for | vmstounix > $(d0root)/offline_util/general/dctoh.f
$(d0root)/offline_util/hmatrix/ddget.f:\
  $(d0root)/offline_util/hmatrix/ddget.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/ddget.for | vmstounix > $(d0root)/offline_util/hmatrix/ddget.f
$(d0root)/offline_util/general/decode_paths.f:\
  $(d0root)/offline_util/general/decode_paths.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/decode_paths.for | vmstounix > $(d0root)/offline_util/general/decode_paths.f
$(d0root)/offline_util/general/descr_header.f:\
  $(d0root)/offline_util/general/descr_header.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/descr_header.for | vmstounix > $(d0root)/offline_util/general/descr_header.f
$(d0root)/offline_util/general/det_eta.f:\
  $(d0root)/offline_util/general/det_eta.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/det_eta.for | vmstounix > $(d0root)/offline_util/general/det_eta.f
$(d0root)/offline_util/hbook/dgn.f:\
  $(d0root)/offline_util/hbook/dgn.for\
  $(d0library)/inc/dgncom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/dgn.for | vmstounix > $(d0root)/offline_util/hbook/dgn.f
$(d0root)/offline_util/hbook/dgn_begin.f:\
  $(d0root)/offline_util/hbook/dgn_begin.for\
  $(d0library)/inc/dgncom.inc\
  $(d0library)/inc/dgncom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/dgn_begin.for | vmstounix > $(d0root)/offline_util/hbook/dgn_begin.f
$(d0root)/offline_util/hbook/dgn_get_prefixed_tags.f:\
  $(d0root)/offline_util/hbook/dgn_get_prefixed_tags.for\
  $(d0library)/inc/dgncom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/dgn_get_prefixed_tags.for | vmstounix > $(d0root)/offline_util/hbook/dgn_get_prefixed_tags.f
$(d0root)/offline_util/hbook/dgn_get_prefixes.f:\
  $(d0root)/offline_util/hbook/dgn_get_prefixes.for\
  $(d0library)/inc/dgncom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/dgn_get_prefixes.for | vmstounix > $(d0root)/offline_util/hbook/dgn_get_prefixes.f
$(d0root)/offline_util/hbook/dgn_get_scales.f:\
  $(d0root)/offline_util/hbook/dgn_get_scales.for\
  $(d0library)/inc/dgncom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/dgn_get_scales.for | vmstounix > $(d0root)/offline_util/hbook/dgn_get_scales.f
$(d0root)/offline_util/hbook/dgn_get_tags.f:\
  $(d0root)/offline_util/hbook/dgn_get_tags.for\
  $(d0library)/inc/dgncom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/dgn_get_tags.for | vmstounix > $(d0root)/offline_util/hbook/dgn_get_tags.f
$(d0root)/offline_util/hbook/dhdir.f:\
  $(d0root)/offline_util/hbook/dhdir.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/dhdir.for | vmstounix > $(d0root)/offline_util/hbook/dhdir.f
$(d0root)/offline_util/hbook/dhdirectory.f:\
  $(d0root)/offline_util/hbook/dhdirectory.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/dhdirectory.for | vmstounix > $(d0root)/offline_util/hbook/dhdirectory.f
$(d0root)/offline_util/hbook/dhpath.f:\
  $(d0root)/offline_util/hbook/dhpath.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/dhpath.for | vmstounix > $(d0root)/offline_util/hbook/dhpath.f
$(d0root)/offline_util/hbook/dhsetdir.f:\
  $(d0root)/offline_util/hbook/dhsetdir.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/dhsetdir.for | vmstounix > $(d0root)/offline_util/hbook/dhsetdir.f
$(d0root)/offline_util/general/dhtoc.f:\
  $(d0root)/offline_util/general/dhtoc.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/dhtoc.for | vmstounix > $(d0root)/offline_util/general/dhtoc.f
$(d0root)/offline_util/general/diff_phi.f:\
  $(d0root)/offline_util/general/diff_phi.for\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/diff_phi.for | vmstounix > $(d0root)/offline_util/general/diff_phi.f
$(d0root)/offline_util/general/display_on.f:\
  $(d0root)/offline_util/general/display_on.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/display_on.for | vmstounix > $(d0root)/offline_util/general/display_on.f
$(d0root)/offline_util/hbook/do_hbook.f:\
  $(d0root)/offline_util/hbook/do_hbook.for\
  $(d0root)/offline_util/general/value.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/do_hbook.for | vmstounix > $(d0root)/offline_util/hbook/do_hbook.f
$(d0root)/offline_util/hbook/do_hbook_open.f:\
  $(d0root)/offline_util/hbook/do_hbook_open.for\
  $(d0root)/offline_util/general/value.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/do_hbook_open.for | vmstounix > $(d0root)/offline_util/hbook/do_hbook_open.f
$(d0root)/offline_util/hbook/do_hbook_test.f:\
  $(d0root)/offline_util/hbook/do_hbook_test.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/do_hbook_test.for | vmstounix > $(d0root)/offline_util/hbook/do_hbook_test.f
$(d0root)/offline_util/hbook/do_hbookn.f:\
  $(d0root)/offline_util/hbook/do_hbookn.for\
  $(d0root)/offline_util/general/value.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/do_hbookn.for | vmstounix > $(d0root)/offline_util/hbook/do_hbookn.f
$(d0root)/offline_util/hbook/do_hf1d.f:\
  $(d0root)/offline_util/hbook/do_hf1d.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/do_hf1d.for | vmstounix > $(d0root)/offline_util/hbook/do_hf1d.f
$(d0root)/offline_util/general/drcos.f:\
  $(d0root)/offline_util/general/drcos.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/drcos.for | vmstounix > $(d0root)/offline_util/general/drcos.f
$(d0root)/offline_util/general/drop_banks.f:\
  $(d0root)/offline_util/general/drop_banks.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/drop_banks.for | vmstounix > $(d0root)/offline_util/general/drop_banks.f
$(d0root)/offline_util/hmatrix/dump_event_quan.f:\
  $(d0root)/offline_util/hmatrix/dump_event_quan.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/event_quan.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/dump_event_quan.for | vmstounix > $(d0root)/offline_util/hmatrix/dump_event_quan.f
$(d0root)/offline_util/hmatrix/dump_top_event.f:\
  $(d0root)/offline_util/hmatrix/dump_top_event.for\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/event_quan.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/dump_top_event.for | vmstounix > $(d0root)/offline_util/hmatrix/dump_top_event.f
$(d0root)/offline_util/hmatrix/dump_w_event.f:\
  $(d0root)/offline_util/hmatrix/dump_w_event.for\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/event_quan.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/dump_w_event.for | vmstounix > $(d0root)/offline_util/hmatrix/dump_w_event.f
$(d0root)/offline_util/general/dvmod.f:\
  $(d0root)/offline_util/general/dvmod.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/dvmod.for | vmstounix > $(d0root)/offline_util/general/dvmod.f
$(d0root)/offline_util/general/end_hstor.f:\
  $(d0root)/offline_util/general/end_hstor.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/end_hstor.for | vmstounix > $(d0root)/offline_util/general/end_hstor.f
$(d0root)/offline_util/endtsk/endet.f:\
  $(d0root)/offline_util/endtsk/endet.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/endtsk/endet.for | vmstounix > $(d0root)/offline_util/endtsk/endet.f
$(d0root)/offline_util/endtsk/enios.f:\
  $(d0root)/offline_util/endtsk/enios.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/endtsk/enios.for | vmstounix > $(d0root)/offline_util/endtsk/enios.f
$(d0root)/offline_util/endtsk/enlog.f:\
  $(d0root)/offline_util/endtsk/enlog.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/endtsk/enlog.for | vmstounix > $(d0root)/offline_util/endtsk/enlog.f
$(d0root)/offline_util/endtsk/enlstp.f:\
  $(d0root)/offline_util/endtsk/enlstp.for\
  $(d0library)/links/izslv0.link\
  $(d0library)/links/izsmuo.link\
  $(d0library)/links/izscal.link\
  $(d0library)/links/izsvtx.link\
  $(d0library)/links/izscdc.link\
  $(d0library)/links/izsfdc.link\
  $(d0library)/links/izstrd.link
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/endtsk/enlstp.for | vmstounix > $(d0root)/offline_util/endtsk/enlstp.f
$(d0root)/offline_util/endtsk/enlun.f:\
  $(d0root)/offline_util/endtsk/enlun.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/endtsk/enlun.for | vmstounix > $(d0root)/offline_util/endtsk/enlun.f
$(d0root)/offline_util/endtsk/enprog.f:\
  $(d0root)/offline_util/endtsk/enprog.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/endtsk/enprog.for | vmstounix > $(d0root)/offline_util/endtsk/enprog.f
$(d0root)/offline_util/endtsk/enstoh.f:\
  $(d0root)/offline_util/endtsk/enstoh.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/endtsk/enstoh.for | vmstounix > $(d0root)/offline_util/endtsk/enstoh.f
$(d0root)/offline_util/endtsk/enstp.f:\
  $(d0root)/offline_util/endtsk/enstp.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/endtsk/enstp.for | vmstounix > $(d0root)/offline_util/endtsk/enstp.f
$(d0root)/offline_util/endtsk/enstr.f:\
  $(d0root)/offline_util/endtsk/enstr.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/endtsk/enstr.for | vmstounix > $(d0root)/offline_util/endtsk/enstr.f
$(d0root)/offline_util/endtsk/enzstp.f:\
  $(d0root)/offline_util/endtsk/enzstp.for\
  $(d0library)/links/izstpo.link\
  $(d0library)/links/izstpc.link\
  $(d0library)/links/izstpn.link\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/endtsk/enzstp.for | vmstounix > $(d0root)/offline_util/endtsk/enzstp.f
$(d0root)/offline_util/general/errmsg_setup.f:\
  $(d0root)/offline_util/general/errmsg_setup.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/errmsg_setup.for | vmstounix > $(d0root)/offline_util/general/errmsg_setup.f
$(d0root)/offline_util/general/errout.f:\
  $(d0root)/offline_util/general/errout.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/errout.for | vmstounix > $(d0root)/offline_util/general/errout.f
$(d0root)/offline_util/general/eta_zcorr.f:\
  $(d0root)/offline_util/general/eta_zcorr.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/eta_zcorr.for | vmstounix > $(d0root)/offline_util/general/eta_zcorr.f
$(d0root)/offline_util/general/etad.f:\
  $(d0root)/offline_util/general/etad.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/etad.for | vmstounix > $(d0root)/offline_util/general/etad.f
$(d0root)/offline_util/general/etoeta.f:\
  $(d0root)/offline_util/general/etoeta.for\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/etoeta.for | vmstounix > $(d0root)/offline_util/general/etoeta.f
$(d0root)/offline_util/general/event_control.f:\
  $(d0root)/offline_util/general/event_control.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/event_control.for | vmstounix > $(d0root)/offline_util/general/event_control.f
$(d0root)/offline_util/general/evtoft.f:\
  $(d0root)/offline_util/general/evtoft.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/evtoft.for | vmstounix > $(d0root)/offline_util/general/evtoft.f
$(d0root)/offline_util/general/extcyl.f:\
  $(d0root)/offline_util/general/extcyl.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/extcyl.for | vmstounix > $(d0root)/offline_util/general/extcyl.f
$(d0root)/offline_util/general/file_name.f:\
  $(d0root)/offline_util/general/file_name.for\
  $(d0library)/inc/fatcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/file_name.for | vmstounix > $(d0root)/offline_util/general/file_name.f
$(d0root)/offline_util/general/filename_parse.f:\
  $(d0root)/offline_util/general/filename_parse.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/filename_parse.for | vmstounix > $(d0root)/offline_util/general/filename_parse.f
$(d0root)/offline_util/general/filesize.f:\
  $(d0root)/offline_util/general/filesize.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/filesize.for | vmstounix > $(d0root)/offline_util/general/filesize.f
$(d0root)/offline_util/general/find_token.f:\
  $(d0root)/offline_util/general/find_token.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/find_token.for | vmstounix > $(d0root)/offline_util/general/find_token.f
$(d0root)/offline_util/general/gearot.f:\
  $(d0root)/offline_util/general/gearot.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/gearot.for | vmstounix > $(d0root)/offline_util/general/gearot.f
$(d0root)/offline_util/general/get_active_runs.f:\
  $(d0root)/offline_util/general/get_active_runs.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/get_active_runs.for | vmstounix > $(d0root)/offline_util/general/get_active_runs.f
$(d0root)/offline_util/general/get_event_type.f:\
  $(d0root)/offline_util/general/get_event_type.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/get_event_type.for | vmstounix > $(d0root)/offline_util/general/get_event_type.f
$(d0root)/offline_util/hbook/get_limits.f:\
  $(d0root)/offline_util/hbook/get_limits.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/get_limits.for | vmstounix > $(d0root)/offline_util/hbook/get_limits.f
$(d0root)/offline_util/general/get_next_token.f:\
  $(d0root)/offline_util/general/get_next_token.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/get_next_token.for | vmstounix > $(d0root)/offline_util/general/get_next_token.f
$(d0root)/offline_util/general/get_process_names.f:\
  $(d0root)/offline_util/general/get_process_names.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/get_process_names.for | vmstounix > $(d0root)/offline_util/general/get_process_names.f
$(d0root)/offline_util/general/get_run_filters.f:\
  $(d0root)/offline_util/general/get_run_filters.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/get_run_filters.for | vmstounix > $(d0root)/offline_util/general/get_run_filters.f
$(d0root)/offline_util/general/get_run_streams.f:\
  $(d0root)/offline_util/general/get_run_streams.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/get_run_streams.for | vmstounix > $(d0root)/offline_util/general/get_run_streams.f
$(d0root)/offline_util/general/getdbg.f:\
  $(d0root)/offline_util/general/getdbg.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/getdbg.for | vmstounix > $(d0root)/offline_util/general/getdbg.f
$(d0root)/offline_util/general/getofs.f:\
  $(d0root)/offline_util/general/getofs.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/getofs.for | vmstounix > $(d0root)/offline_util/general/getofs.f
$(d0root)/offline_util/general/getword.f:\
  $(d0root)/offline_util/general/getword.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/getword.for | vmstounix > $(d0root)/offline_util/general/getword.f
$(d0root)/offline_util/general/gtsite.f:\
  $(d0root)/offline_util/general/gtsite.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/gtsite.for | vmstounix > $(d0root)/offline_util/general/gtsite.f
$(d0root)/offline_util/general/gtunit.f:\
  $(d0root)/offline_util/general/gtunit.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/gtunit.for | vmstounix > $(d0root)/offline_util/general/gtunit.f
$(d0root)/offline_util/hbook/gzhdir.f:\
  $(d0root)/offline_util/hbook/gzhdir.for\
  $(d0library)/inc/hcbook.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/gzhdir.for | vmstounix > $(d0root)/offline_util/hbook/gzhdir.f
$(d0root)/offline_util/hbook/gzhidn.f:\
  $(d0root)/offline_util/hbook/gzhidn.for\
  $(d0library)/inc/hcbook.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/gzhidn.for | vmstounix > $(d0root)/offline_util/hbook/gzhidn.f
$(d0root)/offline_util/hbook/hedit.f:\
  $(d0root)/offline_util/hbook/hedit.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/hedit.for | vmstounix > $(d0root)/offline_util/hbook/hedit.f
$(d0root)/offline_util/hbook/hfast.f:\
  $(d0root)/offline_util/hbook/hfast.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/hfast.for | vmstounix > $(d0root)/offline_util/hbook/hfast.f
$(d0root)/offline_util/hbook/hfff1.f:\
  $(d0root)/offline_util/hbook/hfff1.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/hfff1.for | vmstounix > $(d0root)/offline_util/hbook/hfff1.f
$(d0root)/offline_util/hbook/hinlib.f:\
  $(d0root)/offline_util/hbook/hinlib.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/hinlib.for | vmstounix > $(d0root)/offline_util/hbook/hinlib.f
$(d0root)/offline_util/endtsk/histit.f:\
  $(d0root)/offline_util/endtsk/histit.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/endtsk/histit.for | vmstounix > $(d0root)/offline_util/endtsk/histit.f
$(d0root)/offline_util/general/hlpusr.f:\
  $(d0root)/offline_util/general/hlpusr.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/hlpusr.for | vmstounix > $(d0root)/offline_util/general/hlpusr.f
$(d0root)/offline_util/hmatrix/hmatrix.f:\
  $(d0root)/offline_util/hmatrix/hmatrix.for\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix.f
$(d0root)/offline_util/hmatrix/hmatrix_anal.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_anal.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/event_quan.inc\
  $(d0library)/inc/zlinkc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_anal.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_anal.f
$(d0root)/offline_util/hmatrix/hmatrix_anal1.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_anal1.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_anal1.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_anal1.f
$(d0root)/offline_util/hmatrix/hmatrix_array.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_array.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_array.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_array.f
$(d0root)/offline_util/hmatrix/hmatrix_book_banks.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_book_banks.for\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zhmatrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_book_banks.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_book_banks.f
$(d0root)/offline_util/hmatrix/hmatrix_chisquared.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_chisquared.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/hmatrix_pars.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_chisquared.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_chisquared.f
$(d0root)/offline_util/hmatrix/hmatrix_darray.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_darray.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_darray.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_darray.f
$(d0root)/offline_util/hmatrix/hmatrix_diag_tran.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_diag_tran.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/hmatrix_pars.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_diag_tran.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_diag_tran.f
$(d0root)/offline_util/hmatrix/hmatrix_do_averages.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_do_averages.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zhmatrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_do_averages.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_do_averages.f
$(d0root)/offline_util/hmatrix/hmatrix_drop.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_drop.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_drop.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_drop.f
$(d0root)/offline_util/hmatrix/hmatrix_dump_banks.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_dump_banks.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_dump_banks.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_dump_banks.f
$(d0root)/offline_util/hmatrix/hmatrix_dvect.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_dvect.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_dvect.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_dvect.f
$(d0root)/offline_util/hmatrix/hmatrix_event.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_event.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/use_hmatrix.inc\
  $(d0library)/links/izquan.link
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_event.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_event.f
$(d0root)/offline_util/hmatrix/hmatrix_fill.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_fill.for\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_fill.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_fill.f
$(d0root)/offline_util/hmatrix/hmatrix_fin.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_fin.for\
  $(d0library)/inc/hmatrix_pars.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_fin.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_fin.f
$(d0root)/offline_util/hmatrix/hmatrix_find.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_find.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/nhmatrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_find.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_find.f
$(d0root)/offline_util/hmatrix/hmatrix_get.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_get.for\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_get.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_get.f
$(d0root)/offline_util/hmatrix/hmatrix_ini.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_ini.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/use_hmatrix.inc\
  $(d0library)/links/izhrcp.link
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_ini.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_ini.f
$(d0root)/offline_util/hmatrix/hmatrix_make_link_area.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_make_link_area.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_make_link_area.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_make_link_area.f
$(d0root)/offline_util/hmatrix/hmatrix_make_ntuple.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_make_ntuple.for\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/zlinkc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_make_ntuple.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_make_ntuple.f
$(d0root)/offline_util/hmatrix/hmatrix_name.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_name.for\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/nhmatrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_name.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_name.f
$(d0root)/offline_util/hmatrix/hmatrix_predict.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_predict.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/hmatrix_pars.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_predict.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_predict.f
$(d0root)/offline_util/hmatrix/hmatrix_rz_close.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_rz_close.for\
  $(d0library)/inc/hmatrix_pars.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_rz_close.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_rz_close.f
$(d0root)/offline_util/hmatrix/hmatrix_rz_get_matrix.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_rz_get_matrix.for\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/links/izhrcp.link\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_rz_get_matrix.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_rz_get_matrix.f
$(d0root)/offline_util/hmatrix/hmatrix_rz_list.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_rz_list.for\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_rz_list.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_rz_list.f
$(d0root)/offline_util/hmatrix/hmatrix_rz_make.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_rz_make.for\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_rz_make.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_rz_make.f
$(d0root)/offline_util/hmatrix/hmatrix_rz_save.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_rz_save.for\
  $(d0library)/inc/hmatrix_pars.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_rz_save.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_rz_save.f
$(d0root)/offline_util/hmatrix/hmatrix_set.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_set.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/nhmatrix.inc\
  $(d0library)/inc/ihmatrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_set.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_set.f
$(d0root)/offline_util/hmatrix/hmatrix_set_links.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_set_links.for\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/links/izaver.link\
  $(d0library)/links/izemat.link\
  $(d0library)/links/izhmat.link\
  $(d0library)/links/izhvis.link\
  $(d0library)/links/izhinv.link\
  $(d0library)/links/izeign.link\
  $(d0library)/links/izumat.link\
  $(d0library)/links/izhrcp.link\
  $(d0library)/links/izquan.link\
  $(d0library)/links/izdiag.link\
  $(d0library)/links/izwork.link\
  $(d0library)/links/izprod.link
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_set_links.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_set_links.f
$(d0root)/offline_util/hmatrix/hmatrix_set_sizes.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_set_sizes.for\
  $(d0library)/inc/hmatrix_pars.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_set_sizes.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_set_sizes.f
$(d0root)/offline_util/hmatrix/hmatrix_vect.f:\
  $(d0root)/offline_util/hmatrix/hmatrix_vect.for\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmatrix_vect.for | vmstounix > $(d0root)/offline_util/hmatrix/hmatrix_vect.f
$(d0root)/offline_util/hmatrix/hmindex.f:\
  $(d0root)/offline_util/hmatrix/hmindex.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hmatrix/hmindex.for | vmstounix > $(d0root)/offline_util/hmatrix/hmindex.f
$(d0root)/offline_util/hbook/hsearc.f:\
  $(d0root)/offline_util/hbook/hsearc.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/hsearc.for | vmstounix > $(d0root)/offline_util/hbook/hsearc.f
$(d0root)/offline_util/general/ifind2.f:\
  $(d0root)/offline_util/general/ifind2.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/ifind2.for | vmstounix > $(d0root)/offline_util/general/ifind2.f
$(d0root)/offline_util/general/in_ieta_window.f:\
  $(d0root)/offline_util/general/in_ieta_window.for\
  $(d0library)/params/cal_offline.params
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/in_ieta_window.for | vmstounix > $(d0root)/offline_util/general/in_ieta_window.f
$(d0root)/offline_util/general/in_iphi_window.f:\
  $(d0root)/offline_util/general/in_iphi_window.for\
  $(d0library)/params/cal_offline.params
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/in_iphi_window.for | vmstounix > $(d0root)/offline_util/general/in_iphi_window.f
$(d0root)/offline_util/general/inftmn.f:\
  $(d0root)/offline_util/general/inftmn.for\
  $(d0library)/inc/fatcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/inftmn.for | vmstounix > $(d0root)/offline_util/general/inftmn.f
$(d0root)/offline_util/general/inihbk.f:\
  $(d0root)/offline_util/general/inihbk.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/inihbk.for | vmstounix > $(d0root)/offline_util/general/inihbk.f
$(d0root)/offline_util/general/intmsg.f:\
  $(d0root)/offline_util/general/intmsg.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/intmsg.for | vmstounix > $(d0root)/offline_util/general/intmsg.f
$(d0root)/offline_util/general/intstr.f:\
  $(d0root)/offline_util/general/intstr.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/intstr.for | vmstounix > $(d0root)/offline_util/general/intstr.f
$(d0root)/offline_util/general/inzbra.f:\
  $(d0root)/offline_util/general/inzbra.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/inzbra.for | vmstounix > $(d0root)/offline_util/general/inzbra.f
$(d0root)/offline_util/general/ioserr.f:\
  $(d0root)/offline_util/general/ioserr.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/ioserr.for | vmstounix > $(d0root)/offline_util/general/ioserr.f
$(d0root)/offline_util/general/isort2.f:\
  $(d0root)/offline_util/general/isort2.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/isort2.for | vmstounix > $(d0root)/offline_util/general/isort2.f
$(d0root)/offline_util/general/isorter.f:\
  $(d0root)/offline_util/general/isorter.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/isorter.for | vmstounix > $(d0root)/offline_util/general/isorter.f
$(d0root)/offline_util/general/itest_bank.f:\
  $(d0root)/offline_util/general/itest_bank.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/itest_bank.for | vmstounix > $(d0root)/offline_util/general/itest_bank.f
$(d0root)/offline_util/general/keep_banks.f:\
  $(d0root)/offline_util/general/keep_banks.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/keep_banks.for | vmstounix > $(d0root)/offline_util/general/keep_banks.f
$(d0root)/offline_util/general/l1_passed.f:\
  $(d0root)/offline_util/general/l1_passed.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/l1_passed.for | vmstounix > $(d0root)/offline_util/general/l1_passed.f
$(d0root)/offline_util/general/l1bit_passed.f:\
  $(d0root)/offline_util/general/l1bit_passed.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/l1bit_passed.for | vmstounix > $(d0root)/offline_util/general/l1bit_passed.f
$(d0root)/offline_util/general/l2_passed.f:\
  $(d0root)/offline_util/general/l2_passed.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/l2_passed.for | vmstounix > $(d0root)/offline_util/general/l2_passed.f
$(d0root)/offline_util/general/l2_vert.f:\
  $(d0root)/offline_util/general/l2_vert.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/l2_vert.for | vmstounix > $(d0root)/offline_util/general/l2_vert.f
$(d0root)/offline_util/general/l2bit_on.f:\
  $(d0root)/offline_util/general/l2bit_on.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/l2bit_on.for | vmstounix > $(d0root)/offline_util/general/l2bit_on.f
$(d0root)/offline_util/general/l2bit_passed.f:\
  $(d0root)/offline_util/general/l2bit_passed.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izfilt.link
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/l2bit_passed.for | vmstounix > $(d0root)/offline_util/general/l2bit_passed.f
$(d0root)/offline_util/general/l2bit_set.f:\
  $(d0root)/offline_util/general/l2bit_set.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izfilt.link
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/l2bit_set.for | vmstounix > $(d0root)/offline_util/general/l2bit_set.f
$(d0root)/offline_util/general/l2bit_tried.f:\
  $(d0root)/offline_util/general/l2bit_tried.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izfilt.link
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/l2bit_tried.for | vmstounix > $(d0root)/offline_util/general/l2bit_tried.f
$(d0root)/offline_util/general/l2bit_unbiased.f:\
  $(d0root)/offline_util/general/l2bit_unbiased.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izfilt.link\
  $(d0library)/params/monitor_bit.params
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/l2bit_unbiased.for | vmstounix > $(d0root)/offline_util/general/l2bit_unbiased.f
$(d0root)/offline_util/general/lchain.f:\
  $(d0root)/offline_util/general/lchain.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/lchain.for | vmstounix > $(d0root)/offline_util/general/lchain.f
$(d0root)/offline_util/general/lcpath.f:\
  $(d0root)/offline_util/general/lcpath.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/lcpath.for | vmstounix > $(d0root)/offline_util/general/lcpath.f
$(d0root)/offline_util/general/lctree.f:\
  $(d0root)/offline_util/general/lctree.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/lctree.for | vmstounix > $(d0root)/offline_util/general/lctree.f
$(d0root)/offline_util/general/locnum.f:\
  $(d0root)/offline_util/general/locnum.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/locnum.for | vmstounix > $(d0root)/offline_util/general/locnum.f
$(d0root)/offline_util/general/locstr.f:\
  $(d0root)/offline_util/general/locstr.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/locstr.for | vmstounix > $(d0root)/offline_util/general/locstr.f
$(d0root)/offline_util/general/locstr1.f:\
  $(d0root)/offline_util/general/locstr1.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/locstr1.for | vmstounix > $(d0root)/offline_util/general/locstr1.f
$(d0root)/offline_util/general/loctd0t.f:\
  $(d0root)/offline_util/general/loctd0t.for\
  $(d0library)/params/base_date.params
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/loctd0t.for | vmstounix > $(d0root)/offline_util/general/loctd0t.f
$(d0root)/offline_util/least_sq/lsq_anal.f:\
  $(d0root)/offline_util/least_sq/lsq_anal.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_anal.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_anal.f
$(d0root)/offline_util/least_sq/lsq_bkmatrix.f:\
  $(d0root)/offline_util/least_sq/lsq_bkmatrix.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_bkmatrix.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_bkmatrix.f
$(d0root)/offline_util/least_sq/lsq_collect_garbage.f:\
  $(d0root)/offline_util/least_sq/lsq_collect_garbage.for\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_collect_garbage.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_collect_garbage.f
$(d0root)/offline_util/least_sq/lsq_diag_tran.f:\
  $(d0root)/offline_util/least_sq/lsq_diag_tran.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_diag_tran.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_diag_tran.f
$(d0root)/offline_util/least_sq/lsq_event.f:\
  $(d0root)/offline_util/least_sq/lsq_event.for\
  $(d0library)/inc/dump.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_event.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_event.f
$(d0root)/offline_util/least_sq/lsq_event_lambda.f:\
  $(d0root)/offline_util/least_sq/lsq_event_lambda.for\
  $(d0library)/inc/lsq_event.inc\
  $(d0library)/inc/dump.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_event_lambda.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_event_lambda.f
$(d0root)/offline_util/least_sq/lsq_event_lambda1.f:\
  $(d0root)/offline_util/least_sq/lsq_event_lambda1.for\
  $(d0library)/inc/dump.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_event_lambda1.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_event_lambda1.f
$(d0root)/offline_util/least_sq/lsq_event_lambda2.f:\
  $(d0root)/offline_util/least_sq/lsq_event_lambda2.for\
  $(d0library)/inc/dump.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_event_lambda2.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_event_lambda2.f
$(d0root)/offline_util/least_sq/lsq_event_lambda3.f:\
  $(d0root)/offline_util/least_sq/lsq_event_lambda3.for\
  $(d0library)/inc/dump.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_event_lambda3.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_event_lambda3.f
$(d0root)/offline_util/least_sq/lsq_fin.f:\
  $(d0root)/offline_util/least_sq/lsq_fin.for\
  $(d0library)/inc/lsq_pars.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_fin.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_fin.f
$(d0root)/offline_util/least_sq/lsq_get_rows_columns.f:\
  $(d0root)/offline_util/least_sq/lsq_get_rows_columns.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_get_rows_columns.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_get_rows_columns.f
$(d0root)/offline_util/least_sq/lsq_get_val.f:\
  $(d0root)/offline_util/least_sq/lsq_get_val.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_get_val.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_get_val.f
$(d0root)/offline_util/least_sq/lsq_ini.f:\
  $(d0root)/offline_util/least_sq/lsq_ini.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_pars.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_ini.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_ini.f
$(d0root)/offline_util/least_sq/lsq_make_link_area.f:\
  $(d0root)/offline_util/least_sq/lsq_make_link_area.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_make_link_area.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_make_link_area.f
$(d0root)/offline_util/least_sq/lsq_matrix_add.f:\
  $(d0root)/offline_util/least_sq/lsq_matrix_add.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_matrix_add.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_matrix_add.f
$(d0root)/offline_util/least_sq/lsq_matrix_add_element.f:\
  $(d0root)/offline_util/least_sq/lsq_matrix_add_element.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_matrix_add_element.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_matrix_add_element.f
$(d0root)/offline_util/least_sq/lsq_matrix_copy.f:\
  $(d0root)/offline_util/least_sq/lsq_matrix_copy.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_matrix_copy.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_matrix_copy.f
$(d0root)/offline_util/least_sq/lsq_matrix_delete.f:\
  $(d0root)/offline_util/least_sq/lsq_matrix_delete.for\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_matrix_delete.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_matrix_delete.f
$(d0root)/offline_util/least_sq/lsq_matrix_diag.f:\
  $(d0root)/offline_util/least_sq/lsq_matrix_diag.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_matrix_diag.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_matrix_diag.f
$(d0root)/offline_util/least_sq/lsq_matrix_exist.f:\
  $(d0root)/offline_util/least_sq/lsq_matrix_exist.for\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_matrix_exist.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_matrix_exist.f
$(d0root)/offline_util/least_sq/lsq_matrix_fill.f:\
  $(d0root)/offline_util/least_sq/lsq_matrix_fill.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_matrix_fill.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_matrix_fill.f
$(d0root)/offline_util/least_sq/lsq_matrix_get.f:\
  $(d0root)/offline_util/least_sq/lsq_matrix_get.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_matrix_get.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_matrix_get.f
$(d0root)/offline_util/least_sq/lsq_matrix_invert.f:\
  $(d0root)/offline_util/least_sq/lsq_matrix_invert.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_matrix_invert.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_matrix_invert.f
$(d0root)/offline_util/least_sq/lsq_matrix_invert_test.f:\
  $(d0root)/offline_util/least_sq/lsq_matrix_invert_test.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_matrix_invert_test.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_matrix_invert_test.f
$(d0root)/offline_util/least_sq/lsq_matrix_multiply.f:\
  $(d0root)/offline_util/least_sq/lsq_matrix_multiply.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_matrix_multiply.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_matrix_multiply.f
$(d0root)/offline_util/least_sq/lsq_matrix_print.f:\
  $(d0root)/offline_util/least_sq/lsq_matrix_print.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_matrix_print.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_matrix_print.f
$(d0root)/offline_util/least_sq/lsq_matrix_transform.f:\
  $(d0root)/offline_util/least_sq/lsq_matrix_transform.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_matrix_transform.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_matrix_transform.f
$(d0root)/offline_util/least_sq/lsq_matrix_transpose.f:\
  $(d0root)/offline_util/least_sq/lsq_matrix_transpose.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_matrix_transpose.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_matrix_transpose.f
$(d0root)/offline_util/least_sq/lsq_poly_fit.f:\
  $(d0root)/offline_util/least_sq/lsq_poly_fit.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_poly_fit.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_poly_fit.f
$(d0root)/offline_util/least_sq/lsq_poly_init.f:\
  $(d0root)/offline_util/least_sq/lsq_poly_init.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_poly_init.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_poly_init.f
$(d0root)/offline_util/least_sq/lsq_poly_load.f:\
  $(d0root)/offline_util/least_sq/lsq_poly_load.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_poly_load.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_poly_load.f
$(d0root)/offline_util/least_sq/lsq_read_event.f:\
  $(d0root)/offline_util/least_sq/lsq_read_event.for\
  $(d0library)/inc/lsq_event.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_read_event.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_read_event.f
$(d0root)/offline_util/least_sq/lsq_rz_get.f:\
  $(d0root)/offline_util/least_sq/lsq_rz_get.for\
  $(d0library)/inc/lsq_pars.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_rz_get.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_rz_get.f
$(d0root)/offline_util/least_sq/lsq_rz_make.f:\
  $(d0root)/offline_util/least_sq/lsq_rz_make.for\
  $(d0library)/inc/lsq_pars.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_rz_make.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_rz_make.f
$(d0root)/offline_util/least_sq/lsq_rz_save.f:\
  $(d0root)/offline_util/least_sq/lsq_rz_save.for\
  $(d0library)/inc/lsq_pars.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_rz_save.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_rz_save.f
$(d0root)/offline_util/least_sq/lsq_scalar_multiply.f:\
  $(d0root)/offline_util/least_sq/lsq_scalar_multiply.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_scalar_multiply.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_scalar_multiply.f
$(d0root)/offline_util/least_sq/lsq_set_val.f:\
  $(d0root)/offline_util/least_sq/lsq_set_val.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_set_val.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_set_val.f
$(d0root)/offline_util/least_sq/lsq_test.f:\
  $(d0root)/offline_util/least_sq/lsq_test.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_test.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_test.f
$(d0root)/offline_util/least_sq/lsq_vector_direct_product.f:\
  $(d0root)/offline_util/least_sq/lsq_vector_direct_product.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_vector_direct_product.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_vector_direct_product.f
$(d0root)/offline_util/least_sq/lsq_vector_dot_product.f:\
  $(d0root)/offline_util/least_sq/lsq_vector_dot_product.for\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlsq.inc\
  $(d0library)/inc/lsq_matrix.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/least_sq/lsq_vector_dot_product.for | vmstounix > $(d0root)/offline_util/least_sq/lsq_vector_dot_product.f
$(d0root)/offline_util/general/lstoft.f:\
  $(d0root)/offline_util/general/lstoft.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/lstoft.for | vmstounix > $(d0root)/offline_util/general/lstoft.f
$(d0root)/offline_util/general/match_wild.f:\
  $(d0root)/offline_util/general/match_wild.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/match_wild.for | vmstounix > $(d0root)/offline_util/general/match_wild.f
$(d0root)/offline_util/general/matchjets.f:\
  $(d0root)/offline_util/general/matchjets.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/matchjets.for | vmstounix > $(d0root)/offline_util/general/matchjets.f
$(d0root)/offline_util/general/matrix_invert_1.f:\
  $(d0root)/offline_util/general/matrix_invert_1.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/matrix_invert_1.for | vmstounix > $(d0root)/offline_util/general/matrix_invert_1.f
$(d0root)/offline_util/general/matrix_invert_2.f:\
  $(d0root)/offline_util/general/matrix_invert_2.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/matrix_invert_2.for | vmstounix > $(d0root)/offline_util/general/matrix_invert_2.f
$(d0root)/offline_util/general/matrix_invert_test1.f:\
  $(d0root)/offline_util/general/matrix_invert_test1.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/matrix_invert_test1.for | vmstounix > $(d0root)/offline_util/general/matrix_invert_test1.f
$(d0root)/offline_util/general/matrix_invert_test2.f:\
  $(d0root)/offline_util/general/matrix_invert_test2.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/matrix_invert_test2.for | vmstounix > $(d0root)/offline_util/general/matrix_invert_test2.f
$(d0root)/offline_util/general/mrveto.f:\
  $(d0root)/offline_util/general/mrveto.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/mrveto.for | vmstounix > $(d0root)/offline_util/general/mrveto.f
$(d0root)/offline_util/general/mxprnd.f:\
  $(d0root)/offline_util/general/mxprnd.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/mxprnd.for | vmstounix > $(d0root)/offline_util/general/mxprnd.f
$(d0root)/offline_util/general/mxprnt.f:\
  $(d0root)/offline_util/general/mxprnt.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/mxprnt.for | vmstounix > $(d0root)/offline_util/general/mxprnt.f
$(d0root)/offline_util/endtsk/nextid.f:\
  $(d0root)/offline_util/endtsk/nextid.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/endtsk/nextid.for | vmstounix > $(d0root)/offline_util/endtsk/nextid.f
$(d0root)/offline_util/ntuple/ntuple_book.f:\
  $(d0root)/offline_util/ntuple/ntuple_book.for\
  $(d0library)/inc/nt_housekeep.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/ntuple/ntuple_book.for | vmstounix > $(d0root)/offline_util/ntuple/ntuple_book.f
$(d0root)/offline_util/ntuple/ntuple_close.f:\
  $(d0root)/offline_util/ntuple/ntuple_close.for\
  $(d0library)/inc/nt_housekeep.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/ntuple/ntuple_close.for | vmstounix > $(d0root)/offline_util/ntuple/ntuple_close.f
$(d0root)/offline_util/ntuple/ntuple_end.f:\
  $(d0root)/offline_util/ntuple/ntuple_end.for\
  $(d0library)/inc/nt_housekeep.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/ntuple/ntuple_end.for | vmstounix > $(d0root)/offline_util/ntuple/ntuple_end.f
$(d0root)/offline_util/ntuple/ntuple_file_open.f:\
  $(d0root)/offline_util/ntuple/ntuple_file_open.for\
  $(d0library)/inc/nt_housekeep.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/ntuple/ntuple_file_open.for | vmstounix > $(d0root)/offline_util/ntuple/ntuple_file_open.f
$(d0root)/offline_util/ntuple/ntuple_file_select.f:\
  $(d0root)/offline_util/ntuple/ntuple_file_select.for\
  $(d0library)/inc/nt_housekeep.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/ntuple/ntuple_file_select.for | vmstounix > $(d0root)/offline_util/ntuple/ntuple_file_select.f
$(d0root)/offline_util/ntuple/ntuple_fill.f:\
  $(d0root)/offline_util/ntuple/ntuple_fill.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/ntuple/ntuple_fill.for | vmstounix > $(d0root)/offline_util/ntuple/ntuple_fill.f
$(d0root)/offline_util/ntuple/ntuple_flush.f:\
  $(d0root)/offline_util/ntuple/ntuple_flush.for\
  $(d0library)/inc/nt_housekeep.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/ntuple/ntuple_flush.for | vmstounix > $(d0root)/offline_util/ntuple/ntuple_flush.f
$(d0root)/offline_util/ntuple/ntuple_get.f:\
  $(d0root)/offline_util/ntuple/ntuple_get.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/ntuple/ntuple_get.for | vmstounix > $(d0root)/offline_util/ntuple/ntuple_get.f
$(d0root)/offline_util/hbook/ntuple_get_ids.f:\
  $(d0root)/offline_util/hbook/ntuple_get_ids.for\
  $(d0library)/inc/pawc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/ntuple_get_ids.for | vmstounix > $(d0root)/offline_util/hbook/ntuple_get_ids.f
$(d0root)/offline_util/hbook/ntuple_get_info.f:\
  $(d0root)/offline_util/hbook/ntuple_get_info.for\
  $(d0library)/inc/pawc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/hbook/ntuple_get_info.for | vmstounix > $(d0root)/offline_util/hbook/ntuple_get_info.f
$(d0root)/offline_util/ntuple/ntuple_list_info.f:\
  $(d0root)/offline_util/ntuple/ntuple_list_info.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/ntuple/ntuple_list_info.for | vmstounix > $(d0root)/offline_util/ntuple/ntuple_list_info.f
$(d0root)/offline_util/ntuple/ntuple_out.f:\
  $(d0root)/offline_util/ntuple/ntuple_out.for\
  $(d0library)/inc/nt_housekeep.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/ntuple/ntuple_out.for | vmstounix > $(d0root)/offline_util/ntuple/ntuple_out.f
$(d0root)/offline_util/ntuple/ntuple_save.f:\
  $(d0root)/offline_util/ntuple/ntuple_save.for\
  $(d0library)/inc/nt_housekeep.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/ntuple/ntuple_save.for | vmstounix > $(d0root)/offline_util/ntuple/ntuple_save.f
$(d0root)/offline_util/general/offtim.f:\
  $(d0root)/offline_util/general/offtim.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/offtim.for | vmstounix > $(d0root)/offline_util/general/offtim.f
$(d0root)/offline_util/general/oftevt.f:\
  $(d0root)/offline_util/general/oftevt.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/oftevt.for | vmstounix > $(d0root)/offline_util/general/oftevt.f
$(d0root)/offline_util/general/oftstr.f:\
  $(d0root)/offline_util/general/oftstr.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/oftstr.for | vmstounix > $(d0root)/offline_util/general/oftstr.f
$(d0root)/offline_util/general/oftvax.f:\
  $(d0root)/offline_util/general/oftvax.for\
  $(d0library)/params/base_date.params
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/oftvax.for | vmstounix > $(d0root)/offline_util/general/oftvax.f
$(d0root)/offline_util/general/opnfil.f:\
  $(d0root)/offline_util/general/opnfil.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/opnfil.for | vmstounix > $(d0root)/offline_util/general/opnfil.f
$(d0root)/offline_util/general/outmsg.f:\
  $(d0root)/offline_util/general/outmsg.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc\
  $(d0library)/inc/comchr.inc\
  $(d0library)/inc/smgcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/outmsg.for | vmstounix > $(d0root)/offline_util/general/outmsg.f
$(d0root)/offline_util/general/output_stream.f:\
  $(d0root)/offline_util/general/output_stream.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/output_stream.for | vmstounix > $(d0root)/offline_util/general/output_stream.f
$(d0root)/offline_util/general/output_stream_edit.f:\
  $(d0root)/offline_util/general/output_stream_edit.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/output_stream_edit.for | vmstounix > $(d0root)/offline_util/general/output_stream_edit.f
$(d0root)/offline_util/general/parse.f:\
  $(d0root)/offline_util/general/parse.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/parse.for | vmstounix > $(d0root)/offline_util/general/parse.f
$(d0root)/offline_util/general/parse_files.f:\
  $(d0root)/offline_util/general/parse_files.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/parse_files.for | vmstounix > $(d0root)/offline_util/general/parse_files.f
$(d0root)/offline_util/general/parsfi.f:\
  $(d0root)/offline_util/general/parsfi.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/parsfi.for | vmstounix > $(d0root)/offline_util/general/parsfi.f
$(d0root)/offline_util/rotate/paxgrm.f:\
  $(d0root)/offline_util/rotate/paxgrm.for\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/rotate/paxgrm.for | vmstounix > $(d0root)/offline_util/rotate/paxgrm.f
$(d0root)/offline_util/rotate/paxrot.f:\
  $(d0root)/offline_util/rotate/paxrot.for\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/rotate/paxrot.for | vmstounix > $(d0root)/offline_util/rotate/paxrot.f
$(d0root)/offline_util/general/pbd_get_next_flag.f:\
  $(d0root)/offline_util/general/pbd_get_next_flag.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/pbd_get_next_flag.for | vmstounix > $(d0root)/offline_util/general/pbd_get_next_flag.f
$(d0root)/offline_util/general/pbd_set_all_flags.f:\
  $(d0root)/offline_util/general/pbd_set_all_flags.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/pbd_set_all_flags.for | vmstounix > $(d0root)/offline_util/general/pbd_set_all_flags.f
$(d0root)/offline_util/general/pbd_temp_flag.f:\
  $(d0root)/offline_util/general/pbd_temp_flag.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/pbd_temp_flag.for | vmstounix > $(d0root)/offline_util/general/pbd_temp_flag.f
$(d0root)/offline_util/general/peta_to_deta.f:\
  $(d0root)/offline_util/general/peta_to_deta.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/peta_to_deta.for | vmstounix > $(d0root)/offline_util/general/peta_to_deta.f
$(d0root)/offline_util/general/pm_info.f:\
  $(d0root)/offline_util/general/pm_info.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/pm_info.for | vmstounix > $(d0root)/offline_util/general/pm_info.f
$(d0root)/offline_util/general/pm_message.f:\
  $(d0root)/offline_util/general/pm_message.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/pm_message.for | vmstounix > $(d0root)/offline_util/general/pm_message.f
$(d0root)/offline_util/rotate/polang.f:\
  $(d0root)/offline_util/rotate/polang.for\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/rotate/polang.for | vmstounix > $(d0root)/offline_util/rotate/polang.f
$(d0root)/offline_util/general/produc.f:\
  $(d0root)/offline_util/general/produc.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/produc.for | vmstounix > $(d0root)/offline_util/general/produc.f
$(d0root)/offline_util/endtsk/prunit.f:\
  $(d0root)/offline_util/endtsk/prunit.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/endtsk/prunit.for | vmstounix > $(d0root)/offline_util/endtsk/prunit.f
$(d0root)/offline_util/general/purge_banks.f:\
  $(d0root)/offline_util/general/purge_banks.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/purge_banks.for | vmstounix > $(d0root)/offline_util/general/purge_banks.f
$(d0root)/offline_util/general/qprint.f:\
  $(d0root)/offline_util/general/qprint.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/qprint.for | vmstounix > $(d0root)/offline_util/general/qprint.f
$(d0root)/offline_util/general/reorder.f:\
  $(d0root)/offline_util/general/reorder.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/reorder.for | vmstounix > $(d0root)/offline_util/general/reorder.f
$(d0root)/offline_util/general/rfind2.f:\
  $(d0root)/offline_util/general/rfind2.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/rfind2.for | vmstounix > $(d0root)/offline_util/general/rfind2.f
$(d0root)/offline_util/rotate/rflrot.f:\
  $(d0root)/offline_util/rotate/rflrot.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/rotate/rflrot.for | vmstounix > $(d0root)/offline_util/rotate/rflrot.f
$(d0root)/offline_util/rotate/rotgea.f:\
  $(d0root)/offline_util/rotate/rotgea.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/rotate/rotgea.for | vmstounix > $(d0root)/offline_util/rotate/rotgea.f
$(d0root)/offline_util/rotate/rotmlt.f:\
  $(d0root)/offline_util/rotate/rotmlt.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/rotate/rotmlt.for | vmstounix > $(d0root)/offline_util/rotate/rotmlt.f
$(d0root)/offline_util/rotate/rotpax.f:\
  $(d0root)/offline_util/rotate/rotpax.for\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/rotate/rotpax.for | vmstounix > $(d0root)/offline_util/rotate/rotpax.f
$(d0root)/offline_util/rotate/rpaxok.f:\
  $(d0root)/offline_util/rotate/rpaxok.for\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/rotate/rpaxok.for | vmstounix > $(d0root)/offline_util/rotate/rpaxok.f
$(d0root)/offline_util/general/rsort2.f:\
  $(d0root)/offline_util/general/rsort2.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/rsort2.for | vmstounix > $(d0root)/offline_util/general/rsort2.f
$(d0root)/offline_util/general/rsorter.f:\
  $(d0root)/offline_util/general/rsorter.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/rsorter.for | vmstounix > $(d0root)/offline_util/general/rsorter.f
$(d0root)/offline_util/general/rtest_bank.f:\
  $(d0root)/offline_util/general/rtest_bank.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/rtest_bank.for | vmstounix > $(d0root)/offline_util/general/rtest_bank.f
$(d0root)/offline_util/general/run_type.f:\
  $(d0root)/offline_util/general/run_type.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/run_type.for | vmstounix > $(d0root)/offline_util/general/run_type.f
$(d0root)/offline_util/general/runsum_info.f:\
  $(d0root)/offline_util/general/runsum_info.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/runsum_info.for | vmstounix > $(d0root)/offline_util/general/runsum_info.f
$(d0root)/offline_util/general/scrap.f:\
  $(d0root)/offline_util/general/scrap.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/scrap.for | vmstounix > $(d0root)/offline_util/general/scrap.f
$(d0root)/offline_util/general/show_packages.f:\
  $(d0root)/offline_util/general/show_packages.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/show_packages.for | vmstounix > $(d0root)/offline_util/general/show_packages.f
$(d0root)/offline_util/general/sort_vec_dist.f:\
  $(d0root)/offline_util/general/sort_vec_dist.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/sort_vec_dist.for | vmstounix > $(d0root)/offline_util/general/sort_vec_dist.f
$(d0root)/offline_util/general/spltch.f:\
  $(d0root)/offline_util/general/spltch.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/spltch.for | vmstounix > $(d0root)/offline_util/general/spltch.f
$(d0root)/offline_util/general/srtchr.f:\
  $(d0root)/offline_util/general/srtchr.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/srtchr.for | vmstounix > $(d0root)/offline_util/general/srtchr.f
$(d0root)/offline_util/general/srtflt.f:\
  $(d0root)/offline_util/general/srtflt.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/srtflt.for | vmstounix > $(d0root)/offline_util/general/srtflt.f
$(d0root)/offline_util/general/srtint.f:\
  $(d0root)/offline_util/general/srtint.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/srtint.for | vmstounix > $(d0root)/offline_util/general/srtint.f
$(d0root)/offline_util/general/ssopen.f:\
  $(d0root)/offline_util/general/ssopen.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/ssopen.for | vmstounix > $(d0root)/offline_util/general/ssopen.f
$(d0root)/offline_util/general/ssunit.f:\
  $(d0root)/offline_util/general/ssunit.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/ssunit.for | vmstounix > $(d0root)/offline_util/general/ssunit.f
$(d0root)/offline_util/general/strint.f:\
  $(d0root)/offline_util/general/strint.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/strint.for | vmstounix > $(d0root)/offline_util/general/strint.f
$(d0root)/offline_util/general/strip_event.f:\
  $(d0root)/offline_util/general/strip_event.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/strip_event.for | vmstounix > $(d0root)/offline_util/general/strip_event.f
$(d0root)/offline_util/general/strip_ini.f:\
  $(d0root)/offline_util/general/strip_ini.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/strip_ini.for | vmstounix > $(d0root)/offline_util/general/strip_ini.f
$(d0root)/offline_util/general/stroft.f:\
  $(d0root)/offline_util/general/stroft.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/stroft.for | vmstounix > $(d0root)/offline_util/general/stroft.f
$(d0root)/offline_util/general/sub_chr.f:\
  $(d0root)/offline_util/general/sub_chr.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/sub_chr.for | vmstounix > $(d0root)/offline_util/general/sub_chr.f
$(d0root)/offline_util/general/swap_token.f:\
  $(d0root)/offline_util/general/swap_token.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/swap_token.for | vmstounix > $(d0root)/offline_util/general/swap_token.f
$(d0root)/offline_util/general/swords.f:\
  $(d0root)/offline_util/general/swords.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/swords.for | vmstounix > $(d0root)/offline_util/general/swords.f
$(d0root)/offline_util/general/token.f:\
  $(d0root)/offline_util/general/token.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/token.for | vmstounix > $(d0root)/offline_util/general/token.f
$(d0root)/offline_util/general/tranup.f:\
  $(d0root)/offline_util/general/tranup.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/tranup.for | vmstounix > $(d0root)/offline_util/general/tranup.f
$(d0root)/offline_util/general/trnlnm.f:\
  $(d0root)/offline_util/general/trnlnm.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/trnlnm.for | vmstounix > $(d0root)/offline_util/general/trnlnm.f
$(d0root)/offline_util/general/trulen.f:\
  $(d0root)/offline_util/general/trulen.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/trulen.for | vmstounix > $(d0root)/offline_util/general/trulen.f
$(d0root)/offline_util/general/ucopydd.f:\
  $(d0root)/offline_util/general/ucopydd.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/ucopydd.for | vmstounix > $(d0root)/offline_util/general/ucopydd.f
$(d0root)/offline_util/general/ucopyds.f:\
  $(d0root)/offline_util/general/ucopyds.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/ucopyds.for | vmstounix > $(d0root)/offline_util/general/ucopyds.f
$(d0root)/offline_util/general/ucopysd.f:\
  $(d0root)/offline_util/general/ucopysd.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/ucopysd.for | vmstounix > $(d0root)/offline_util/general/ucopysd.f
$(d0root)/offline_util/general/upcase.f:\
  $(d0root)/offline_util/general/upcase.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/upcase.for | vmstounix > $(d0root)/offline_util/general/upcase.f
$(d0root)/offline_util/general/uquit.f:\
  $(d0root)/offline_util/general/uquit.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/uquit.for | vmstounix > $(d0root)/offline_util/general/uquit.f
$(d0root)/offline_util/general/usunit.f:\
  $(d0root)/offline_util/general/usunit.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/usunit.for | vmstounix > $(d0root)/offline_util/general/usunit.f
$(d0root)/offline_util/general/value.f:\
  $(d0root)/offline_util/general/value.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/value.for | vmstounix > $(d0root)/offline_util/general/value.f
$(d0root)/offline_util/general/values.f:\
  $(d0root)/offline_util/general/values.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/values.for | vmstounix > $(d0root)/offline_util/general/values.f
$(d0root)/offline_util/general/valuex.f:\
  $(d0root)/offline_util/general/valuex.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/valuex.for | vmstounix > $(d0root)/offline_util/general/valuex.f
$(d0root)/offline_util/general/valusx.f:\
  $(d0root)/offline_util/general/valusx.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/valusx.for | vmstounix > $(d0root)/offline_util/general/valusx.f
$(d0root)/offline_util/general/valusy.f:\
  $(d0root)/offline_util/general/valusy.for\
  $(d0root)/offline_util/general/value.def
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/valusy.for | vmstounix > $(d0root)/offline_util/general/valusy.f
$(d0root)/offline_util/general/vaxoft.f:\
  $(d0root)/offline_util/general/vaxoft.for\
  $(d0library)/params/base_date.params
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/vaxoft.for | vmstounix > $(d0root)/offline_util/general/vaxoft.f
$(d0root)/offline_util/general/vec_print.f:\
  $(d0root)/offline_util/general/vec_print.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/vec_print.for | vmstounix > $(d0root)/offline_util/general/vec_print.f
$(d0root)/offline_util/general/vnumi.f:\
  $(d0root)/offline_util/general/vnumi.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/vnumi.for | vmstounix > $(d0root)/offline_util/general/vnumi.f
$(d0root)/offline_util/general/vnumr.f:\
  $(d0root)/offline_util/general/vnumr.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/vnumr.for | vmstounix > $(d0root)/offline_util/general/vnumr.f
$(d0root)/offline_util/general/word.f:\
  $(d0root)/offline_util/general/word.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/word.for | vmstounix > $(d0root)/offline_util/general/word.f
$(d0root)/offline_util/general/word_bounded_by.f:\
  $(d0root)/offline_util/general/word_bounded_by.for
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/word_bounded_by.for | vmstounix > $(d0root)/offline_util/general/word_bounded_by.f
$(d0root)/offline_util/general/xchker.f:\
  $(d0root)/offline_util/general/xchker.for\
  $(d0library)/params/byte_order.params
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/xchker.for | vmstounix > $(d0root)/offline_util/general/xchker.f
$(d0root)/offline_util/general/zchain.f:\
  $(d0root)/offline_util/general/zchain.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/offline_util/general/zchain.for | vmstounix > $(d0root)/offline_util/general/zchain.f
for:
	touch $(d0root)/offline_util/general/add_alarm.for
	touch $(d0root)/offline_util/general/add_chr.for
	touch $(d0root)/offline_util/general/addstr.for
	touch $(d0root)/offline_util/general/allwords.for
	touch $(d0root)/offline_util/rotate/angnor.for
	touch $(d0root)/offline_util/general/assign_name.for
	touch $(d0root)/offline_util/general/binsea.for
	touch $(d0root)/offline_util/general/cal_th.for
	touch $(d0root)/offline_util/rotate/cartes.for
	touch $(d0root)/offline_util/general/cdtime.for
	touch $(d0root)/offline_util/general/chop.for
	touch $(d0root)/offline_util/general/clofil.for
	touch $(d0root)/offline_util/general/connect_init.for
	touch $(d0root)/offline_util/general/cputim.for
	touch $(d0root)/offline_util/general/cvsec_delta_time.for
	touch $(d0root)/offline_util/rotate/cylind.for
	touch $(d0root)/offline_util/general/d0_abort.for
	touch $(d0root)/offline_util/general/d0csbuf.for
	touch $(d0root)/offline_util/general/d0csopen.for
	touch $(d0root)/offline_util/general/d0csopen_hook.for
	touch $(d0root)/offline_util/general/d0hcmp_add.for
	touch $(d0root)/offline_util/general/d0hcmp_cre_pawdirs.for
	touch $(d0root)/offline_util/general/d0hcmp_dirscan.for
	touch $(d0root)/offline_util/general/d0hcmp_get_hpar.for
	touch $(d0root)/offline_util/general/d0hcmp_get_subdirs.for
	touch $(d0root)/offline_util/general/d0hcmp_hst_rcp.for
	touch $(d0root)/offline_util/general/d0hcmp_veri.for
	touch $(d0root)/offline_util/general/d0open.for
	touch $(d0root)/offline_util/general/d0rzopen.for
	touch $(d0root)/offline_util/general/d0tloct.for
	touch $(d0root)/offline_util/hbook/d0veri.for
	touch $(d0root)/offline_util/general/dbl3init_rsrvr.for
	touch $(d0root)/offline_util/general/dcos_from_eta_phi.for
	touch $(d0root)/offline_util/general/dctoh.for
	touch $(d0root)/offline_util/hmatrix/ddget.for
	touch $(d0root)/offline_util/general/decode_paths.for
	touch $(d0root)/offline_util/general/descr_header.for
	touch $(d0root)/offline_util/general/det_eta.for
	touch $(d0root)/offline_util/hbook/dgn.for
	touch $(d0root)/offline_util/hbook/dgn_begin.for
	touch $(d0root)/offline_util/hbook/dgn_get_prefixed_tags.for
	touch $(d0root)/offline_util/hbook/dgn_get_prefixes.for
	touch $(d0root)/offline_util/hbook/dgn_get_scales.for
	touch $(d0root)/offline_util/hbook/dgn_get_tags.for
	touch $(d0root)/offline_util/hbook/dhdir.for
	touch $(d0root)/offline_util/hbook/dhdirectory.for
	touch $(d0root)/offline_util/hbook/dhpath.for
	touch $(d0root)/offline_util/hbook/dhsetdir.for
	touch $(d0root)/offline_util/general/dhtoc.for
	touch $(d0root)/offline_util/general/diff_phi.for
	touch $(d0root)/offline_util/general/display_on.for
	touch $(d0root)/offline_util/hbook/do_hbook.for
	touch $(d0root)/offline_util/hbook/do_hbook_open.for
	touch $(d0root)/offline_util/hbook/do_hbook_test.for
	touch $(d0root)/offline_util/hbook/do_hbookn.for
	touch $(d0root)/offline_util/hbook/do_hf1d.for
	touch $(d0root)/offline_util/general/drcos.for
	touch $(d0root)/offline_util/general/drop_banks.for
	touch $(d0root)/offline_util/hmatrix/dump_event_quan.for
	touch $(d0root)/offline_util/hmatrix/dump_top_event.for
	touch $(d0root)/offline_util/hmatrix/dump_w_event.for
	touch $(d0root)/offline_util/general/dvmod.for
	touch $(d0root)/offline_util/general/end_hstor.for
	touch $(d0root)/offline_util/endtsk/endet.for
	touch $(d0root)/offline_util/endtsk/enios.for
	touch $(d0root)/offline_util/endtsk/enlog.for
	touch $(d0root)/offline_util/endtsk/enlstp.for
	touch $(d0root)/offline_util/endtsk/enlun.for
	touch $(d0root)/offline_util/endtsk/enprog.for
	touch $(d0root)/offline_util/endtsk/enstoh.for
	touch $(d0root)/offline_util/endtsk/enstp.for
	touch $(d0root)/offline_util/endtsk/enstr.for
	touch $(d0root)/offline_util/endtsk/enzstp.for
	touch $(d0root)/offline_util/general/errmsg_setup.for
	touch $(d0root)/offline_util/general/errout.for
	touch $(d0root)/offline_util/general/eta_zcorr.for
	touch $(d0root)/offline_util/general/etad.for
	touch $(d0root)/offline_util/general/etoeta.for
	touch $(d0root)/offline_util/general/event_control.for
	touch $(d0root)/offline_util/general/evtoft.for
	touch $(d0root)/offline_util/general/extcyl.for
	touch $(d0root)/offline_util/general/file_name.for
	touch $(d0root)/offline_util/general/filename_parse.for
	touch $(d0root)/offline_util/general/filesize.for
	touch $(d0root)/offline_util/general/find_token.for
	touch $(d0root)/offline_util/general/gearot.for
	touch $(d0root)/offline_util/general/get_active_runs.for
	touch $(d0root)/offline_util/general/get_event_type.for
	touch $(d0root)/offline_util/hbook/get_limits.for
	touch $(d0root)/offline_util/general/get_next_token.for
	touch $(d0root)/offline_util/general/get_process_names.for
	touch $(d0root)/offline_util/general/get_run_filters.for
	touch $(d0root)/offline_util/general/get_run_streams.for
	touch $(d0root)/offline_util/general/getdbg.for
	touch $(d0root)/offline_util/general/getofs.for
	touch $(d0root)/offline_util/general/getword.for
	touch $(d0root)/offline_util/general/gtsite.for
	touch $(d0root)/offline_util/general/gtunit.for
	touch $(d0root)/offline_util/hbook/gzhdir.for
	touch $(d0root)/offline_util/hbook/gzhidn.for
	touch $(d0root)/offline_util/hbook/hedit.for
	touch $(d0root)/offline_util/hbook/hfast.for
	touch $(d0root)/offline_util/hbook/hfff1.for
	touch $(d0root)/offline_util/hbook/hinlib.for
	touch $(d0root)/offline_util/endtsk/histit.for
	touch $(d0root)/offline_util/general/hlpusr.for
	touch $(d0root)/offline_util/hmatrix/hmatrix.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_anal.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_anal1.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_array.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_book_banks.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_chisquared.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_darray.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_diag_tran.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_do_averages.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_drop.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_dump_banks.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_dvect.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_event.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_fill.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_fin.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_find.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_get.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_ini.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_make_link_area.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_make_ntuple.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_name.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_predict.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_rz_close.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_rz_get_matrix.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_rz_list.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_rz_make.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_rz_save.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_set.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_set_links.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_set_sizes.for
	touch $(d0root)/offline_util/hmatrix/hmatrix_vect.for
	touch $(d0root)/offline_util/hmatrix/hmindex.for
	touch $(d0root)/offline_util/hbook/hsearc.for
	touch $(d0root)/offline_util/general/ifind2.for
	touch $(d0root)/offline_util/general/in_ieta_window.for
	touch $(d0root)/offline_util/general/in_iphi_window.for
	touch $(d0root)/offline_util/general/inftmn.for
	touch $(d0root)/offline_util/general/inihbk.for
	touch $(d0root)/offline_util/general/intmsg.for
	touch $(d0root)/offline_util/general/intstr.for
	touch $(d0root)/offline_util/general/inzbra.for
	touch $(d0root)/offline_util/general/ioserr.for
	touch $(d0root)/offline_util/general/isort2.for
	touch $(d0root)/offline_util/general/isorter.for
	touch $(d0root)/offline_util/general/itest_bank.for
	touch $(d0root)/offline_util/general/keep_banks.for
	touch $(d0root)/offline_util/general/l1_passed.for
	touch $(d0root)/offline_util/general/l1bit_passed.for
	touch $(d0root)/offline_util/general/l2_passed.for
	touch $(d0root)/offline_util/general/l2_vert.for
	touch $(d0root)/offline_util/general/l2bit_on.for
	touch $(d0root)/offline_util/general/l2bit_passed.for
	touch $(d0root)/offline_util/general/l2bit_set.for
	touch $(d0root)/offline_util/general/l2bit_tried.for
	touch $(d0root)/offline_util/general/l2bit_unbiased.for
	touch $(d0root)/offline_util/general/lchain.for
	touch $(d0root)/offline_util/general/lcpath.for
	touch $(d0root)/offline_util/general/lctree.for
	touch $(d0root)/offline_util/general/locnum.for
	touch $(d0root)/offline_util/general/locstr.for
	touch $(d0root)/offline_util/general/locstr1.for
	touch $(d0root)/offline_util/general/loctd0t.for
	touch $(d0root)/offline_util/least_sq/lsq_anal.for
	touch $(d0root)/offline_util/least_sq/lsq_bkmatrix.for
	touch $(d0root)/offline_util/least_sq/lsq_collect_garbage.for
	touch $(d0root)/offline_util/least_sq/lsq_diag_tran.for
	touch $(d0root)/offline_util/least_sq/lsq_event.for
	touch $(d0root)/offline_util/least_sq/lsq_event_lambda.for
	touch $(d0root)/offline_util/least_sq/lsq_event_lambda1.for
	touch $(d0root)/offline_util/least_sq/lsq_event_lambda2.for
	touch $(d0root)/offline_util/least_sq/lsq_event_lambda3.for
	touch $(d0root)/offline_util/least_sq/lsq_fin.for
	touch $(d0root)/offline_util/least_sq/lsq_get_rows_columns.for
	touch $(d0root)/offline_util/least_sq/lsq_get_val.for
	touch $(d0root)/offline_util/least_sq/lsq_ini.for
	touch $(d0root)/offline_util/least_sq/lsq_make_link_area.for
	touch $(d0root)/offline_util/least_sq/lsq_matrix_add.for
	touch $(d0root)/offline_util/least_sq/lsq_matrix_add_element.for
	touch $(d0root)/offline_util/least_sq/lsq_matrix_copy.for
	touch $(d0root)/offline_util/least_sq/lsq_matrix_delete.for
	touch $(d0root)/offline_util/least_sq/lsq_matrix_diag.for
	touch $(d0root)/offline_util/least_sq/lsq_matrix_exist.for
	touch $(d0root)/offline_util/least_sq/lsq_matrix_fill.for
	touch $(d0root)/offline_util/least_sq/lsq_matrix_get.for
	touch $(d0root)/offline_util/least_sq/lsq_matrix_invert.for
	touch $(d0root)/offline_util/least_sq/lsq_matrix_invert_test.for
	touch $(d0root)/offline_util/least_sq/lsq_matrix_multiply.for
	touch $(d0root)/offline_util/least_sq/lsq_matrix_print.for
	touch $(d0root)/offline_util/least_sq/lsq_matrix_transform.for
	touch $(d0root)/offline_util/least_sq/lsq_matrix_transpose.for
	touch $(d0root)/offline_util/least_sq/lsq_poly_fit.for
	touch $(d0root)/offline_util/least_sq/lsq_poly_init.for
	touch $(d0root)/offline_util/least_sq/lsq_poly_load.for
	touch $(d0root)/offline_util/least_sq/lsq_read_event.for
	touch $(d0root)/offline_util/least_sq/lsq_rz_get.for
	touch $(d0root)/offline_util/least_sq/lsq_rz_make.for
	touch $(d0root)/offline_util/least_sq/lsq_rz_save.for
	touch $(d0root)/offline_util/least_sq/lsq_scalar_multiply.for
	touch $(d0root)/offline_util/least_sq/lsq_set_val.for
	touch $(d0root)/offline_util/least_sq/lsq_test.for
	touch $(d0root)/offline_util/least_sq/lsq_vector_direct_product.for
	touch $(d0root)/offline_util/least_sq/lsq_vector_dot_product.for
	touch $(d0root)/offline_util/general/lstoft.for
	touch $(d0root)/offline_util/general/match_wild.for
	touch $(d0root)/offline_util/general/matchjets.for
	touch $(d0root)/offline_util/general/matrix_invert_1.for
	touch $(d0root)/offline_util/general/matrix_invert_2.for
	touch $(d0root)/offline_util/general/matrix_invert_test1.for
	touch $(d0root)/offline_util/general/matrix_invert_test2.for
	touch $(d0root)/offline_util/general/mrveto.for
	touch $(d0root)/offline_util/general/mxprnd.for
	touch $(d0root)/offline_util/general/mxprnt.for
	touch $(d0root)/offline_util/endtsk/nextid.for
	touch $(d0root)/offline_util/ntuple/ntuple_book.for
	touch $(d0root)/offline_util/ntuple/ntuple_close.for
	touch $(d0root)/offline_util/ntuple/ntuple_end.for
	touch $(d0root)/offline_util/ntuple/ntuple_file_open.for
	touch $(d0root)/offline_util/ntuple/ntuple_file_select.for
	touch $(d0root)/offline_util/ntuple/ntuple_fill.for
	touch $(d0root)/offline_util/ntuple/ntuple_flush.for
	touch $(d0root)/offline_util/ntuple/ntuple_get.for
	touch $(d0root)/offline_util/hbook/ntuple_get_ids.for
	touch $(d0root)/offline_util/hbook/ntuple_get_info.for
	touch $(d0root)/offline_util/ntuple/ntuple_list_info.for
	touch $(d0root)/offline_util/ntuple/ntuple_out.for
	touch $(d0root)/offline_util/ntuple/ntuple_save.for
	touch $(d0root)/offline_util/general/offtim.for
	touch $(d0root)/offline_util/general/oftevt.for
	touch $(d0root)/offline_util/general/oftstr.for
	touch $(d0root)/offline_util/general/oftvax.for
	touch $(d0root)/offline_util/general/opnfil.for
	touch $(d0root)/offline_util/general/outmsg.for
	touch $(d0root)/offline_util/general/output_stream.for
	touch $(d0root)/offline_util/general/output_stream_edit.for
	touch $(d0root)/offline_util/general/parse.for
	touch $(d0root)/offline_util/general/parse_files.for
	touch $(d0root)/offline_util/general/parsfi.for
	touch $(d0root)/offline_util/rotate/paxgrm.for
	touch $(d0root)/offline_util/rotate/paxrot.for
	touch $(d0root)/offline_util/general/pbd_get_next_flag.for
	touch $(d0root)/offline_util/general/pbd_set_all_flags.for
	touch $(d0root)/offline_util/general/pbd_temp_flag.for
	touch $(d0root)/offline_util/general/peta_to_deta.for
	touch $(d0root)/offline_util/general/pm_info.for
	touch $(d0root)/offline_util/general/pm_message.for
	touch $(d0root)/offline_util/rotate/polang.for
	touch $(d0root)/offline_util/general/produc.for
	touch $(d0root)/offline_util/endtsk/prunit.for
	touch $(d0root)/offline_util/general/purge_banks.for
	touch $(d0root)/offline_util/general/qprint.for
	touch $(d0root)/offline_util/general/reorder.for
	touch $(d0root)/offline_util/general/rfind2.for
	touch $(d0root)/offline_util/rotate/rflrot.for
	touch $(d0root)/offline_util/rotate/rotgea.for
	touch $(d0root)/offline_util/rotate/rotmlt.for
	touch $(d0root)/offline_util/rotate/rotpax.for
	touch $(d0root)/offline_util/rotate/rpaxok.for
	touch $(d0root)/offline_util/general/rsort2.for
	touch $(d0root)/offline_util/general/rsorter.for
	touch $(d0root)/offline_util/general/rtest_bank.for
	touch $(d0root)/offline_util/general/run_type.for
	touch $(d0root)/offline_util/general/runsum_info.for
	touch $(d0root)/offline_util/general/scrap.for
	touch $(d0root)/offline_util/general/show_packages.for
	touch $(d0root)/offline_util/general/sort_vec_dist.for
	touch $(d0root)/offline_util/general/spltch.for
	touch $(d0root)/offline_util/general/srtchr.for
	touch $(d0root)/offline_util/general/srtflt.for
	touch $(d0root)/offline_util/general/srtint.for
	touch $(d0root)/offline_util/general/ssopen.for
	touch $(d0root)/offline_util/general/ssunit.for
	touch $(d0root)/offline_util/general/strint.for
	touch $(d0root)/offline_util/general/strip_event.for
	touch $(d0root)/offline_util/general/strip_ini.for
	touch $(d0root)/offline_util/general/stroft.for
	touch $(d0root)/offline_util/general/sub_chr.for
	touch $(d0root)/offline_util/general/swap_token.for
	touch $(d0root)/offline_util/general/swords.for
	touch $(d0root)/offline_util/general/token.for
	touch $(d0root)/offline_util/general/tranup.for
	touch $(d0root)/offline_util/general/trnlnm.for
	touch $(d0root)/offline_util/general/trulen.for
	touch $(d0root)/offline_util/general/ucopydd.for
	touch $(d0root)/offline_util/general/ucopyds.for
	touch $(d0root)/offline_util/general/ucopysd.for
	touch $(d0root)/offline_util/general/upcase.for
	touch $(d0root)/offline_util/general/uquit.for
	touch $(d0root)/offline_util/general/usunit.for
	touch $(d0root)/offline_util/general/value.for
	touch $(d0root)/offline_util/general/values.for
	touch $(d0root)/offline_util/general/valuex.for
	touch $(d0root)/offline_util/general/valusx.for
	touch $(d0root)/offline_util/general/valusy.for
	touch $(d0root)/offline_util/general/vaxoft.for
	touch $(d0root)/offline_util/general/vec_print.for
	touch $(d0root)/offline_util/general/vnumi.for
	touch $(d0root)/offline_util/general/vnumr.for
	touch $(d0root)/offline_util/general/word.for
	touch $(d0root)/offline_util/general/word_bounded_by.for
	touch $(d0root)/offline_util/general/xchker.for
	touch $(d0root)/offline_util/general/zchain.for
