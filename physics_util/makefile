SHELL = /bin/sh
CCFLAGS = -c -dollar -G 0 -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0library)/c_inc
LCCFLAGS = -c -dollar -woff 275 -DD0FLAVOR=SIUNIX -Wf,-XNl8192 -I$(d0test)/unix/source -I$(d0library)/unix/source -I$(d0library)/c_inc
F77FLAGS = -c -nocpp -static -col72 -backslash -G 0 -Olimit 2500 -Wo,-loopunroll,2
DEBUG = -O0 -g2
OPT = -O2
ARFLAGS = crsl
SCRATCH = $(d0library)/tmp/d0librar/mmstomake/physics_util
FLAVOR = SIUNIX
F77 = f77
.IGNORE:
.SUFFIXES:

opt :\
  physics_util.a
physics_util.a : $(d0root)/physics_util/physics_util.a
	@ echo physics_util.a is up to date
$(d0root)/physics_util/physics_util.a:: $(SCRATCH)/physics_util/nothing.nl
$(d0root)/physics_util/physics_util.a::\
  $(d0root)/physics_util/physics_util.a(amoeba.o)\
  $(d0root)/physics_util/physics_util.a(auto_fmt.o)\
  $(d0root)/physics_util/physics_util.a(b1m_mrjet332.o)\
  $(d0root)/physics_util/physics_util.a(b1m_sjets455.o)\
  $(d0root)/physics_util/physics_util.a(cacl_esum.o)\
  $(d0root)/physics_util/physics_util.a(cal_ester389.o)\
  $(d0root)/physics_util/physics_util.a(cal_jets.o)\
  $(d0root)/physics_util/physics_util.a(catalcorr910.o)\
  $(d0root)/physics_util/physics_util.a(cd_veinfo497.o)\
  $(d0root)/physics_util/physics_util.a(cleanjunk459.o)\
  $(d0root)/physics_util/physics_util.a(cm3pos_fix.o)\
  $(d0root)/physics_util/physics_util.a(correct_jets.o)\
  $(d0root)/physics_util/physics_util.a(corremove351.o)\
  $(d0root)/physics_util/physics_util.a(correiseu051.o)\
  $(d0root)/physics_util/physics_util.a(correions122.o)\
  $(d0root)/physics_util/physics_util.a(corre_ooc713.o)\
  $(d0root)/physics_util/physics_util.a(corresion686.o)\
  $(d0root)/physics_util/physics_util.a(corretors761.o)\
  $(d0root)/physics_util/physics_util.a(correidth936.o)\
  $(d0root)/physics_util/physics_util.a(correct_met.o)\
  $(d0root)/physics_util/physics_util.a(correixup254.o)\
  $(d0root)/physics_util/physics_util.a(corretlum927.o)\
  $(d0root)/physics_util/physics_util.a(d0dad_list.o)\
  $(d0root)/physics_util/physics_util.a(d0hcdir.o)\
  $(d0root)/physics_util/physics_util.a(d0hchd.o)\
  $(d0root)/physics_util/physics_util.a(d0hcom.o)\
  $(d0root)/physics_util/physics_util.a(d0hend.o)\
  $(d0root)/physics_util/physics_util.a(d0hindex.o)\
  $(d0root)/physics_util/physics_util.a(d0hinx.o)\
  $(d0root)/physics_util/physics_util.a(d0hlas.o)\
  $(d0root)/physics_util/physics_util.a(d0hmaklas.o)\
  $(d0root)/physics_util/physics_util.a(d0hpld.o)\
  $(d0root)/physics_util/physics_util.a(d0hple.o)\
  $(d0root)/physics_util/physics_util.a(d0hplt.o)\
  $(d0root)/physics_util/physics_util.a(d0hplatch541.o)\
  $(d0root)/physics_util/physics_util.a(d0hplt_event.o)\
  $(d0root)/physics_util/physics_util.a(d0htyp.o)\
  $(d0root)/physics_util/physics_util.a(d0huph.o)\
  $(d0root)/physics_util/physics_util.a(dbank.o)\
  $(d0root)/physics_util/physics_util.a(dcd_chfrm.o)\
  $(d0root)/physics_util/physics_util.a(deta_to_peta.o)\
  $(d0root)/physics_util/physics_util.a(dmpdef.o)\
  $(d0root)/physics_util/physics_util.a(dot.o)\
  $(d0root)/physics_util/physics_util.a(drop__alg357.o)\
  $(d0root)/physics_util/physics_util.a(duplilter347.o)\
  $(d0root)/physics_util/physics_util.a(dzfor1.o)\
  $(d0root)/physics_util/physics_util.a(el2_b.o)\
  $(d0root)/physics_util/physics_util.a(elf.o)\
  $(d0root)/physics_util/physics_util.a(elf_med.o)\
  $(d0root)/physics_util/physics_util.a(elf_w.o)\
  $(d0root)/physics_util/physics_util.a(elf_z.o)\
  $(d0root)/physics_util/physics_util.a(em_etzv.o)\
  $(d0root)/physics_util/physics_util.a(em_fits_mask.o)\
  $(d0root)/physics_util/physics_util.a(em_in_jet.o)\
  $(d0root)/physics_util/physics_util.a(emag_jets.o)\
  $(d0root)/physics_util/physics_util.a(emfix.o)\
  $(d0root)/physics_util/physics_util.a(emphifix.o)\
  $(d0root)/physics_util/physics_util.a(esum_ress423.o)\
  $(d0root)/physics_util/physics_util.a(event_list.o)\
  $(d0root)/physics_util/physics_util.a(ez_sepack718.o)\
  $(d0root)/physics_util/physics_util.a(ezbank.o)\
  $(d0root)/physics_util/physics_util.a(fake_date633.o)\
  $(d0root)/physics_util/physics_util.a(fill_cdtk.o)\
  $(d0root)/physics_util/physics_util.a(fill_nt.o)\
  $(d0root)/physics_util/physics_util.a(fill_ions945.o)\
  $(d0root)/physics_util/physics_util.a(filter_bits.o)\
  $(d0root)/physics_util/physics_util.a(filtset.o)\
  $(d0root)/physics_util/physics_util.a(find_st_b469.o)\
  $(d0root)/physics_util/physics_util.a(find_st_c470.o)\
  $(d0root)/physics_util/physics_util.a(find_st_e472.o)\
  $(d0root)/physics_util/physics_util.a(find_t_mu597.o)\
  $(d0root)/physics_util/physics_util.a(find_ject002.o)\
  $(d0root)/physics_util/physics_util.a(find__tau701.o)\
  $(d0root)/physics_util/physics_util.a(find_wlnu.o)\
  $(d0root)/physics_util/physics_util.a(find_wlnu2.o)\
  $(d0root)/physics_util/physics_util.a(find_wlnu2d.o)\
  $(d0root)/physics_util/physics_util.a(find_wlnud.o)\
  $(d0root)/physics_util/physics_util.a(fix_jet_et.o)\
  $(d0root)/physics_util/physics_util.a(flusher.o)\
  $(d0root)/physics_util/physics_util.a(frame_menadd.o)\
  $(d0root)/physics_util/physics_util.a(fwmoment.o)\
  $(d0root)/physics_util/physics_util.a(get_caph_alg.o)\
  $(d0root)/physics_util/physics_util.a(get_eance462.o)\
  $(d0root)/physics_util/physics_util.a(get_erons400.o)\
  $(d0root)/physics_util/physics_util.a(get_idren784.o)\
  $(d0root)/physics_util/physics_util.a(get_irton603.o)\
  $(d0root)/physics_util/physics_util.a(get_idren001.o)\
  $(d0root)/physics_util/physics_util.a(get_irton820.o)\
  $(d0root)/physics_util/physics_util.a(get_idren786.o)\
  $(d0root)/physics_util/physics_util.a(get_irton605.o)\
  $(d0root)/physics_util/physics_util.a(get_idren791.o)\
  $(d0root)/physics_util/physics_util.a(get_irton610.o)\
  $(d0root)/physics_util/physics_util.a(get_jets.o)\
  $(d0root)/physics_util/physics_util.a(get_linfo358.o)\
  $(d0root)/physics_util/physics_util.a(get_muons.o)\
  $(d0root)/physics_util/physics_util.a(get_nicle819.o)\
  $(d0root)/physics_util/physics_util.a(get_p2neut.o)\
  $(d0root)/physics_util/physics_util.a(get_preco030.o)\
  $(d0root)/physics_util/physics_util.a(get_photons.o)\
  $(d0root)/physics_util/physics_util.a(get_ptors729.o)\
  $(d0root)/physics_util/physics_util.a(get_qtent831.o)\
  $(d0root)/physics_util/physics_util.a(get_s_div400.o)\
  $(d0root)/physics_util/physics_util.a(get_tags.o)\
  $(d0root)/physics_util/physics_util.a(get_t_run825.o)\
  $(d0root)/physics_util/physics_util.a(getlum.o)\
  $(d0root)/physics_util/physics_util.a(getlum_corr.o)\
  $(d0root)/physics_util/physics_util.a(getzeb.o)\
  $(d0root)/physics_util/physics_util.a(good_tron390.o)\
  $(d0root)/physics_util/physics_util.a(good_heck582.o)\
  $(d0root)/physics_util/physics_util.a(good_icle382.o)\
  $(d0root)/physics_util/physics_util.a(good_photon.o)\
  $(d0root)/physics_util/physics_util.a(good_mu1b754.o)\
  $(d0root)/physics_util/physics_util.a(gtcdtk.o)\
  $(d0root)/physics_util/physics_util.a(gtchfr.o)\
  $(d0root)/physics_util/physics_util.a(gtzfrm.o)\
  $(d0root)/physics_util/physics_util.a(handler.o)\
  $(d0root)/physics_util/physics_util.a(hisprt.o)\
  $(d0root)/physics_util/physics_util.a(hot_c_jet580.o)\
  $(d0root)/physics_util/physics_util.a(hplotatch504.o)\
  $(d0root)/physics_util/physics_util.a(hplot_menu.o)\
  $(d0root)/physics_util/physics_util.a(hplotmenu624.o)\
  $(d0root)/physics_util/physics_util.a(invarmass513.o)\
  $(d0root)/physics_util/physics_util.a(is_datafile.o)\
  $(d0root)/physics_util/physics_util.a(isol_lect505.o)\
  $(d0root)/physics_util/physics_util.a(jet_ccorr382.o)\
  $(d0root)/physics_util/physics_util.a(jet_corr.o)\
  $(d0root)/physics_util/physics_util.a(jet_ecale355.o)\
  $(d0root)/physics_util/physics_util.a(jet_et_true.o)\
  $(d0root)/physics_util/physics_util.a(jet_info.o)\
  $(d0root)/physics_util/physics_util.a(jet_ocorr129.o)\
  $(d0root)/physics_util/physics_util.a(jet_uctor838.o)\
  $(d0root)/physics_util/physics_util.a(jutlfl.o)\
  $(d0root)/physics_util/physics_util.a(l15cafill579.o)\
  $(d0root)/physics_util/physics_util.a(l1esum_evt.o)\
  $(d0root)/physics_util/physics_util.a(l1esum_muon.o)\
  $(d0root)/physics_util/physics_util.a(l1esum_trgr.o)\
  $(d0root)/physics_util/physics_util.a(l1esum_trgr2.o)\
  $(d0root)/physics_util/physics_util.a(lo_calc.o)\
  $(d0root)/physics_util/physics_util.a(lo_dsig_dt.o)\
  $(d0root)/physics_util/physics_util.a(lo_ds_sel477.o)\
  $(d0root)/physics_util/physics_util.a(lo_in_r.o)\
  $(d0root)/physics_util/physics_util.a(lo_main_r.o)\
  $(d0root)/physics_util/physics_util.a(lo_nt.o)\
  $(d0root)/physics_util/physics_util.a(lo_out.o)\
  $(d0root)/physics_util/physics_util.a(lqnn.o)\
  $(d0root)/physics_util/physics_util.a(lqnue.o)\
  $(d0root)/physics_util/physics_util.a(make_nt.o)\
  $(d0root)/physics_util/physics_util.a(make_udst.o)\
  $(d0root)/physics_util/physics_util.a(make_uint.o)\
  $(d0root)/physics_util/physics_util.a(matchdata553.o)\
  $(d0root)/physics_util/physics_util.a(mc_et_corr.o)\
  $(d0root)/physics_util/physics_util.a(menu_d0hplt.o)\
  $(d0root)/physics_util/physics_util.a(minbias.o)\
  $(d0root)/physics_util/physics_util.a(miss_et_corr.o)\
  $(d0root)/physics_util/physics_util.a(montedata686.o)\
  $(d0root)/physics_util/physics_util.a(mpf_fjets384.o)\
  $(d0root)/physics_util/physics_util.a(mpf_jonse725.o)\
  $(d0root)/physics_util/physics_util.a(mpf_j_fin137.o)\
  $(d0root)/physics_util/physics_util.a(mpf_j_ini140.o)\
  $(d0root)/physics_util/physics_util.a(mpf_jetcorr.o)\
  $(d0root)/physics_util/physics_util.a(mpf_jets.o)\
  $(d0root)/physics_util/physics_util.a(mpf_lcorr121.o)\
  $(d0root)/physics_util/physics_util.a(mpf_mcorr492.o)\
  $(d0root)/physics_util/physics_util.a(mpf_photons.o)\
  $(d0root)/physics_util/physics_util.a(mpf_r_fit725.o)\
  $(d0root)/physics_util/physics_util.a(mpf_sim.o)\
  $(d0root)/physics_util/physics_util.a(mpf_z_boson.o)\
  $(d0root)/physics_util/physics_util.a(msp_mlect623.o)\
  $(d0root)/physics_util/physics_util.a(mu1_b.o)\
  $(d0root)/physics_util/physics_util.a(mu1_filt.o)\
  $(d0root)/physics_util/physics_util.a(mu1_wzt.o)\
  $(d0root)/physics_util/physics_util.a(mu2_b.o)\
  $(d0root)/physics_util/physics_util.a(mu2_wzt.o)\
  $(d0root)/physics_util/physics_util.a(mu3_b.o)\
  $(d0root)/physics_util/physics_util.a(mu_b.o)\
  $(d0root)/physics_util/physics_util.a(mu_unbiased.o)\
  $(d0root)/physics_util/physics_util.a(muj_b.o)\
  $(d0root)/physics_util/physics_util.a(mujettion747.o)\
  $(d0root)/physics_util/physics_util.a(mujetlect730.o)\
  $(d0root)/physics_util/physics_util.a(mulq_lect734.o)\
  $(d0root)/physics_util/physics_util.a(mulq_ight373.o)\
  $(d0root)/physics_util/physics_util.a(multi_filter.o)\
  $(d0root)/physics_util/physics_util.a(multitool706.o)\
  $(d0root)/physics_util/physics_util.a(multirun1191.o)\
  $(d0root)/physics_util/physics_util.a(muon_t_np509.o)\
  $(d0root)/physics_util/physics_util.a(muon_calc619.o)\
  $(d0root)/physics_util/physics_util.a(new_gam_jet.o)\
  $(d0root)/physics_util/physics_util.a(np_clean_tau.o)\
  $(d0root)/physics_util/physics_util.a(np_dioose926.o)\
  $(d0root)/physics_util/physics_util.a(np_diight924.o)\
  $(d0root)/physics_util/physics_util.a(np_had.o)\
  $(d0root)/physics_util/physics_util.a(np_lep.o)\
  $(d0root)/physics_util/physics_util.a(np_lq_2em.o)\
  $(d0root)/physics_util/physics_util.a(np_lqight542.o)\
  $(d0root)/physics_util/physics_util.a(np_lq_enu.o)\
  $(d0root)/physics_util/physics_util.a(np_lslect400.o)\
  $(d0root)/physics_util/physics_util.a(np_lss_tight.o)\
  $(d0root)/physics_util/physics_util.a(np_lsoose057.o)\
  $(d0root)/physics_util/physics_util.a(np_lsight055.o)\
  $(d0root)/physics_util/physics_util.a(np_met.o)\
  $(d0root)/physics_util/physics_util.a(np_msp.o)\
  $(d0root)/physics_util/physics_util.a(np_msheck589.o)\
  $(d0root)/physics_util/physics_util.a(np_mslect719.o)\
  $(d0root)/physics_util/physics_util.a(np_mulq.o)\
  $(d0root)/physics_util/physics_util.a(np_muight413.o)\
  $(d0root)/physics_util/physics_util.a(np_munu.o)\
  $(d0root)/physics_util/physics_util.a(np_scalar.o)\
  $(d0root)/physics_util/physics_util.a(np_scight596.o)\
  $(d0root)/physics_util/physics_util.a(np_sqgl.o)\
  $(d0root)/physics_util/physics_util.a(np_sqight405.o)\
  $(d0root)/physics_util/physics_util.a(np_tau.o)\
  $(d0root)/physics_util/physics_util.a(np_wright.o)\
  $(d0root)/physics_util/physics_util.a(np_wright627.o)\
  $(d0root)/physics_util/physics_util.a(object_muon1.o)\
  $(d0root)/physics_util/physics_util.a(ordx.o)\
  $(d0root)/physics_util/physics_util.a(partilect597.o)\
  $(d0root)/physics_util/physics_util.a(phi_dence461.o)\
  $(d0root)/physics_util/physics_util.a(phys_cor.o)\
  $(d0root)/physics_util/physics_util.a(planarity.o)\
  $(d0root)/physics_util/physics_util.a(prnt_bank.o)\
  $(d0root)/physics_util/physics_util.a(ptfix.o)\
  $(d0root)/physics_util/physics_util.a(ptopt.o)\
  $(d0root)/physics_util/physics_util.a(qcd_eta_bias.o)\
  $(d0root)/physics_util/physics_util.a(qcd_eesol375.o)\
  $(d0root)/physics_util/physics_util.a(qcd_fring606.o)\
  $(d0root)/physics_util/physics_util.a(qcd_f_bit477.o)\
  $(d0root)/physics_util/physics_util.a(qcd_finit008.o)\
  $(d0root)/physics_util/physics_util.a(qcd_fgger914.o)\
  $(d0root)/physics_util/physics_util.a(qcd_gamma.o)\
  $(d0root)/physics_util/physics_util.a(qcd_gap.o)\
  $(d0root)/physics_util/physics_util.a(qcd_get_mask.o)\
  $(d0root)/physics_util/physics_util.a(qcd_jtion915.o)\
  $(d0root)/physics_util/physics_util.a(qcd_jjj.o)\
  $(d0root)/physics_util/physics_util.a(qcd_make_dst.o)\
  $(d0root)/physics_util/physics_util.a(qcd_nol.o)\
  $(d0root)/physics_util/physics_util.a(qcd_ntr.o)\
  $(d0root)/physics_util/physics_util.a(qcd_qjt.o)\
  $(d0root)/physics_util/physics_util.a(qcd_sd.o)\
  $(d0root)/physics_util/physics_util.a(qcd_sta.o)\
  $(d0root)/physics_util/physics_util.a(qcdjets.o)\
  $(d0root)/physics_util/physics_util.a(recalc_muon.o)\
  $(d0root)/physics_util/physics_util.a(reco_esum.o)\
  $(d0root)/physics_util/physics_util.a(reco_fill493.o)\
  $(d0root)/physics_util/physics_util.a(reco_lect039.o)\
  $(d0root)/physics_util/physics_util.a(run_select.o)\
  $(d0root)/physics_util/physics_util.a(sel_ejets371.o)\
  $(d0root)/physics_util/physics_util.a(set_caph_alg.o)\
  $(d0root)/physics_util/physics_util.a(set_jet.o)\
  $(d0root)/physics_util/physics_util.a(set_ttnum131.o)\
  $(d0root)/physics_util/physics_util.a(sm_jector157.o)\
  $(d0root)/physics_util/physics_util.a(sort_descend.o)\
  $(d0root)/physics_util/physics_util.a(sort_vect.o)\
  $(d0root)/physics_util/physics_util.a(sortbanks.o)\
  $(d0root)/physics_util/physics_util.a(sphericity.o)\
  $(d0root)/physics_util/physics_util.a(sphericity0.o)\
  $(d0root)/physics_util/physics_util.a(splitmask408.o)\
  $(d0root)/physics_util/physics_util.a(ss_mu_check.o)\
  $(d0root)/physics_util/physics_util.a(ss_mu_chk.o)\
  $(d0root)/physics_util/physics_util.a(ss_mu_sel.o)\
  $(d0root)/physics_util/physics_util.a(ss_mu_select.o)\
  $(d0root)/physics_util/physics_util.a(stream_ejets.o)\
  $(d0root)/physics_util/physics_util.a(tag_mlect382.o)\
  $(d0root)/physics_util/physics_util.a(tau_filter.o)\
  $(d0root)/physics_util/physics_util.a(threelter610.o)\
  $(d0root)/physics_util/physics_util.a(top_bkg.o)\
  $(d0root)/physics_util/physics_util.a(top_diem.o)\
  $(d0root)/physics_util/physics_util.a(top_ee.o)\
  $(d0root)/physics_util/physics_util.a(top_ejbkg.o)\
  $(d0root)/physics_util/physics_util.a(top_ejet.o)\
  $(d0root)/physics_util/physics_util.a(top_ejsig.o)\
  $(d0root)/physics_util/physics_util.a(top_emu.o)\
  $(d0root)/physics_util/physics_util.a(top_eanup829.o)\
  $(d0root)/physics_util/physics_util.a(top_jets.o)\
  $(d0root)/physics_util/physics_util.a(top_muj.o)\
  $(d0root)/physics_util/physics_util.a(top_mujet.o)\
  $(d0root)/physics_util/physics_util.a(top_mumu.o)\
  $(d0root)/physics_util/physics_util.a(top_sgl.o)\
  $(d0root)/physics_util/physics_util.a(top_tbbar383.o)\
  $(d0root)/physics_util/physics_util.a(top_top_ee.o)\
  $(d0root)/physics_util/physics_util.a(top_top_ejet.o)\
  $(d0root)/physics_util/physics_util.a(top_top_emu.o)\
  $(d0root)/physics_util/physics_util.a(top_top_jets.o)\
  $(d0root)/physics_util/physics_util.a(top_tujet427.o)\
  $(d0root)/physics_util/physics_util.a(top_top_mumu.o)\
  $(d0root)/physics_util/physics_util.a(top_top_sgl.o)\
  $(d0root)/physics_util/physics_util.a(topup.o)\
  $(d0root)/physics_util/physics_util.a(topup_err405.o)\
  $(d0root)/physics_util/physics_util.a(transmass642.o)\
  $(d0root)/physics_util/physics_util.a(triggheck785.o)\
  $(d0root)/physics_util/physics_util.a(tsum_fill.o)\
  $(d0root)/physics_util/physics_util.a(tsum_names.o)\
  $(d0root)/physics_util/physics_util.a(twojelter420.o)\
  $(d0root)/physics_util/physics_util.a(ucshfl.o)\
  $(d0root)/physics_util/physics_util.a(ucwxfl.o)\
  $(d0root)/physics_util/physics_util.a(udst.o)\
  $(d0root)/physics_util/physics_util.a(udst_roup632.o)\
  $(d0root)/physics_util/physics_util.a(udst_pack249.o)\
  $(d0root)/physics_util/physics_util.a(udst_pack476.o)\
  $(d0root)/physics_util/physics_util.a(udst_fin.o)\
  $(d0root)/physics_util/physics_util.a(udst_bank387.o)\
  $(d0root)/physics_util/physics_util.a(udst_jets413.o)\
  $(d0root)/physics_util/physics_util.a(udst_pelc395.o)\
  $(d0root)/physics_util/physics_util.a(udst_pmuo424.o)\
  $(d0root)/physics_util/physics_util.a(udst_pnut430.o)\
  $(d0root)/physics_util/physics_util.a(udst_ptau417.o)\
  $(d0root)/physics_util/physics_util.a(udst_vtxt445.o)\
  $(d0root)/physics_util/physics_util.a(udst_cash383.o)\
  $(d0root)/physics_util/physics_util.a(udst_cawx403.o)\
  $(d0root)/physics_util/physics_util.a(udst_clus617.o)\
  $(d0root)/physics_util/physics_util.a(udst_glob388.o)\
  $(d0root)/physics_util/physics_util.a(udst_head370.o)\
  $(d0root)/physics_util/physics_util.a(udst_jets406.o)\
  $(d0root)/physics_util/physics_util.a(udst_jnep397.o)\
  $(d0root)/physics_util/physics_util.a(udst__met596.o)\
  $(d0root)/physics_util/physics_util.a(udst_gpol608.o)\
  $(d0root)/physics_util/physics_util.a(udst_muon415.o)\
  $(d0root)/physics_util/physics_util.a(udst_link913.o)\
  $(d0root)/physics_util/physics_util.a(udst_pnu1356.o)\
  $(d0root)/physics_util/physics_util.a(udst_pnut423.o)\
  $(d0root)/physics_util/physics_util.a(udst_ptau410.o)\
  $(d0root)/physics_util/physics_util.a(udst_sahh388.o)\
  $(d0root)/physics_util/physics_util.a(udst_link026.o)\
  $(d0root)/physics_util/physics_util.a(udst_trak402.o)\
  $(d0root)/physics_util/physics_util.a(udst_sion742.o)\
  $(d0root)/physics_util/physics_util.a(udst_vert417.o)\
  $(d0root)/physics_util/physics_util.a(udst_link942.o)\
  $(d0root)/physics_util/physics_util.a(udst_vfit409.o)\
  $(d0root)/physics_util/physics_util.a(udst_vtxt438.o)\
  $(d0root)/physics_util/physics_util.a(udst_link963.o)\
  $(d0root)/physics_util/physics_util.a(udst_ini.o)\
  $(d0root)/physics_util/physics_util.a(udst_out.o)\
  $(d0root)/physics_util/physics_util.a(udst_tag.o)\
  $(d0root)/physics_util/physics_util.a(udst_to_dst.o)\
  $(d0root)/physics_util/physics_util.a(udst_fill724.o)\
  $(d0root)/physics_util/physics_util.a(udst_bank231.o)\
  $(d0root)/physics_util/physics_util.a(udst_init737.o)\
  $(d0root)/physics_util/physics_util.a(udst_sion064.o)\
  $(d0root)/physics_util/physics_util.a(udstfl.o)\
  $(d0root)/physics_util/physics_util.a(uint_to_dst.o)\
  $(d0root)/physics_util/physics_util.a(uncorbank031.o)\
  $(d0root)/physics_util/physics_util.a(uni_filter.o)\
  $(d0root)/physics_util/physics_util.a(unix_tall857.o)\
  $(d0root)/physics_util/physics_util.a(utag_udst517.o)\
  $(d0root)/physics_util/physics_util.a(utagfl.o)\
  $(d0root)/physics_util/physics_util.a(vbrowse.o)\
  $(d0root)/physics_util/physics_util.a(verify_zebra.o)\
  $(d0root)/physics_util/physics_util.a(vertelter421.o)\
  $(d0root)/physics_util/physics_util.a(zb_smg_pn1.o)\
  $(d0root)/physics_util/physics_util.a(zb_smg_pn2.o)\
  $(d0root)/physics_util/physics_util.a(zbank_edit.o)\
  $(d0root)/physics_util/physics_util.a(zbank_tag366.o)\
  $(d0root)/physics_util/physics_util.a(zbank_ntuple.o)\
  $(d0root)/physics_util/physics_util.a(zbank_test.o)\
  $(d0root)/physics_util/physics_util.a(zbank_val.o)
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a1/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a2/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a3/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a4/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a5/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a6/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a7/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a8/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a9/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a10/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a11/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a12/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a13/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a14/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a15/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a16/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a17/*.o 2> /dev/null`
	ar $(ARFLAGS) $(d0root)/physics_util/physics_util.a `ls $(SCRATCH)/physics_util/a18/*.o 2> /dev/null`
	rmdirp $(SCRATCH)/physics_util
$(SCRATCH)/physics_util/nothing.nl:
	rmdirp $(SCRATCH)/physics_util
	mkdirp $(SCRATCH)/physics_util/a1
	mkdirp $(SCRATCH)/physics_util/a2
	mkdirp $(SCRATCH)/physics_util/a3
	mkdirp $(SCRATCH)/physics_util/a4
	mkdirp $(SCRATCH)/physics_util/a5
	mkdirp $(SCRATCH)/physics_util/a6
	mkdirp $(SCRATCH)/physics_util/a7
	mkdirp $(SCRATCH)/physics_util/a8
	mkdirp $(SCRATCH)/physics_util/a9
	mkdirp $(SCRATCH)/physics_util/a10
	mkdirp $(SCRATCH)/physics_util/a11
	mkdirp $(SCRATCH)/physics_util/a12
	mkdirp $(SCRATCH)/physics_util/a13
	mkdirp $(SCRATCH)/physics_util/a14
	mkdirp $(SCRATCH)/physics_util/a15
	mkdirp $(SCRATCH)/physics_util/a16
	mkdirp $(SCRATCH)/physics_util/a17
	mkdirp $(SCRATCH)/physics_util/a18
$(d0root)/physics_util/physics_util.a(amoeba.o):\
  $(d0root)/physics_util/general/amoeba.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/amoeba.f ;\
	mv amoeba.o $(SCRATCH)/physics_util/a1/amoeba.o ;\
	)
$(d0root)/physics_util/physics_util.a(auto_fmt.o):\
  $(d0root)/physics_util/dbank/auto_fmt.f\
  $(d0library)/inc/autof.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebq.inc\
  $(d0library)/inc/mzca.inc\
  $(d0library)/inc/mzcb.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/dbank/auto_fmt.f ;\
	mv auto_fmt.o $(SCRATCH)/physics_util/a1/auto_fmt.o ;\
	)
$(d0root)/physics_util/physics_util.a(b1m_mrjet332.o):\
  $(d0root)/physics_util/filters/b1m_munearjet.f\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/b1m_munearjet.f ;\
	mv b1m_munearjet.o $(SCRATCH)/physics_util/a1/b1m_mrjet332.o ;\
	)
$(d0root)/physics_util/physics_util.a(b1m_sjets455.o):\
  $(d0root)/physics_util/filters/b1m_setup_jets.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izisae.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/b1m_setup_jets.f ;\
	mv b1m_setup_jets.o $(SCRATCH)/physics_util/a1/b1m_sjets455.o ;\
	)
$(d0root)/physics_util/physics_util.a(cacl_esum.o):\
  $(d0root)/physics_util/general/cacl_esum.f\
  $(d0library)/params/esum.params\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izvert.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/cacl_esum.f ;\
	mv cacl_esum.o $(SCRATCH)/physics_util/a1/cacl_esum.o ;\
	)
$(d0root)/physics_util/physics_util.a(cal_ester389.o):\
  $(d0root)/physics_util/general/cal_emcluster.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/cal_emcluster.f ;\
	mv cal_emcluster.o $(SCRATCH)/physics_util/a1/cal_ester389.o ;\
	)
$(d0root)/physics_util/physics_util.a(cal_jets.o):\
  $(d0root)/physics_util/general/cal_jets.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izjets.link\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/cal_jets.f ;\
	mv cal_jets.o $(SCRATCH)/physics_util/a1/cal_jets.o ;\
	)
$(d0root)/physics_util/physics_util.a(catalcorr910.o):\
  $(d0root)/physics_util/general/catalogue_jet_corr.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/catalogue_jet_corr.f ;\
	mv catalogue_jet_corr.o $(SCRATCH)/physics_util/a1/catalcorr910.o ;\
	)
$(d0root)/physics_util/physics_util.a(cd_veinfo497.o):\
  $(d0root)/physics_util/general/cd_vertex_info.f\
  $(d0library)/params/vertex_multi.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/cd_vertex_info.f ;\
	mv cd_vertex_info.o $(SCRATCH)/physics_util/a1/cd_veinfo497.o ;\
	)
$(d0root)/physics_util/physics_util.a(cleanjunk459.o):\
  $(d0root)/physics_util/general/clean_cal_junk.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/clean_cal_junk.f ;\
	mv clean_cal_junk.o $(SCRATCH)/physics_util/a1/cleanjunk459.o ;\
	)
$(d0root)/physics_util/physics_util.a(cm3pos_fix.o):\
  $(d0root)/physics_util/filters/cm3pos_fix.f\
  $(d0library)/inc/cemprf.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/cal_offline.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/cm3pos_fix.f ;\
	mv cm3pos_fix.o $(SCRATCH)/physics_util/a1/cm3pos_fix.o ;\
	)
$(d0root)/physics_util/physics_util.a(correct_jets.o):\
  $(d0root)/physics_util/general/correct_jets.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcaph.link\
  $(d0library)/links/izjets.link\
  $(d0library)/params/caph.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/correct_jets.f ;\
	mv correct_jets.o $(SCRATCH)/physics_util/a1/correct_jets.o ;\
	)
$(d0root)/physics_util/physics_util.a(corremove351.o):\
  $(d0root)/physics_util/general/correct_jets_em_remove.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/correct_jets_em_remove.f ;\
	mv correct_jets_em_remove.o $(SCRATCH)/physics_util/a1/corremove351.o ;\
	)
$(d0root)/physics_util/physics_util.a(correiseu051.o):\
  $(d0root)/physics_util/general/correct_jets_noiseu.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/correct_jets_noiseu.f ;\
	mv correct_jets_noiseu.o $(SCRATCH)/physics_util/a1/correiseu051.o ;\
	)
$(d0root)/physics_util/physics_util.a(correions122.o):\
  $(d0root)/physics_util/general/correct_jets_num_interactions.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/correct_jets_num_interactions.f ;\
	mv correct_jets_num_interactions.o $(SCRATCH)/physics_util/a1/correions122.o ;\
	)
$(d0root)/physics_util/physics_util.a(corre_ooc713.o):\
  $(d0root)/physics_util/general/correct_jets_ooc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/correct_jets_ooc.f ;\
	mv correct_jets_ooc.o $(SCRATCH)/physics_util/a1/corre_ooc713.o ;\
	)
$(d0root)/physics_util/physics_util.a(corresion686.o):\
  $(d0root)/physics_util/general/correct_jets_reco_version.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/correct_jets_reco_version.f ;\
	mv correct_jets_reco_version.o $(SCRATCH)/physics_util/a1/corresion686.o ;\
	)
$(d0root)/physics_util/physics_util.a(corretors761.o):\
  $(d0root)/physics_util/general/correct_jets_scale_factors.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/correct_jets_scale_factors.f ;\
	mv correct_jets_scale_factors.o $(SCRATCH)/physics_util/a1/corretors761.o ;\
	)
$(d0root)/physics_util/physics_util.a(correidth936.o):\
  $(d0root)/physics_util/general/correct_jets_width.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/correct_jets_width.f ;\
	mv correct_jets_width.o $(SCRATCH)/physics_util/a1/correidth936.o ;\
	)
$(d0root)/physics_util/physics_util.a(correct_met.o):\
  $(d0root)/physics_util/general/correct_met.f\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izreco.link\
  $(d0library)/links/izjnep.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/correct_met.f ;\
	mv correct_met.o $(SCRATCH)/physics_util/a1/correct_met.o ;\
	)
$(d0root)/physics_util/physics_util.a(correixup254.o):\
  $(d0root)/physics_util/general/correct_met_jet_fixup.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/correct_met_jet_fixup.f ;\
	mv correct_met_jet_fixup.o $(SCRATCH)/physics_util/a2/correixup254.o ;\
	)
$(d0root)/physics_util/physics_util.a(corretlum927.o):\
  $(d0root)/physics_util/general/correct_old_getlum.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/correct_old_getlum.f ;\
	mv correct_old_getlum.o $(SCRATCH)/physics_util/a2/corretlum927.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0dad_list.o):\
  $(d0root)/physics_util/filters/d0dad_list.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/d0dad_list.f ;\
	mv d0dad_list.o $(SCRATCH)/physics_util/a2/d0dad_list.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0hcdir.o):\
  $(d0root)/physics_util/d0hplt/d0hcdir.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/d0hcdir.f ;\
	mv d0hcdir.o $(SCRATCH)/physics_util/a2/d0hcdir.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0hchd.o):\
  $(d0root)/physics_util/d0hplt/d0hchd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/d0hchd.f ;\
	mv d0hchd.o $(SCRATCH)/physics_util/a2/d0hchd.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0hcom.o):\
  $(d0root)/physics_util/d0hplt/d0hcom.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/d0hcom.f ;\
	mv d0hcom.o $(SCRATCH)/physics_util/a2/d0hcom.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0hend.o):\
  $(d0root)/physics_util/d0hplt/d0hend.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/d0hend.f ;\
	mv d0hend.o $(SCRATCH)/physics_util/a2/d0hend.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0hindex.o):\
  $(d0root)/physics_util/d0hplt/d0hindex.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/d0hindex.f ;\
	mv d0hindex.o $(SCRATCH)/physics_util/a2/d0hindex.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0hinx.o):\
  $(d0root)/physics_util/d0hplt/d0hinx.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/d0hinx.f ;\
	mv d0hinx.o $(SCRATCH)/physics_util/a2/d0hinx.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0hlas.o):\
  $(d0root)/physics_util/d0hplt/d0hlas.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/d0hlas.f ;\
	mv d0hlas.o $(SCRATCH)/physics_util/a2/d0hlas.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0hmaklas.o):\
  $(d0root)/physics_util/d0hplt/d0hmaklas.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/d0hmaklas.f ;\
	mv d0hmaklas.o $(SCRATCH)/physics_util/a2/d0hmaklas.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0hpld.o):\
  $(d0root)/physics_util/d0hplt/d0hpld.f\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/d0hpld.f ;\
	mv d0hpld.o $(SCRATCH)/physics_util/a2/d0hpld.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0hple.o):\
  $(d0root)/physics_util/d0hplt/d0hple.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/d0hple.f ;\
	mv d0hple.o $(SCRATCH)/physics_util/a2/d0hple.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0hplt.o):\
  $(d0root)/physics_util/d0hplt/d0hplt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/d0hplt.f ;\
	mv d0hplt.o $(SCRATCH)/physics_util/a2/d0hplt.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0hplatch541.o):\
  $(d0root)/physics_util/d0hplt/d0hplt_dispatch.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/d0hplt_dispatch.f ;\
	mv d0hplt_dispatch.o $(SCRATCH)/physics_util/a2/d0hplatch541.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0hplt_event.o):\
  $(d0root)/physics_util/d0hplt/d0hplt_event.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/d0hplt_event.f ;\
	mv d0hplt_event.o $(SCRATCH)/physics_util/a2/d0hplt_event.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0htyp.o):\
  $(d0root)/physics_util/d0hplt/d0htyp.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/d0htyp.f ;\
	mv d0htyp.o $(SCRATCH)/physics_util/a2/d0htyp.o ;\
	)
$(d0root)/physics_util/physics_util.a(d0huph.o):\
  $(d0root)/physics_util/d0hplt/d0huph.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/d0huph.f ;\
	mv d0huph.o $(SCRATCH)/physics_util/a2/d0huph.o ;\
	)
$(d0root)/physics_util/physics_util.a(dbank.o):\
  $(d0root)/physics_util/dbank/dbank.f\
  $(d0library)/inc/ssdef.def\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebq.inc\
  $(d0library)/inc/mzca.inc\
  $(d0library)/inc/mzcb.inc\
  $(d0library)/inc/autof.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/dbank/dbank.f ;\
	mv dbank.o $(SCRATCH)/physics_util/a2/dbank.o ;\
	)
$(d0root)/physics_util/physics_util.a(dcd_chfrm.o):\
  $(d0root)/physics_util/dbank/dcd_chfrm.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/dbank/dcd_chfrm.f ;\
	mv dcd_chfrm.o $(SCRATCH)/physics_util/a2/dcd_chfrm.o ;\
	)
$(d0root)/physics_util/physics_util.a(deta_to_peta.o):\
  $(d0root)/physics_util/general/deta_to_peta.f\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/deta_to_peta.f ;\
	mv deta_to_peta.o $(SCRATCH)/physics_util/a3/deta_to_peta.o ;\
	)
$(d0root)/physics_util/physics_util.a(dmpdef.o):\
  $(d0root)/physics_util/general/dmpdef.f\
  $(d0library)/inc/dump.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/dmpdef.f ;\
	mv dmpdef.o $(SCRATCH)/physics_util/a3/dmpdef.o ;\
	)
$(d0root)/physics_util/physics_util.a(dot.o):\
  $(d0root)/physics_util/general/dot.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/dot.f ;\
	mv dot.o $(SCRATCH)/physics_util/a3/dot.o ;\
	)
$(d0root)/physics_util/physics_util.a(drop__alg357.o):\
  $(d0root)/physics_util/general/drop_caph_alg.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/caph.def\
  $(d0library)/links/izcaph.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/drop_caph_alg.f ;\
	mv drop_caph_alg.o $(SCRATCH)/physics_util/a3/drop__alg357.o ;\
	)
$(d0root)/physics_util/physics_util.a(duplilter347.o):\
  $(d0root)/physics_util/general/duplicate_event_filter.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/duplicate_event_filter.f ;\
	mv duplicate_event_filter.o $(SCRATCH)/physics_util/a3/duplilter347.o ;\
	)
$(d0root)/physics_util/physics_util.a(dzfor1.o):\
  $(d0root)/physics_util/dbank/dzfor1.f\
  $(d0library)/inc/mzca.inc\
  $(d0library)/inc/mzcb.inc\
  $(d0library)/inc/mzcc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/dbank/dzfor1.f ;\
	mv dzfor1.o $(SCRATCH)/physics_util/a3/dzfor1.o ;\
	)
$(d0root)/physics_util/physics_util.a(el2_b.o):\
  $(d0root)/physics_util/filters/el2_b.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpelc.link\
  $(d0library)/links/izppho.link\
  $(d0library)/links/izpmuo.link\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/el2_b.f ;\
	mv el2_b.o $(SCRATCH)/physics_util/a3/el2_b.o ;\
	)
$(d0root)/physics_util/physics_util.a(elf.o):\
  $(d0root)/physics_util/filters/elf.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/elf.f ;\
	mv elf.o $(SCRATCH)/physics_util/a3/elf.o ;\
	)
$(d0root)/physics_util/physics_util.a(elf_med.o):\
  $(d0root)/physics_util/filters/elf_med.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/elf_med.f ;\
	mv elf_med.o $(SCRATCH)/physics_util/a3/elf_med.o ;\
	)
$(d0root)/physics_util/physics_util.a(elf_w.o):\
  $(d0root)/physics_util/filters/elf_w.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/elf_w.f ;\
	mv elf_w.o $(SCRATCH)/physics_util/a3/elf_w.o ;\
	)
$(d0root)/physics_util/physics_util.a(elf_z.o):\
  $(d0root)/physics_util/filters/elf_z.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/elf_z.f ;\
	mv elf_z.o $(SCRATCH)/physics_util/a3/elf_z.o ;\
	)
$(d0root)/physics_util/physics_util.a(em_etzv.o):\
  $(d0root)/physics_util/filters/em_etzv.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izzfit.link\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/em_etzv.f ;\
	mv em_etzv.o $(SCRATCH)/physics_util/a3/em_etzv.o ;\
	)
$(d0root)/physics_util/physics_util.a(em_fits_mask.o):\
  $(d0root)/physics_util/general/em_fits_mask.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/em_fits_mask.f ;\
	mv em_fits_mask.o $(SCRATCH)/physics_util/a3/em_fits_mask.o ;\
	)
$(d0root)/physics_util/physics_util.a(em_in_jet.o):\
  $(d0root)/physics_util/general/em_in_jet.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/em_in_jet.f ;\
	mv em_in_jet.o $(SCRATCH)/physics_util/a3/em_in_jet.o ;\
	)
$(d0root)/physics_util/physics_util.a(emag_jets.o):\
  $(d0root)/physics_util/filters/emag_jets.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/emag_jets.f ;\
	mv emag_jets.o $(SCRATCH)/physics_util/a3/emag_jets.o ;\
	)
$(d0root)/physics_util/physics_util.a(emfix.o):\
  $(d0root)/physics_util/filters/emfix.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/cemprf.inc\
  $(d0library)/inc/cimpact.inc\
  $(d0library)/inc/ctrak.inc\
  $(d0library)/inc/chmatr_new.inc\
  $(d0library)/inc/more_ntuple.inc\
  $(d0library)/links/izzfit.link\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/emfix.f ;\
	mv emfix.o $(SCRATCH)/physics_util/a3/emfix.o ;\
	)
$(d0root)/physics_util/physics_util.a(emphifix.o):\
  $(d0root)/physics_util/general/emphifix.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/emphifix.f ;\
	mv emphifix.o $(SCRATCH)/physics_util/a3/emphifix.o ;\
	)
$(d0root)/physics_util/physics_util.a(esum_ress423.o):\
  $(d0root)/physics_util/udst/esum_compress.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/esum_compress.f ;\
	mv esum_compress.o $(SCRATCH)/physics_util/a3/esum_ress423.o ;\
	)
$(d0root)/physics_util/physics_util.a(event_list.o):\
  $(d0root)/physics_util/filters/event_list.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/event_list.f ;\
	mv event_list.o $(SCRATCH)/physics_util/a3/event_list.o ;\
	)
$(d0root)/physics_util/physics_util.a(ez_sepack718.o):\
  $(d0root)/physics_util/general/ez_setup_compack.f\
  $(d0library)/offline_util/general/value.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/ez_setup_compack.f ;\
	mv ez_setup_compack.o $(SCRATCH)/physics_util/a3/ez_sepack718.o ;\
	)
$(d0root)/physics_util/physics_util.a(ezbank.o):\
  $(d0root)/physics_util/general/ezbank.f\
  $(d0library)/params/srcp.def\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/ezbank.f ;\
	mv ezbank.o $(SCRATCH)/physics_util/a4/ezbank.o ;\
	)
$(d0root)/physics_util/physics_util.a(fake_date633.o):\
  $(d0root)/physics_util/filters/fake_e_candidate.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/links/izvert.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/fake_e_candidate.f ;\
	mv fake_e_candidate.o $(SCRATCH)/physics_util/a4/fake_date633.o ;\
	)
$(d0root)/physics_util/physics_util.a(fill_cdtk.o):\
  $(d0root)/physics_util/udst/fill_cdtk.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izdtrk.link\
  $(d0library)/inc/cdtk_packing.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/fill_cdtk.f ;\
	mv fill_cdtk.o $(SCRATCH)/physics_util/a4/fill_cdtk.o ;\
	)
$(d0root)/physics_util/physics_util.a(fill_nt.o):\
  $(d0root)/physics_util/udst/fill_nt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/fill_nt.f ;\
	mv fill_nt.o $(SCRATCH)/physics_util/a4/fill_nt.o ;\
	)
$(d0root)/physics_util/physics_util.a(fill_ions945.o):\
  $(d0root)/physics_util/udst/fill_utag_versions.f\
  $(d0library)/inc/utag_data.inc\
  $(d0library)/inc/utag_versions.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/fill_utag_versions.f ;\
	mv fill_utag_versions.o $(SCRATCH)/physics_util/a4/fill_ions945.o ;\
	)
$(d0root)/physics_util/physics_util.a(filter_bits.o):\
  $(d0root)/physics_util/general/filter_bits.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/filter_bits.f ;\
	mv filter_bits.o $(SCRATCH)/physics_util/a4/filter_bits.o ;\
	)
$(d0root)/physics_util/physics_util.a(filtset.o):\
  $(d0root)/physics_util/udst/filtset.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/filtset.f ;\
	mv filtset.o $(SCRATCH)/physics_util/a4/filtset.o ;\
	)
$(d0root)/physics_util/physics_util.a(find_st_b469.o):\
  $(d0root)/physics_util/general/find_nearest_b.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/find_nearest_b.f ;\
	mv find_nearest_b.o $(SCRATCH)/physics_util/a4/find_st_b469.o ;\
	)
$(d0root)/physics_util/physics_util.a(find_st_c470.o):\
  $(d0root)/physics_util/general/find_nearest_c.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/find_nearest_c.f ;\
	mv find_nearest_c.o $(SCRATCH)/physics_util/a4/find_st_c470.o ;\
	)
$(d0root)/physics_util/physics_util.a(find_st_e472.o):\
  $(d0root)/physics_util/general/find_nearest_e.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/find_nearest_e.f ;\
	mv find_nearest_e.o $(SCRATCH)/physics_util/a4/find_st_e472.o ;\
	)
$(d0root)/physics_util/physics_util.a(find_t_mu597.o):\
  $(d0root)/physics_util/general/find_nearest_mu.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/find_nearest_mu.f ;\
	mv find_nearest_mu.o $(SCRATCH)/physics_util/a4/find_t_mu597.o ;\
	)
$(d0root)/physics_util/physics_util.a(find_ject002.o):\
  $(d0root)/physics_util/general/find_nearest_object.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/find_nearest_object.f ;\
	mv find_nearest_object.o $(SCRATCH)/physics_util/a4/find_ject002.o ;\
	)
$(d0root)/physics_util/physics_util.a(find__tau701.o):\
  $(d0root)/physics_util/general/find_nearest_tau.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/find_nearest_tau.f ;\
	mv find_nearest_tau.o $(SCRATCH)/physics_util/a4/find__tau701.o ;\
	)
$(d0root)/physics_util/physics_util.a(find_wlnu.o):\
  $(d0root)/physics_util/general/find_wlnu.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/find_wlnu.f ;\
	mv find_wlnu.o $(SCRATCH)/physics_util/a4/find_wlnu.o ;\
	)
$(d0root)/physics_util/physics_util.a(find_wlnu2.o):\
  $(d0root)/physics_util/general/find_wlnu2.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/find_wlnu2.f ;\
	mv find_wlnu2.o $(SCRATCH)/physics_util/a4/find_wlnu2.o ;\
	)
$(d0root)/physics_util/physics_util.a(find_wlnu2d.o):\
  $(d0root)/physics_util/general/find_wlnu2d.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/find_wlnu2d.f ;\
	mv find_wlnu2d.o $(SCRATCH)/physics_util/a4/find_wlnu2d.o ;\
	)
$(d0root)/physics_util/physics_util.a(find_wlnud.o):\
  $(d0root)/physics_util/general/find_wlnud.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/find_wlnud.f ;\
	mv find_wlnud.o $(SCRATCH)/physics_util/a4/find_wlnud.o ;\
	)
$(d0root)/physics_util/physics_util.a(fix_jet_et.o):\
  $(d0root)/physics_util/general/fix_jet_et.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcaph.link\
  $(d0library)/links/izjets.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/fix_jet_et.f ;\
	mv fix_jet_et.o $(SCRATCH)/physics_util/a4/fix_jet_et.o ;\
	)
$(d0root)/physics_util/physics_util.a(flusher.o):\
  $(d0root)/physics_util/general/flusher.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/flusher.f ;\
	mv flusher.o $(SCRATCH)/physics_util/a4/flusher.o ;\
	)
$(d0root)/physics_util/physics_util.a(frame_menadd.o):\
  $(d0root)/physics_util/general/frame_menadd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/frame_menadd.f ;\
	mv frame_menadd.o $(SCRATCH)/physics_util/a4/frame_menadd.o ;\
	)
$(d0root)/physics_util/physics_util.a(fwmoment.o):\
  $(d0root)/physics_util/general/fwmoment.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/fwmoment.f ;\
	mv fwmoment.o $(SCRATCH)/physics_util/a5/fwmoment.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_caph_alg.o):\
  $(d0root)/physics_util/general/get_caph_alg.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/caph.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_caph_alg.f ;\
	mv get_caph_alg.o $(SCRATCH)/physics_util/a5/get_caph_alg.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_eance462.o):\
  $(d0root)/physics_util/udst/get_e_variance.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/get_e_variance.f ;\
	mv get_e_variance.o $(SCRATCH)/physics_util/a5/get_eance462.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_erons400.o):\
  $(d0root)/physics_util/general/get_electrons.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/links/izhmte.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_electrons.f ;\
	mv get_electrons.o $(SCRATCH)/physics_util/a5/get_erons400.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_idren784.o):\
  $(d0root)/physics_util/general/get_isaj_children.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_isaj_children.f ;\
	mv get_isaj_children.o $(SCRATCH)/physics_util/a5/get_idren784.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_irton603.o):\
  $(d0root)/physics_util/general/get_isaj_parton.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_isaj_parton.f ;\
	mv get_isaj_parton.o $(SCRATCH)/physics_util/a5/get_irton603.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_idren001.o):\
  $(d0root)/physics_util/general/get_isajet_children.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_isajet_children.f ;\
	mv get_isajet_children.o $(SCRATCH)/physics_util/a5/get_idren001.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_irton820.o):\
  $(d0root)/physics_util/general/get_isajet_parton.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_isajet_parton.f ;\
	mv get_isajet_parton.o $(SCRATCH)/physics_util/a5/get_irton820.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_idren786.o):\
  $(d0root)/physics_util/general/get_isal_children.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_isal_children.f ;\
	mv get_isal_children.o $(SCRATCH)/physics_util/a5/get_idren786.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_irton605.o):\
  $(d0root)/physics_util/general/get_isal_parton.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_isal_parton.f ;\
	mv get_isal_parton.o $(SCRATCH)/physics_util/a5/get_irton605.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_idren791.o):\
  $(d0root)/physics_util/general/get_isaq_children.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_isaq_children.f ;\
	mv get_isaq_children.o $(SCRATCH)/physics_util/a5/get_idren791.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_irton610.o):\
  $(d0root)/physics_util/general/get_isaq_parton.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_isaq_parton.f ;\
	mv get_isaq_parton.o $(SCRATCH)/physics_util/a5/get_irton610.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_jets.o):\
  $(d0root)/physics_util/general/get_jets.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/params/caph.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_jets.f ;\
	mv get_jets.o $(SCRATCH)/physics_util/a5/get_jets.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_linfo358.o):\
  $(d0root)/physics_util/general/get_l1_l2_info.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_l1_l2_info.f ;\
	mv get_l1_l2_info.o $(SCRATCH)/physics_util/a5/get_linfo358.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_muons.o):\
  $(d0root)/physics_util/general/get_muons.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_muons.f ;\
	mv get_muons.o $(SCRATCH)/physics_util/a5/get_muons.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_nicle819.o):\
  $(d0root)/physics_util/general/get_next_particle.f\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisp1.link\
  $(d0library)/links/izisv1.link\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_next_particle.f ;\
	mv get_next_particle.o $(SCRATCH)/physics_util/a5/get_nicle819.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_p2neut.o):\
  $(d0root)/physics_util/general/get_p2neut.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_p2neut.f ;\
	mv get_p2neut.o $(SCRATCH)/physics_util/a5/get_p2neut.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_preco030.o):\
  $(d0root)/physics_util/general/get_partons_in_reco.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_partons_in_reco.f ;\
	mv get_partons_in_reco.o $(SCRATCH)/physics_util/a5/get_preco030.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_photons.o):\
  $(d0root)/physics_util/general/get_photons.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_photons.f ;\
	mv get_photons.o $(SCRATCH)/physics_util/a5/get_photons.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_ptors729.o):\
  $(d0root)/physics_util/general/get_pjet_vectors.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpjet.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_pjet_vectors.f ;\
	mv get_pjet_vectors.o $(SCRATCH)/physics_util/a5/get_ptors729.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_qtent831.o):\
  $(d0root)/physics_util/general/get_quark_content.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_quark_content.f ;\
	mv get_quark_content.o $(SCRATCH)/physics_util/a6/get_qtent831.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_s_div400.o):\
  $(d0root)/physics_util/dbank/get_store_div.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/dbank/get_store_div.f ;\
	mv get_store_div.o $(SCRATCH)/physics_util/a6/get_s_div400.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_tags.o):\
  $(d0root)/physics_util/udst/get_tags.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/udst_dimensions.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/get_tags.f ;\
	mv get_tags.o $(SCRATCH)/physics_util/a6/get_tags.o ;\
	)
$(d0root)/physics_util/physics_util.a(get_t_run825.o):\
  $(d0root)/physics_util/general/get_trig_filt_run.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/get_trig_filt_run.f ;\
	mv get_trig_filt_run.o $(SCRATCH)/physics_util/a6/get_t_run825.o ;\
	)
$(d0root)/physics_util/physics_util.a(getlum.o):\
  $(d0root)/physics_util/general/getlum.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/getlum.f ;\
	mv getlum.o $(SCRATCH)/physics_util/a6/getlum.o ;\
	)
$(d0root)/physics_util/physics_util.a(getlum_corr.o):\
  $(d0root)/physics_util/general/getlum_corr.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/getlum_corr.f ;\
	mv getlum_corr.o $(SCRATCH)/physics_util/a6/getlum_corr.o ;\
	)
$(d0root)/physics_util/physics_util.a(getzeb.o):\
  $(d0root)/physics_util/dbank/getzeb.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/dbank/getzeb.f ;\
	mv getzeb.o $(SCRATCH)/physics_util/a6/getzeb.o ;\
	)
$(d0root)/physics_util/physics_util.a(good_tron390.o):\
  $(d0root)/physics_util/general/good_electron.f\
  $(d0library)/params/good_electron_photon.params\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/good_electron.f ;\
	mv good_electron.o $(SCRATCH)/physics_util/a6/good_tron390.o ;\
	)
$(d0root)/physics_util/physics_util.a(good_heck582.o):\
  $(d0root)/physics_util/filters/good_muon_check.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/good_muon_check.f ;\
	mv good_muon_check.o $(SCRATCH)/physics_util/a6/good_heck582.o ;\
	)
$(d0root)/physics_util/physics_util.a(good_icle382.o):\
  $(d0root)/physics_util/general/good_particle.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/good_particle.f ;\
	mv good_particle.o $(SCRATCH)/physics_util/a6/good_icle382.o ;\
	)
$(d0root)/physics_util/physics_util.a(good_photon.o):\
  $(d0root)/physics_util/general/good_photon.f\
  $(d0library)/params/good_electron_photon.params\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/good_photon.f ;\
	mv good_photon.o $(SCRATCH)/physics_util/a6/good_photon.o ;\
	)
$(d0root)/physics_util/physics_util.a(good_mu1b754.o):\
  $(d0root)/physics_util/filters/good_trigger_mu1b.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/good_trigger_mu1b.f ;\
	mv good_trigger_mu1b.o $(SCRATCH)/physics_util/a6/good_mu1b754.o ;\
	)
$(d0root)/physics_util/physics_util.a(gtcdtk.o):\
  $(d0root)/physics_util/udst/gtcdtk.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcdtk.link\
  $(d0library)/inc/cdtk_packing.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/gtcdtk.f ;\
	mv gtcdtk.o $(SCRATCH)/physics_util/a6/gtcdtk.o ;\
	)
$(d0root)/physics_util/physics_util.a(gtchfr.o):\
  $(d0root)/physics_util/dbank/gtchfr.f\
  $(d0library)/inc/autof.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebq.inc\
  $(d0library)/inc/mzca.inc\
  $(d0library)/inc/mzcb.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/dbank/gtchfr.f ;\
	mv gtchfr.o $(SCRATCH)/physics_util/a6/gtchfr.o ;\
	)
$(d0root)/physics_util/physics_util.a(gtzfrm.o):\
  $(d0root)/physics_util/dbank/gtzfrm.f\
  $(d0library)/inc/mzca.inc\
  $(d0library)/inc/mzcb.inc\
  $(d0library)/inc/mzcc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/dbank/gtzfrm.f ;\
	mv gtzfrm.o $(SCRATCH)/physics_util/a6/gtzfrm.o ;\
	)
$(d0root)/physics_util/physics_util.a(handler.o):\
  $(d0root)/physics_util/udst/handler.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/handler.f ;\
	mv handler.o $(SCRATCH)/physics_util/a6/handler.o ;\
	)
$(d0root)/physics_util/physics_util.a(hisprt.o):\
  $(d0root)/physics_util/general/hisprt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/hisprt.f ;\
	mv hisprt.o $(SCRATCH)/physics_util/a6/hisprt.o ;\
	)
$(d0root)/physics_util/physics_util.a(hot_c_jet580.o):\
  $(d0root)/physics_util/general/hot_cell_in_jet.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/byte_order.params\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/hot_cell_in_jet.f ;\
	mv hot_cell_in_jet.o $(SCRATCH)/physics_util/a6/hot_c_jet580.o ;\
	)
$(d0root)/physics_util/physics_util.a(hplotatch504.o):\
  $(d0root)/physics_util/d0hplt/hplot_dispatch.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/hplot_dispatch.f ;\
	mv hplot_dispatch.o $(SCRATCH)/physics_util/a6/hplotatch504.o ;\
	)
$(d0root)/physics_util/physics_util.a(hplot_menu.o):\
  $(d0root)/physics_util/d0hplt/hplot_menu.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/hplot_menu.f ;\
	mv hplot_menu.o $(SCRATCH)/physics_util/a6/hplot_menu.o ;\
	)
$(d0root)/physics_util/physics_util.a(hplotmenu624.o):\
  $(d0root)/physics_util/d0hplt/hplot_proc_menu.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/hplot_proc_menu.f ;\
	mv hplot_proc_menu.o $(SCRATCH)/physics_util/a7/hplotmenu624.o ;\
	)
$(d0root)/physics_util/physics_util.a(invarmass513.o):\
  $(d0root)/physics_util/general/invariant_mass.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/invariant_mass.f ;\
	mv invariant_mass.o $(SCRATCH)/physics_util/a7/invarmass513.o ;\
	)
$(d0root)/physics_util/physics_util.a(is_datafile.o):\
  $(d0root)/physics_util/general/is_datafile.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/is_datafile.f ;\
	mv is_datafile.o $(SCRATCH)/physics_util/a7/is_datafile.o ;\
	)
$(d0root)/physics_util/physics_util.a(isol_lect505.o):\
  $(d0root)/physics_util/general/isol_mu_select.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/isol_mu_select.f ;\
	mv isol_mu_select.o $(SCRATCH)/physics_util/a7/isol_lect505.o ;\
	)
$(d0root)/physics_util/physics_util.a(jet_ccorr382.o):\
  $(d0root)/physics_util/general/jet_cone_corr.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/jet_cone_corr.f ;\
	mv jet_cone_corr.o $(SCRATCH)/physics_util/a7/jet_ccorr382.o ;\
	)
$(d0root)/physics_util/physics_util.a(jet_corr.o):\
  $(d0root)/physics_util/general/jet_corr.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izvert.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/jet_corr.f ;\
	mv jet_corr.o $(SCRATCH)/physics_util/a7/jet_corr.o ;\
	)
$(d0root)/physics_util/physics_util.a(jet_ecale355.o):\
  $(d0root)/physics_util/general/jet_emf_scale.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/jet_emf_scale.f ;\
	mv jet_emf_scale.o $(SCRATCH)/physics_util/a7/jet_ecale355.o ;\
	)
$(d0root)/physics_util/physics_util.a(jet_et_true.o):\
  $(d0root)/physics_util/general/jet_et_true.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/jet_et_true.f ;\
	mv jet_et_true.o $(SCRATCH)/physics_util/a7/jet_et_true.o ;\
	)
$(d0root)/physics_util/physics_util.a(jet_info.o):\
  $(d0root)/physics_util/filters/jet_info.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/jet_info.f ;\
	mv jet_info.o $(SCRATCH)/physics_util/a7/jet_info.o ;\
	)
$(d0root)/physics_util/physics_util.a(jet_ocorr129.o):\
  $(d0root)/physics_util/general/jet_out_of_cone_corr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/jet_out_of_cone_corr.f ;\
	mv jet_out_of_cone_corr.o $(SCRATCH)/physics_util/a7/jet_ocorr129.o ;\
	)
$(d0root)/physics_util/physics_util.a(jet_uctor838.o):\
  $(d0root)/physics_util/general/jet_undzsp_factor.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/jet_undzsp_factor.f ;\
	mv jet_undzsp_factor.o $(SCRATCH)/physics_util/a7/jet_uctor838.o ;\
	)
$(d0root)/physics_util/physics_util.a(jutlfl.o):\
  $(d0root)/physics_util/udst/jutlfl.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcaph.link\
  $(d0library)/links/izjtcs.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/jutlfl.f ;\
	mv jutlfl.o $(SCRATCH)/physics_util/a7/jutlfl.o ;\
	)
$(d0root)/physics_util/physics_util.a(l15cafill579.o):\
  $(d0root)/physics_util/general/l15cal_esum_fill.f\
  $(d0library)/params/esum.params\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/level1_lookup.params\
  $(d0library)/params/l1_caltrig.params\
  $(d0library)/params/l1php.params\
  $(d0library)/inc/dbitt.inc\
  $(d0library)/params/byte_order.params\
  $(d0library)/params/l15caldbb_data_block.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/l15cal_esum_fill.f ;\
	mv l15cal_esum_fill.o $(SCRATCH)/physics_util/a7/l15cafill579.o ;\
	)
$(d0root)/physics_util/physics_util.a(l1esum_evt.o):\
  $(d0root)/physics_util/general/l1esum_evt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/l1esum_evt.f ;\
	mv l1esum_evt.o $(SCRATCH)/physics_util/a7/l1esum_evt.o ;\
	)
$(d0root)/physics_util/physics_util.a(l1esum_muon.o):\
  $(d0root)/physics_util/general/l1esum_muon.f\
  $(d0library)/params/esum.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/l1esum_muon.f ;\
	mv l1esum_muon.o $(SCRATCH)/physics_util/a7/l1esum_muon.o ;\
	)
$(d0root)/physics_util/physics_util.a(l1esum_trgr.o):\
  $(d0root)/physics_util/general/l1esum_trgr.f\
  $(d0library)/params/esum.params\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/level1_lookup.params\
  $(d0library)/params/l1_caltrig.params\
  $(d0library)/params/l1php.params\
  $(d0library)/inc/dbitt.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/l1esum_trgr.f ;\
	mv l1esum_trgr.o $(SCRATCH)/physics_util/a7/l1esum_trgr.o ;\
	)
$(d0root)/physics_util/physics_util.a(l1esum_trgr2.o):\
  $(d0root)/physics_util/general/l1esum_trgr2.f\
  $(d0library)/params/esum.params\
  $(d0library)/params/level1_lookup.params\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/l1esum_trgr2.f ;\
	mv l1esum_trgr2.o $(SCRATCH)/physics_util/a7/l1esum_trgr2.o ;\
	)
$(d0root)/physics_util/physics_util.a(lo_calc.o):\
  $(d0root)/physics_util/general/lo_calc.f\
  $(d0library)/inc/lo_com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/lo_calc.f ;\
	mv lo_calc.o $(SCRATCH)/physics_util/a7/lo_calc.o ;\
	)
$(d0root)/physics_util/physics_util.a(lo_dsig_dt.o):\
  $(d0root)/physics_util/general/lo_dsig_dt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/lo_dsig_dt.f ;\
	mv lo_dsig_dt.o $(SCRATCH)/physics_util/a7/lo_dsig_dt.o ;\
	)
$(d0root)/physics_util/physics_util.a(lo_ds_sel477.o):\
  $(d0root)/physics_util/general/lo_dsig_dt_sel.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/lo_dsig_dt_sel.f ;\
	mv lo_dsig_dt_sel.o $(SCRATCH)/physics_util/a7/lo_ds_sel477.o ;\
	)
$(d0root)/physics_util/physics_util.a(lo_in_r.o):\
  $(d0root)/physics_util/general/lo_in_r.f\
  $(d0library)/inc/lo_com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/lo_in_r.f ;\
	mv lo_in_r.o $(SCRATCH)/physics_util/a8/lo_in_r.o ;\
	)
$(d0root)/physics_util/physics_util.a(lo_main_r.o):\
  $(d0root)/physics_util/general/lo_main_r.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/lo_main_r.f ;\
	mv lo_main_r.o $(SCRATCH)/physics_util/a8/lo_main_r.o ;\
	)
$(d0root)/physics_util/physics_util.a(lo_nt.o):\
  $(d0root)/physics_util/general/lo_nt.f\
  $(d0library)/inc/lo_com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/lo_nt.f ;\
	mv lo_nt.o $(SCRATCH)/physics_util/a8/lo_nt.o ;\
	)
$(d0root)/physics_util/physics_util.a(lo_out.o):\
  $(d0root)/physics_util/general/lo_out.f\
  $(d0library)/inc/lo_com.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/lo_out.f ;\
	mv lo_out.o $(SCRATCH)/physics_util/a8/lo_out.o ;\
	)
$(d0root)/physics_util/physics_util.a(lqnn.o):\
  $(d0root)/physics_util/filters/lqnn.f\
  $(d0library)/links/izpelc.link\
  $(d0library)/links/izppho.link\
  $(d0library)/links/izpnut.link\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/lqnn.f ;\
	mv lqnn.o $(SCRATCH)/physics_util/a8/lqnn.o ;\
	)
$(d0root)/physics_util/physics_util.a(lqnue.o):\
  $(d0root)/physics_util/filters/lqnue.f\
  $(d0library)/links/izpelc.link\
  $(d0library)/links/izppho.link\
  $(d0library)/links/izpnut.link\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/lqnue.f ;\
	mv lqnue.o $(SCRATCH)/physics_util/a8/lqnue.o ;\
	)
$(d0root)/physics_util/physics_util.a(make_nt.o):\
  $(d0root)/physics_util/udst/make_nt.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/make_nt.f ;\
	mv make_nt.o $(SCRATCH)/physics_util/a8/make_nt.o ;\
	)
$(d0root)/physics_util/physics_util.a(make_udst.o):\
  $(d0root)/physics_util/udst/make_udst.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izvert.link\
  $(d0library)/links/izztrk.link\
  $(d0library)/links/izvtxt.link\
  $(d0library)/links/izjets.link\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/inc/udst_links.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/make_udst.f ;\
	mv make_udst.o $(SCRATCH)/physics_util/a8/make_udst.o ;\
	)
$(d0root)/physics_util/physics_util.a(make_uint.o):\
  $(d0root)/physics_util/udst/make_uint.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izuint.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/make_uint.f ;\
	mv make_uint.o $(SCRATCH)/physics_util/a8/make_uint.o ;\
	)
$(d0root)/physics_util/physics_util.a(matchdata553.o):\
  $(d0root)/physics_util/general/matchem_mc_data.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/matchem_mc_data.f ;\
	mv matchem_mc_data.o $(SCRATCH)/physics_util/a8/matchdata553.o ;\
	)
$(d0root)/physics_util/physics_util.a(mc_et_corr.o):\
  $(d0root)/physics_util/general/mc_et_corr.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mc_et_corr.f ;\
	mv mc_et_corr.o $(SCRATCH)/physics_util/a8/mc_et_corr.o ;\
	)
$(d0root)/physics_util/physics_util.a(menu_d0hplt.o):\
  $(d0root)/physics_util/d0hplt/menu_d0hplt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/d0hplt/menu_d0hplt.f ;\
	mv menu_d0hplt.o $(SCRATCH)/physics_util/a8/menu_d0hplt.o ;\
	)
$(d0root)/physics_util/physics_util.a(minbias.o):\
  $(d0root)/physics_util/filters/minbias.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/minbias.f ;\
	mv minbias.o $(SCRATCH)/physics_util/a8/minbias.o ;\
	)
$(d0root)/physics_util/physics_util.a(miss_et_corr.o):\
  $(d0root)/physics_util/general/miss_et_corr.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/miss_et_corr.f ;\
	mv miss_et_corr.o $(SCRATCH)/physics_util/a8/miss_et_corr.o ;\
	)
$(d0root)/physics_util/physics_util.a(montedata686.o):\
  $(d0root)/physics_util/general/monte_carlo_data.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/monte_carlo_data.f ;\
	mv monte_carlo_data.o $(SCRATCH)/physics_util/a8/montedata686.o ;\
	)
$(d0root)/physics_util/physics_util.a(mpf_fjets384.o):\
  $(d0root)/physics_util/general/mpf_fill_jets.f\
  $(d0library)/inc/mpf_jetres.inc\
  $(d0library)/inc/mpf_filljet.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mpf_fill_jets.f ;\
	mv mpf_fill_jets.o $(SCRATCH)/physics_util/a8/mpf_fjets384.o ;\
	)
$(d0root)/physics_util/physics_util.a(mpf_jonse725.o):\
  $(d0root)/physics_util/general/mpf_jet_response.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/mpf_jetres.inc\
  $(d0library)/inc/mpf_filljet.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mpf_jet_response.f ;\
	mv mpf_jet_response.o $(SCRATCH)/physics_util/a8/mpf_jonse725.o ;\
	)
$(d0root)/physics_util/physics_util.a(mpf_j_fin137.o):\
  $(d0root)/physics_util/general/mpf_jet_response_fin.f\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/mpf_jetres.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mpf_jet_response_fin.f ;\
	mv mpf_jet_response_fin.o $(SCRATCH)/physics_util/a8/mpf_j_fin137.o ;\
	)
$(d0root)/physics_util/physics_util.a(mpf_j_ini140.o):\
  $(d0root)/physics_util/general/mpf_jet_response_ini.f\
  $(d0library)/inc/mpf_jetres.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mpf_jet_response_ini.f ;\
	mv mpf_jet_response_ini.o $(SCRATCH)/physics_util/a8/mpf_j_ini140.o ;\
	)
$(d0root)/physics_util/physics_util.a(mpf_jetcorr.o):\
  $(d0root)/physics_util/general/mpf_jetcorr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mpf_jetcorr.f ;\
	mv mpf_jetcorr.o $(SCRATCH)/physics_util/a8/mpf_jetcorr.o ;\
	)
$(d0root)/physics_util/physics_util.a(mpf_jets.o):\
  $(d0root)/physics_util/general/mpf_jets.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/mpf_jetres.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mpf_jets.f ;\
	mv mpf_jets.o $(SCRATCH)/physics_util/a9/mpf_jets.o ;\
	)
$(d0root)/physics_util/physics_util.a(mpf_lcorr121.o):\
  $(d0root)/physics_util/general/mpf_low_et_bias_corr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mpf_low_et_bias_corr.f ;\
	mv mpf_low_et_bias_corr.o $(SCRATCH)/physics_util/a9/mpf_lcorr121.o ;\
	)
$(d0root)/physics_util/physics_util.a(mpf_mcorr492.o):\
  $(d0root)/physics_util/general/mpf_mc_jetcorr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mpf_mc_jetcorr.f ;\
	mv mpf_mc_jetcorr.o $(SCRATCH)/physics_util/a9/mpf_mcorr492.o ;\
	)
$(d0root)/physics_util/physics_util.a(mpf_photons.o):\
  $(d0root)/physics_util/general/mpf_photons.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izjets.link\
  $(d0library)/inc/mpf_jetres.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mpf_photons.f ;\
	mv mpf_photons.o $(SCRATCH)/physics_util/a9/mpf_photons.o ;\
	)
$(d0root)/physics_util/physics_util.a(mpf_r_fit725.o):\
  $(d0root)/physics_util/general/mpf_response_fit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mpf_response_fit.f ;\
	mv mpf_response_fit.o $(SCRATCH)/physics_util/a9/mpf_r_fit725.o ;\
	)
$(d0root)/physics_util/physics_util.a(mpf_sim.o):\
  $(d0root)/physics_util/general/mpf_sim.f\
  $(d0library)/inc/mpf_jetres.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mpf_sim.f ;\
	mv mpf_sim.o $(SCRATCH)/physics_util/a9/mpf_sim.o ;\
	)
$(d0root)/physics_util/physics_util.a(mpf_z_boson.o):\
  $(d0root)/physics_util/general/mpf_z_boson.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/mpf_jetres.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mpf_z_boson.f ;\
	mv mpf_z_boson.o $(SCRATCH)/physics_util/a9/mpf_z_boson.o ;\
	)
$(d0root)/physics_util/physics_util.a(msp_mlect623.o):\
  $(d0root)/physics_util/filters/msp_muon_select.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/msp_muon_select.f ;\
	mv msp_muon_select.o $(SCRATCH)/physics_util/a9/msp_mlect623.o ;\
	)
$(d0root)/physics_util/physics_util.a(mu1_b.o):\
  $(d0root)/physics_util/filters/mu1_b.f\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/mu1_b.f ;\
	mv mu1_b.o $(SCRATCH)/physics_util/a9/mu1_b.o ;\
	)
$(d0root)/physics_util/physics_util.a(mu1_filt.o):\
  $(d0root)/physics_util/filters/mu1_filt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/mu1_filt.f ;\
	mv mu1_filt.o $(SCRATCH)/physics_util/a9/mu1_filt.o ;\
	)
$(d0root)/physics_util/physics_util.a(mu1_wzt.o):\
  $(d0root)/physics_util/filters/mu1_wzt.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/mu1_wzt.f ;\
	mv mu1_wzt.o $(SCRATCH)/physics_util/a9/mu1_wzt.o ;\
	)
$(d0root)/physics_util/physics_util.a(mu2_b.o):\
  $(d0root)/physics_util/filters/mu2_b.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izmuot.link\
  $(d0library)/links/izpmuo.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/mu2_b.f ;\
	mv mu2_b.o $(SCRATCH)/physics_util/a9/mu2_b.o ;\
	)
$(d0root)/physics_util/physics_util.a(mu2_wzt.o):\
  $(d0root)/physics_util/filters/mu2_wzt.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/mu2_wzt.f ;\
	mv mu2_wzt.o $(SCRATCH)/physics_util/a9/mu2_wzt.o ;\
	)
$(d0root)/physics_util/physics_util.a(mu3_b.o):\
  $(d0root)/physics_util/filters/mu3_b.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpmuo.link\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/mu3_b.f ;\
	mv mu3_b.o $(SCRATCH)/physics_util/a9/mu3_b.o ;\
	)
$(d0root)/physics_util/physics_util.a(mu_b.o):\
  $(d0root)/physics_util/filters/mu_b.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/mu_b.f ;\
	mv mu_b.o $(SCRATCH)/physics_util/a9/mu_b.o ;\
	)
$(d0root)/physics_util/physics_util.a(mu_unbiased.o):\
  $(d0root)/physics_util/general/mu_unbiased.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mu_unbiased.f ;\
	mv mu_unbiased.o $(SCRATCH)/physics_util/a9/mu_unbiased.o ;\
	)
$(d0root)/physics_util/physics_util.a(muj_b.o):\
  $(d0root)/physics_util/filters/muj_b.f\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/muj_b.f ;\
	mv muj_b.o $(SCRATCH)/physics_util/a9/muj_b.o ;\
	)
$(d0root)/physics_util/physics_util.a(mujettion747.o):\
  $(d0root)/physics_util/general/mujets_isolation.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/mujets_mu_select.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mujets_isolation.f ;\
	mv mujets_isolation.o $(SCRATCH)/physics_util/a9/mujettion747.o ;\
	)
$(d0root)/physics_util/physics_util.a(mujetlect730.o):\
  $(d0root)/physics_util/general/mujets_mu_select.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/params/mujets_mu_select.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/mujets_mu_select.f ;\
	mv mujets_mu_select.o $(SCRATCH)/physics_util/a9/mujetlect730.o ;\
	)
$(d0root)/physics_util/physics_util.a(mulq_lect734.o):\
  $(d0root)/physics_util/filters/mulq_muon_select.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/mulq_muon_select.f ;\
	mv mulq_muon_select.o $(SCRATCH)/physics_util/a9/mulq_lect734.o ;\
	)
$(d0root)/physics_util/physics_util.a(mulq_ight373.o):\
  $(d0root)/physics_util/filters/mulq_muon_select_tight.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/mulq_muon_select_tight.f ;\
	mv mulq_muon_select_tight.o $(SCRATCH)/physics_util/a10/mulq_ight373.o ;\
	)
$(d0root)/physics_util/physics_util.a(multi_filter.o):\
  $(d0root)/physics_util/filters/multi_filter.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/multi_filter.f ;\
	mv multi_filter.o $(SCRATCH)/physics_util/a10/multi_filter.o ;\
	)
$(d0root)/physics_util/physics_util.a(multitool706.o):\
  $(d0root)/physics_util/general/multiple_interaction_tool.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/vertex_multi.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/multiple_interaction_tool.f ;\
	mv multiple_interaction_tool.o $(SCRATCH)/physics_util/a10/multitool706.o ;\
	)
$(d0root)/physics_util/physics_util.a(multirun1191.o):\
  $(d0root)/physics_util/general/multiple_interaction_tool_run1.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/vertex_multi.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/multiple_interaction_tool_run1.f ;\
	mv multiple_interaction_tool_run1.o $(SCRATCH)/physics_util/a10/multirun1191.o ;\
	)
$(d0root)/physics_util/physics_util.a(muon_t_np509.o):\
  $(d0root)/physics_util/filters/muon_select_np.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/muon_select_np.f ;\
	mv muon_select_np.o $(SCRATCH)/physics_util/a10/muon_t_np509.o ;\
	)
$(d0root)/physics_util/physics_util.a(muon_calc619.o):\
  $(d0root)/physics_util/general/muon_vtx_recalc.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/muon_vtx_recalc.f ;\
	mv muon_vtx_recalc.o $(SCRATCH)/physics_util/a10/muon_calc619.o ;\
	)
$(d0root)/physics_util/physics_util.a(new_gam_jet.o):\
  $(d0root)/physics_util/filters/new_gam_jet.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/new_gam_jet.f ;\
	mv new_gam_jet.o $(SCRATCH)/physics_util/a10/new_gam_jet.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_clean_tau.o):\
  $(d0root)/physics_util/filters/np_clean_tau.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_clean_tau.f ;\
	mv np_clean_tau.o $(SCRATCH)/physics_util/a10/np_clean_tau.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_dioose926.o):\
  $(d0root)/physics_util/filters/np_di_lepton_loose.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_di_lepton_loose.f ;\
	mv np_di_lepton_loose.o $(SCRATCH)/physics_util/a10/np_dioose926.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_diight924.o):\
  $(d0root)/physics_util/filters/np_di_lepton_tight.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_di_lepton_tight.f ;\
	mv np_di_lepton_tight.o $(SCRATCH)/physics_util/a10/np_diight924.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_had.o):\
  $(d0root)/physics_util/filters/np_had.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_had.f ;\
	mv np_had.o $(SCRATCH)/physics_util/a10/np_had.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_lep.o):\
  $(d0root)/physics_util/filters/np_lep.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_lep.f ;\
	mv np_lep.o $(SCRATCH)/physics_util/a10/np_lep.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_lq_2em.o):\
  $(d0root)/physics_util/filters/np_lq_2em.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_lq_2em.f ;\
	mv np_lq_2em.o $(SCRATCH)/physics_util/a10/np_lq_2em.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_lqight542.o):\
  $(d0root)/physics_util/filters/np_lq_2em_tight.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_lq_2em_tight.f ;\
	mv np_lq_2em_tight.o $(SCRATCH)/physics_util/a10/np_lqight542.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_lq_enu.o):\
  $(d0root)/physics_util/filters/np_lq_enu.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_lq_enu.f ;\
	mv np_lq_enu.o $(SCRATCH)/physics_util/a10/np_lq_enu.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_lslect400.o):\
  $(d0root)/physics_util/filters/np_lss_select.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_lss_select.f ;\
	mv np_lss_select.o $(SCRATCH)/physics_util/a10/np_lslect400.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_lss_tight.o):\
  $(d0root)/physics_util/filters/np_lss_tight.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_lss_tight.f ;\
	mv np_lss_tight.o $(SCRATCH)/physics_util/a10/np_lss_tight.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_lsoose057.o):\
  $(d0root)/physics_util/filters/np_lss_trilep_loose.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/links/izjpts.link\
  $(d0library)/links/izjtsh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_lss_trilep_loose.f ;\
	mv np_lss_trilep_loose.o $(SCRATCH)/physics_util/a10/np_lsoose057.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_lsight055.o):\
  $(d0root)/physics_util/filters/np_lss_trilep_tight.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/links/izjpts.link\
  $(d0library)/links/izjtsh.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_lss_trilep_tight.f ;\
	mv np_lss_trilep_tight.o $(SCRATCH)/physics_util/a10/np_lsight055.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_met.o):\
  $(d0root)/physics_util/filters/np_met.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_met.f ;\
	mv np_met.o $(SCRATCH)/physics_util/a10/np_met.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_msp.o):\
  $(d0root)/physics_util/filters/np_msp.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_msp.f ;\
	mv np_msp.o $(SCRATCH)/physics_util/a11/np_msp.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_msheck589.o):\
  $(d0root)/physics_util/filters/np_msp_mu_check.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_msp_mu_check.f ;\
	mv np_msp_mu_check.o $(SCRATCH)/physics_util/a11/np_msheck589.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_mslect719.o):\
  $(d0root)/physics_util/filters/np_msp_mu_select.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_msp_mu_select.f ;\
	mv np_msp_mu_select.o $(SCRATCH)/physics_util/a11/np_mslect719.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_mulq.o):\
  $(d0root)/physics_util/filters/np_mulq.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpmuo.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_mulq.f ;\
	mv np_mulq.o $(SCRATCH)/physics_util/a11/np_mulq.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_muight413.o):\
  $(d0root)/physics_util/filters/np_mulq_tight.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpmuo.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_mulq_tight.f ;\
	mv np_mulq_tight.o $(SCRATCH)/physics_util/a11/np_muight413.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_munu.o):\
  $(d0root)/physics_util/filters/np_munu.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpnut.link\
  $(d0library)/links/izpmuo.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_munu.f ;\
	mv np_munu.o $(SCRATCH)/physics_util/a11/np_munu.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_scalar.o):\
  $(d0root)/physics_util/filters/np_scalar.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_scalar.f ;\
	mv np_scalar.o $(SCRATCH)/physics_util/a11/np_scalar.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_scight596.o):\
  $(d0root)/physics_util/filters/np_scalar_tight.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_scalar_tight.f ;\
	mv np_scalar_tight.o $(SCRATCH)/physics_util/a11/np_scight596.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_sqgl.o):\
  $(d0root)/physics_util/filters/np_sqgl.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_sqgl.f ;\
	mv np_sqgl.o $(SCRATCH)/physics_util/a11/np_sqgl.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_sqight405.o):\
  $(d0root)/physics_util/filters/np_sqgl_tight.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izhmte.link\
  $(d0library)/links/izhmtp.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_sqgl_tight.f ;\
	mv np_sqgl_tight.o $(SCRATCH)/physics_util/a11/np_sqight405.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_tau.o):\
  $(d0root)/physics_util/filters/np_tau.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_tau.f ;\
	mv np_tau.o $(SCRATCH)/physics_util/a11/np_tau.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_wright.o):\
  $(d0root)/physics_util/filters/np_wright.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_wright.f ;\
	mv np_wright.o $(SCRATCH)/physics_util/a11/np_wright.o ;\
	)
$(d0root)/physics_util/physics_util.a(np_wright627.o):\
  $(d0root)/physics_util/filters/np_wright_tight.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/np_wright_tight.f ;\
	mv np_wright_tight.o $(SCRATCH)/physics_util/a11/np_wright627.o ;\
	)
$(d0root)/physics_util/physics_util.a(object_muon1.o):\
  $(d0root)/physics_util/general/object_muon1.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/object_muon1.f ;\
	mv object_muon1.o $(SCRATCH)/physics_util/a11/object_muon1.o ;\
	)
$(d0root)/physics_util/physics_util.a(ordx.o):\
  $(d0root)/physics_util/general/ordx.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/ordx.f ;\
	mv ordx.o $(SCRATCH)/physics_util/a11/ordx.o ;\
	)
$(d0root)/physics_util/physics_util.a(partilect597.o):\
  $(d0root)/physics_util/general/particle_select.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/particle_select.f ;\
	mv particle_select.o $(SCRATCH)/physics_util/a11/partilect597.o ;\
	)
$(d0root)/physics_util/physics_util.a(phi_dence461.o):\
  $(d0root)/physics_util/filters/phi_difference.f\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/phi_difference.f ;\
	mv phi_difference.o $(SCRATCH)/physics_util/a11/phi_dence461.o ;\
	)
$(d0root)/physics_util/physics_util.a(phys_cor.o):\
  $(d0root)/physics_util/general/phys_cor.f\
  $(d0library)/params/cal_offline.params\
  $(d0library)/params/l1php.params\
  $(d0library)/inc/cl2_rings.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/phys_cor.f ;\
	mv phys_cor.o $(SCRATCH)/physics_util/a11/phys_cor.o ;\
	)
$(d0root)/physics_util/physics_util.a(planarity.o):\
  $(d0root)/physics_util/general/planarity.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/planarity.f ;\
	mv planarity.o $(SCRATCH)/physics_util/a11/planarity.o ;\
	)
$(d0root)/physics_util/physics_util.a(prnt_bank.o):\
  $(d0root)/physics_util/dbank/prnt_bank.f\
  $(d0library)/inc/autof.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/dbank/prnt_bank.f ;\
	mv prnt_bank.o $(SCRATCH)/physics_util/a11/prnt_bank.o ;\
	)
$(d0root)/physics_util/physics_util.a(ptfix.o):\
  $(d0root)/physics_util/filters/ptfix.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/ptfix.f ;\
	mv ptfix.o $(SCRATCH)/physics_util/a12/ptfix.o ;\
	)
$(d0root)/physics_util/physics_util.a(ptopt.o):\
  $(d0root)/physics_util/udst/ptopt.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izvert.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/ptopt.f ;\
	mv ptopt.o $(SCRATCH)/physics_util/a12/ptopt.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_eta_bias.o):\
  $(d0root)/physics_util/general/qcd_eta_bias.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/qcd_eta_bias.f ;\
	mv qcd_eta_bias.o $(SCRATCH)/physics_util/a12/qcd_eta_bias.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_eesol375.o):\
  $(d0root)/physics_util/general/qcd_eta_resol.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/qcd_eta_resol.f ;\
	mv qcd_eta_resol.o $(SCRATCH)/physics_util/a12/qcd_eesol375.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_fring606.o):\
  $(d0root)/physics_util/filters/qcd_filt_string.f\
  $(d0library)/inc/qcd_bit_names.inc\
  $(d0library)/inc/qcd_bit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/qcd_filt_string.f ;\
	mv qcd_filt_string.o $(SCRATCH)/physics_util/a12/qcd_fring606.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_f_bit477.o):\
  $(d0root)/physics_util/udst/qcd_filter_bit.f\
  $(d0library)/inc/qcd_bit_names.inc\
  $(d0library)/inc/qcd_bit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/qcd_filter_bit.f ;\
	mv qcd_filter_bit.o $(SCRATCH)/physics_util/a12/qcd_f_bit477.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_finit008.o):\
  $(d0root)/physics_util/udst/qcd_filter_bit_init.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/qcd_bit_names.inc\
  $(d0library)/inc/qcd_bit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/qcd_filter_bit_init.f ;\
	mv qcd_filter_bit_init.o $(SCRATCH)/physics_util/a12/qcd_finit008.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_fgger914.o):\
  $(d0root)/physics_util/udst/qcd_filter_trigger.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/esum.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/qcd_filter_trigger.f ;\
	mv qcd_filter_trigger.o $(SCRATCH)/physics_util/a12/qcd_fgger914.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_gamma.o):\
  $(d0root)/physics_util/filters/qcd_gamma.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/qcd_gamma.f ;\
	mv qcd_gamma.o $(SCRATCH)/physics_util/a12/qcd_gamma.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_gap.o):\
  $(d0root)/physics_util/filters/qcd_gap.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/qcd_gap.f ;\
	mv qcd_gap.o $(SCRATCH)/physics_util/a12/qcd_gap.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_get_mask.o):\
  $(d0root)/physics_util/udst/qcd_get_mask.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/qcd_bit_names.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/qcd_get_mask.f ;\
	mv qcd_get_mask.o $(SCRATCH)/physics_util/a12/qcd_get_mask.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_jtion915.o):\
  $(d0root)/physics_util/general/qcd_jet_correction.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/qcd_jet_correction.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/qcd_jet_correction.f ;\
	mv qcd_jet_correction.o $(SCRATCH)/physics_util/a12/qcd_jtion915.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_jjj.o):\
  $(d0root)/physics_util/filters/qcd_jjj.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/qcd_jjj.f ;\
	mv qcd_jjj.o $(SCRATCH)/physics_util/a12/qcd_jjj.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_make_dst.o):\
  $(d0root)/physics_util/udst/qcd_make_dst.f\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izanls.link\
  $(d0library)/links/izcaeq.link\
  $(d0library)/links/izcaid.link\
  $(d0library)/links/izjtcs.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/qcd_make_dst.f ;\
	mv qcd_make_dst.o $(SCRATCH)/physics_util/a12/qcd_make_dst.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_nol.o):\
  $(d0root)/physics_util/filters/qcd_nol.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/qcd_nol.f ;\
	mv qcd_nol.o $(SCRATCH)/physics_util/a12/qcd_nol.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_ntr.o):\
  $(d0root)/physics_util/filters/qcd_ntr.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/qcd_ntr.f ;\
	mv qcd_ntr.o $(SCRATCH)/physics_util/a12/qcd_ntr.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_qjt.o):\
  $(d0root)/physics_util/filters/qcd_qjt.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/qcd_qjt.f ;\
	mv qcd_qjt.o $(SCRATCH)/physics_util/a12/qcd_qjt.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_sd.o):\
  $(d0root)/physics_util/filters/qcd_sd.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/qcd_sd.f ;\
	mv qcd_sd.o $(SCRATCH)/physics_util/a12/qcd_sd.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcd_sta.o):\
  $(d0root)/physics_util/filters/qcd_sta.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/qcd_sta.f ;\
	mv qcd_sta.o $(SCRATCH)/physics_util/a12/qcd_sta.o ;\
	)
$(d0root)/physics_util/physics_util.a(qcdjets.o):\
  $(d0root)/physics_util/filters/qcdjets.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/qcd_bit_names.inc\
  $(d0library)/inc/qcd_bit.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/qcdjets.f ;\
	mv qcdjets.o $(SCRATCH)/physics_util/a12/qcdjets.o ;\
	)
$(d0root)/physics_util/physics_util.a(recalc_muon.o):\
  $(d0root)/physics_util/general/recalc_muon.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/recalc_muon.f ;\
	mv recalc_muon.o $(SCRATCH)/physics_util/a13/recalc_muon.o ;\
	)
$(d0root)/physics_util/physics_util.a(reco_esum.o):\
  $(d0root)/physics_util/general/reco_esum.f\
  $(d0library)/params/esum.params\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izvert.link\
  $(d0library)/links/izcaph.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/reco_esum.f ;\
	mv reco_esum.o $(SCRATCH)/physics_util/a13/reco_esum.o ;\
	)
$(d0root)/physics_util/physics_util.a(reco_fill493.o):\
  $(d0root)/physics_util/general/reco_hsum_fill.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/reco_hsum_fill.f ;\
	mv reco_hsum_fill.o $(SCRATCH)/physics_util/a13/reco_fill493.o ;\
	)
$(d0root)/physics_util/physics_util.a(reco_lect039.o):\
  $(d0root)/physics_util/general/reco_version_select.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/reco_version_select.f ;\
	mv reco_version_select.o $(SCRATCH)/physics_util/a13/reco_lect039.o ;\
	)
$(d0root)/physics_util/physics_util.a(run_select.o):\
  $(d0root)/physics_util/general/run_select.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/run_select.f ;\
	mv run_select.o $(SCRATCH)/physics_util/a13/run_select.o ;\
	)
$(d0root)/physics_util/physics_util.a(sel_ejets371.o):\
  $(d0root)/physics_util/general/sel_elec_jets.f\
  $(d0library)/params/isajet_codes.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/sel_elec_jets.f ;\
	mv sel_elec_jets.o $(SCRATCH)/physics_util/a13/sel_ejets371.o ;\
	)
$(d0root)/physics_util/physics_util.a(set_caph_alg.o):\
  $(d0root)/physics_util/general/set_caph_alg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/set_caph_alg.f ;\
	mv set_caph_alg.o $(SCRATCH)/physics_util/a13/set_caph_alg.o ;\
	)
$(d0root)/physics_util/physics_util.a(set_jet.o):\
  $(d0root)/physics_util/general/set_jet.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/set_jet.f ;\
	mv set_jet.o $(SCRATCH)/physics_util/a13/set_jet.o ;\
	)
$(d0root)/physics_util/physics_util.a(set_ttnum131.o):\
  $(d0root)/physics_util/general/set_traceback_evtnum.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/set_traceback_evtnum.f ;\
	mv set_traceback_evtnum.o $(SCRATCH)/physics_util/a13/set_ttnum131.o ;\
	)
$(d0root)/physics_util/physics_util.a(sm_jector157.o):\
  $(d0root)/physics_util/general/sm_jet_undzsp_factor.f\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/sm_jet_undzsp_factor.f ;\
	mv sm_jet_undzsp_factor.o $(SCRATCH)/physics_util/a13/sm_jector157.o ;\
	)
$(d0root)/physics_util/physics_util.a(sort_descend.o):\
  $(d0root)/physics_util/filters/sort_descend.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/sort_descend.f ;\
	mv sort_descend.o $(SCRATCH)/physics_util/a13/sort_descend.o ;\
	)
$(d0root)/physics_util/physics_util.a(sort_vect.o):\
  $(d0root)/physics_util/general/sort_vect.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/sort_vect.f ;\
	mv sort_vect.o $(SCRATCH)/physics_util/a13/sort_vect.o ;\
	)
$(d0root)/physics_util/physics_util.a(sortbanks.o):\
  $(d0root)/physics_util/filters/sortbanks.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/sortbanks.f ;\
	mv sortbanks.o $(SCRATCH)/physics_util/a13/sortbanks.o ;\
	)
$(d0root)/physics_util/physics_util.a(sphericity.o):\
  $(d0root)/physics_util/general/sphericity.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/sphericity.f ;\
	mv sphericity.o $(SCRATCH)/physics_util/a13/sphericity.o ;\
	)
$(d0root)/physics_util/physics_util.a(sphericity0.o):\
  $(d0root)/physics_util/general/sphericity0.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/sphericity0.f ;\
	mv sphericity0.o $(SCRATCH)/physics_util/a13/sphericity0.o ;\
	)
$(d0root)/physics_util/physics_util.a(splitmask408.o):\
  $(d0root)/physics_util/udst/split_bitmask.f\
  $(d0library)/params/byte_order.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/split_bitmask.f ;\
	mv split_bitmask.o $(SCRATCH)/physics_util/a13/splitmask408.o ;\
	)
$(d0root)/physics_util/physics_util.a(ss_mu_check.o):\
  $(d0root)/physics_util/filters/ss_mu_check.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/ss_mu_check.f ;\
	mv ss_mu_check.o $(SCRATCH)/physics_util/a13/ss_mu_check.o ;\
	)
$(d0root)/physics_util/physics_util.a(ss_mu_chk.o):\
  $(d0root)/physics_util/filters/ss_mu_chk.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/ss_mu_chk.f ;\
	mv ss_mu_chk.o $(SCRATCH)/physics_util/a13/ss_mu_chk.o ;\
	)
$(d0root)/physics_util/physics_util.a(ss_mu_sel.o):\
  $(d0root)/physics_util/filters/ss_mu_sel.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/ss_mu_sel.f ;\
	mv ss_mu_sel.o $(SCRATCH)/physics_util/a13/ss_mu_sel.o ;\
	)
$(d0root)/physics_util/physics_util.a(ss_mu_select.o):\
  $(d0root)/physics_util/filters/ss_mu_select.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/ss_mu_select.f ;\
	mv ss_mu_select.o $(SCRATCH)/physics_util/a13/ss_mu_select.o ;\
	)
$(d0root)/physics_util/physics_util.a(stream_ejets.o):\
  $(d0root)/physics_util/filters/stream_ejets.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/stream_ejets.f ;\
	mv stream_ejets.o $(SCRATCH)/physics_util/a14/stream_ejets.o ;\
	)
$(d0root)/physics_util/physics_util.a(tag_mlect382.o):\
  $(d0root)/physics_util/general/tag_mu_select.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/tag_mu_select.f ;\
	mv tag_mu_select.o $(SCRATCH)/physics_util/a14/tag_mlect382.o ;\
	)
$(d0root)/physics_util/physics_util.a(tau_filter.o):\
  $(d0root)/physics_util/filters/tau_filter.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/tau_filter.f ;\
	mv tau_filter.o $(SCRATCH)/physics_util/a14/tau_filter.o ;\
	)
$(d0root)/physics_util/physics_util.a(threelter610.o):\
  $(d0root)/physics_util/filters/threejet_filter.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/threejet_filter.f ;\
	mv threejet_filter.o $(SCRATCH)/physics_util/a14/threelter610.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_bkg.o):\
  $(d0root)/physics_util/filters/top_bkg.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_bkg.f ;\
	mv top_bkg.o $(SCRATCH)/physics_util/a14/top_bkg.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_diem.o):\
  $(d0root)/physics_util/filters/top_diem.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_diem.f ;\
	mv top_diem.o $(SCRATCH)/physics_util/a14/top_diem.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_ee.o):\
  $(d0root)/physics_util/filters/top_ee.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_ee.f ;\
	mv top_ee.o $(SCRATCH)/physics_util/a14/top_ee.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_ejbkg.o):\
  $(d0root)/physics_util/filters/top_ejbkg.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_ejbkg.f ;\
	mv top_ejbkg.o $(SCRATCH)/physics_util/a14/top_ejbkg.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_ejet.o):\
  $(d0root)/physics_util/filters/top_ejet.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_ejet.f ;\
	mv top_ejet.o $(SCRATCH)/physics_util/a14/top_ejet.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_ejsig.o):\
  $(d0root)/physics_util/filters/top_ejsig.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_ejsig.f ;\
	mv top_ejsig.o $(SCRATCH)/physics_util/a14/top_ejsig.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_emu.o):\
  $(d0root)/physics_util/filters/top_emu.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_emu.f ;\
	mv top_emu.o $(SCRATCH)/physics_util/a14/top_emu.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_eanup829.o):\
  $(d0root)/physics_util/general/top_event_cleanup.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/top_event_cleanup.f ;\
	mv top_event_cleanup.o $(SCRATCH)/physics_util/a14/top_eanup829.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_jets.o):\
  $(d0root)/physics_util/filters/top_jets.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_jets.f ;\
	mv top_jets.o $(SCRATCH)/physics_util/a14/top_jets.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_muj.o):\
  $(d0root)/physics_util/filters/top_muj.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_muj.f ;\
	mv top_muj.o $(SCRATCH)/physics_util/a14/top_muj.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_mujet.o):\
  $(d0root)/physics_util/filters/top_mujet.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_mujet.f ;\
	mv top_mujet.o $(SCRATCH)/physics_util/a14/top_mujet.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_mumu.o):\
  $(d0root)/physics_util/filters/top_mumu.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_mumu.f ;\
	mv top_mumu.o $(SCRATCH)/physics_util/a14/top_mumu.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_sgl.o):\
  $(d0root)/physics_util/filters/top_sgl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_sgl.f ;\
	mv top_sgl.o $(SCRATCH)/physics_util/a14/top_sgl.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_tbbar383.o):\
  $(d0root)/physics_util/filters/top_top_bbbar.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_top_bbbar.f ;\
	mv top_top_bbbar.o $(SCRATCH)/physics_util/a14/top_tbbar383.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_top_ee.o):\
  $(d0root)/physics_util/filters/top_top_ee.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_top_ee.f ;\
	mv top_top_ee.o $(SCRATCH)/physics_util/a14/top_top_ee.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_top_ejet.o):\
  $(d0root)/physics_util/filters/top_top_ejet.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_top_ejet.f ;\
	mv top_top_ejet.o $(SCRATCH)/physics_util/a14/top_top_ejet.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_top_emu.o):\
  $(d0root)/physics_util/filters/top_top_emu.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_top_emu.f ;\
	mv top_top_emu.o $(SCRATCH)/physics_util/a15/top_top_emu.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_top_jets.o):\
  $(d0root)/physics_util/filters/top_top_jets.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_top_jets.f ;\
	mv top_top_jets.o $(SCRATCH)/physics_util/a15/top_top_jets.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_tujet427.o):\
  $(d0root)/physics_util/filters/top_top_mujet.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_top_mujet.f ;\
	mv top_top_mujet.o $(SCRATCH)/physics_util/a15/top_tujet427.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_top_mumu.o):\
  $(d0root)/physics_util/filters/top_top_mumu.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_top_mumu.f ;\
	mv top_top_mumu.o $(SCRATCH)/physics_util/a15/top_top_mumu.o ;\
	)
$(d0root)/physics_util/physics_util.a(top_top_sgl.o):\
  $(d0root)/physics_util/filters/top_top_sgl.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/top_top_sgl.f ;\
	mv top_top_sgl.o $(SCRATCH)/physics_util/a15/top_top_sgl.o ;\
	)
$(d0root)/physics_util/physics_util.a(topup.o):\
  $(d0root)/physics_util/general/topup.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/topup.f ;\
	mv topup.o $(SCRATCH)/physics_util/a15/topup.o ;\
	)
$(d0root)/physics_util/physics_util.a(topup_err405.o):\
  $(d0root)/physics_util/general/topup_bkg_err.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/topup_bkg_err.f ;\
	mv topup_bkg_err.o $(SCRATCH)/physics_util/a15/topup_err405.o ;\
	)
$(d0root)/physics_util/physics_util.a(transmass642.o):\
  $(d0root)/physics_util/general/transverse_mass.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/transverse_mass.f ;\
	mv transverse_mass.o $(SCRATCH)/physics_util/a15/transmass642.o ;\
	)
$(d0root)/physics_util/physics_util.a(triggheck785.o):\
  $(d0root)/physics_util/general/trigger_bit_check.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/trigger_bit_check.f ;\
	mv trigger_bit_check.o $(SCRATCH)/physics_util/a15/triggheck785.o ;\
	)
$(d0root)/physics_util/physics_util.a(tsum_fill.o):\
  $(d0root)/physics_util/general/tsum_fill.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/tsum_fill.f ;\
	mv tsum_fill.o $(SCRATCH)/physics_util/a15/tsum_fill.o ;\
	)
$(d0root)/physics_util/physics_util.a(tsum_names.o):\
  $(d0root)/physics_util/general/tsum_names.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/tsum_names.f ;\
	mv tsum_names.o $(SCRATCH)/physics_util/a15/tsum_names.o ;\
	)
$(d0root)/physics_util/physics_util.a(twojelter420.o):\
  $(d0root)/physics_util/filters/twojet_filter.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/twojet_filter.f ;\
	mv twojet_filter.o $(SCRATCH)/physics_util/a15/twojelter420.o ;\
	)
$(d0root)/physics_util/physics_util.a(ucshfl.o):\
  $(d0root)/physics_util/udst/ucshfl.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/ucshfl.f ;\
	mv ucshfl.o $(SCRATCH)/physics_util/a15/ucshfl.o ;\
	)
$(d0root)/physics_util/physics_util.a(ucwxfl.o):\
  $(d0root)/physics_util/udst/ucwxfl.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/ucwxfl.f ;\
	mv ucwxfl.o $(SCRATCH)/physics_util/a15/ucwxfl.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst.o):\
  $(d0root)/physics_util/udst/udst.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/iztrgr.link\
  $(d0library)/links/izmud1.link\
  $(d0library)/links/izcdd1.link\
  $(d0library)/links/izcdd2.link\
  $(d0library)/links/izcdd3.link\
  $(d0library)/links/izcdd4.link\
  $(d0library)/links/izcad1.link\
  $(d0library)/links/izcad2.link\
  $(d0library)/links/izreco.link\
  $(d0library)/links/izfilt.link\
  $(d0library)/links/izhsum.link\
  $(d0library)/links/izesum.link\
  $(d0library)/links/iztsum.link\
  $(d0library)/links/izucsh.link\
  $(d0library)/links/izutag.link\
  $(d0library)/links/izuint.link\
  $(d0library)/links/izcdtk.link\
  $(d0library)/links/izucwx.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst.f ;\
	mv udst.o $(SCRATCH)/physics_util/a15/udst.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_roup632.o):\
  $(d0root)/physics_util/udst/udst_book_group.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/udst_dimensions.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_book_group.f ;\
	mv udst_book_group.o $(SCRATCH)/physics_util/a15/udst_roup632.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_pack249.o):\
  $(d0root)/physics_util/udst/udst_cdtk_errors_pack.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_cdtk_errors_pack.f ;\
	mv udst_cdtk_errors_pack.o $(SCRATCH)/physics_util/a15/udst_pack249.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_pack476.o):\
  $(d0root)/physics_util/udst/udst_cdtk_errors_unpack.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_cdtk_errors_unpack.f ;\
	mv udst_cdtk_errors_unpack.o $(SCRATCH)/physics_util/a15/udst_pack476.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_fin.o):\
  $(d0root)/physics_util/udst/udst_fin.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_fin.f ;\
	mv udst_fin.o $(SCRATCH)/physics_util/a15/udst_fin.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_bank387.o):\
  $(d0root)/physics_util/udst/udst_fix_bank.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_fix_bank.f ;\
	mv udst_fix_bank.o $(SCRATCH)/physics_util/a15/udst_bank387.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_jets413.o):\
  $(d0root)/physics_util/udst/udst_fix_jets.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_fix_jets.f ;\
	mv udst_fix_jets.o $(SCRATCH)/physics_util/a16/udst_jets413.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_pelc395.o):\
  $(d0root)/physics_util/udst/udst_fix_pelc.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_fix_pelc.f ;\
	mv udst_fix_pelc.o $(SCRATCH)/physics_util/a16/udst_pelc395.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_pmuo424.o):\
  $(d0root)/physics_util/udst/udst_fix_pmuo.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_fix_pmuo.f ;\
	mv udst_fix_pmuo.o $(SCRATCH)/physics_util/a16/udst_pmuo424.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_pnut430.o):\
  $(d0root)/physics_util/udst/udst_fix_pnut.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_fix_pnut.f ;\
	mv udst_fix_pnut.o $(SCRATCH)/physics_util/a16/udst_pnut430.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_ptau417.o):\
  $(d0root)/physics_util/udst/udst_fix_ptau.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_fix_ptau.f ;\
	mv udst_fix_ptau.o $(SCRATCH)/physics_util/a16/udst_ptau417.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_vtxt445.o):\
  $(d0root)/physics_util/udst/udst_fix_vtxt.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_fix_vtxt.f ;\
	mv udst_fix_vtxt.o $(SCRATCH)/physics_util/a16/udst_vtxt445.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_cash383.o):\
  $(d0root)/physics_util/udst/udst_get_cash.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_cash.f ;\
	mv udst_get_cash.o $(SCRATCH)/physics_util/a16/udst_cash383.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_cawx403.o):\
  $(d0root)/physics_util/udst/udst_get_cawx.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcawx.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_cawx.f ;\
	mv udst_get_cawx.o $(SCRATCH)/physics_util/a16/udst_cawx403.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_clus617.o):\
  $(d0root)/physics_util/udst/udst_get_emclus.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izvert.link\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zlinkc.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_emclus.f ;\
	mv udst_get_emclus.o $(SCRATCH)/physics_util/a16/udst_clus617.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_glob388.o):\
  $(d0root)/physics_util/udst/udst_get_glob.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izfilt.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_glob.f ;\
	mv udst_get_glob.o $(SCRATCH)/physics_util/a16/udst_glob388.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_head370.o):\
  $(d0root)/physics_util/udst/udst_get_head.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_head.f ;\
	mv udst_get_head.o $(SCRATCH)/physics_util/a16/udst_head370.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_jets406.o):\
  $(d0root)/physics_util/udst/udst_get_jets.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_jets.f ;\
	mv udst_get_jets.o $(SCRATCH)/physics_util/a16/udst_jets406.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_jnep397.o):\
  $(d0root)/physics_util/udst/udst_get_jnep.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_jnep.f ;\
	mv udst_get_jnep.o $(SCRATCH)/physics_util/a16/udst_jnep397.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst__met596.o):\
  $(d0root)/physics_util/udst/udst_get_l12_met.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/esum.params
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_l12_met.f ;\
	mv udst_get_l12_met.o $(SCRATCH)/physics_util/a16/udst__met596.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_gpol608.o):\
  $(d0root)/physics_util/udst/udst_get_magpol.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_magpol.f ;\
	mv udst_get_magpol.o $(SCRATCH)/physics_util/a16/udst_gpol608.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_muon415.o):\
  $(d0root)/physics_util/udst/udst_get_muon.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_muon.f ;\
	mv udst_get_muon.o $(SCRATCH)/physics_util/a16/udst_muon415.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_link913.o):\
  $(d0root)/physics_util/udst/udst_get_pelc_link.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpelc.link\
  $(d0library)/links/izppho.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_pelc_link.f ;\
	mv udst_get_pelc_link.o $(SCRATCH)/physics_util/a16/udst_link913.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_pnu1356.o):\
  $(d0root)/physics_util/udst/udst_get_pnu1.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_pnu1.f ;\
	mv udst_get_pnu1.o $(SCRATCH)/physics_util/a16/udst_pnu1356.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_pnut423.o):\
  $(d0root)/physics_util/udst/udst_get_pnut.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_pnut.f ;\
	mv udst_get_pnut.o $(SCRATCH)/physics_util/a16/udst_pnut423.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_ptau410.o):\
  $(d0root)/physics_util/udst/udst_get_ptau.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_ptau.f ;\
	mv udst_get_ptau.o $(SCRATCH)/physics_util/a16/udst_ptau410.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_sahh388.o):\
  $(d0root)/physics_util/udst/udst_get_sahh.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_sahh.f ;\
	mv udst_get_sahh.o $(SCRATCH)/physics_util/a17/udst_sahh388.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_link026.o):\
  $(d0root)/physics_util/udst/udst_get_track_link.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/udst_links.inc\
  $(d0library)/links/izztrk.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_track_link.f ;\
	mv udst_get_track_link.o $(SCRATCH)/physics_util/a17/udst_link026.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_trak402.o):\
  $(d0root)/physics_util/udst/udst_get_trak.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_trak.f ;\
	mv udst_get_trak.o $(SCRATCH)/physics_util/a17/udst_trak402.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_sion742.o):\
  $(d0root)/physics_util/udst/udst_get_version.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_version.f ;\
	mv udst_get_version.o $(SCRATCH)/physics_util/a17/udst_sion742.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_vert417.o):\
  $(d0root)/physics_util/udst/udst_get_vert.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_vert.f ;\
	mv udst_get_vert.o $(SCRATCH)/physics_util/a17/udst_vert417.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_link942.o):\
  $(d0root)/physics_util/udst/udst_get_vert_link.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/udst_links.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_vert_link.f ;\
	mv udst_get_vert_link.o $(SCRATCH)/physics_util/a17/udst_link942.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_vfit409.o):\
  $(d0root)/physics_util/udst/udst_get_vfit.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_vfit.f ;\
	mv udst_get_vfit.o $(SCRATCH)/physics_util/a17/udst_vfit409.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_vtxt438.o):\
  $(d0root)/physics_util/udst/udst_get_vtxt.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_vtxt.f ;\
	mv udst_get_vtxt.o $(SCRATCH)/physics_util/a17/udst_vtxt438.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_link963.o):\
  $(d0root)/physics_util/udst/udst_get_vtxt_link.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/udst_links.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_get_vtxt_link.f ;\
	mv udst_get_vtxt_link.o $(SCRATCH)/physics_util/a17/udst_link963.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_ini.o):\
  $(d0root)/physics_util/udst/udst_ini.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_ini.f ;\
	mv udst_ini.o $(SCRATCH)/physics_util/a17/udst_ini.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_out.o):\
  $(d0root)/physics_util/udst/udst_out.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izreco.link\
  $(d0library)/links/izfilt.link\
  $(d0library)/links/izhsum.link\
  $(d0library)/links/izesum.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_out.f ;\
	mv udst_out.o $(SCRATCH)/physics_util/a17/udst_out.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_tag.o):\
  $(d0root)/physics_util/udst/udst_tag.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/udst_dimensions.params\
  $(d0library)/inc/utag_data.inc\
  $(d0library)/inc/utag_versions.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_tag.f ;\
	mv udst_tag.o $(SCRATCH)/physics_util/a17/udst_tag.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_to_dst.o):\
  $(d0root)/physics_util/udst/udst_to_dst.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/inc/utag_data.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_to_dst.f ;\
	mv udst_to_dst.o $(SCRATCH)/physics_util/a17/udst_to_dst.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_fill724.o):\
  $(d0root)/physics_util/udst/udst_to_dst_fill.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/utag_data.inc\
  $(d0library)/params/caph.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_to_dst_fill.f ;\
	mv udst_to_dst_fill.o $(SCRATCH)/physics_util/a17/udst_fill724.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_bank231.o):\
  $(d0root)/physics_util/udst/udst_to_dst_fill_bank.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_to_dst_fill_bank.f ;\
	mv udst_to_dst_fill_bank.o $(SCRATCH)/physics_util/a17/udst_bank231.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_init737.o):\
  $(d0root)/physics_util/udst/udst_to_dst_init.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izutag.link\
  $(d0library)/links/izanls.link\
  $(d0library)/inc/utag_data.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_to_dst_init.f ;\
	mv udst_to_dst_init.o $(SCRATCH)/physics_util/a17/udst_init737.o ;\
	)
$(d0root)/physics_util/physics_util.a(udst_sion064.o):\
  $(d0root)/physics_util/udst/udst_unpack_version.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udst_unpack_version.f ;\
	mv udst_unpack_version.o $(SCRATCH)/physics_util/a17/udst_sion064.o ;\
	)
$(d0root)/physics_util/physics_util.a(udstfl.o):\
  $(d0root)/physics_util/udst/udstfl.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/udstfl.f ;\
	mv udstfl.o $(SCRATCH)/physics_util/a17/udstfl.o ;\
	)
$(d0root)/physics_util/physics_util.a(uint_to_dst.o):\
  $(d0root)/physics_util/udst/uint_to_dst.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izuint.link
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/uint_to_dst.f ;\
	mv uint_to_dst.o $(SCRATCH)/physics_util/a17/uint_to_dst.o ;\
	)
$(d0root)/physics_util/physics_util.a(uncorbank031.o):\
  $(d0root)/physics_util/general/uncorrect_jets_bank.f\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/uncorrect_jets_bank.f ;\
	mv uncorrect_jets_bank.o $(SCRATCH)/physics_util/a17/uncorbank031.o ;\
	)
$(d0root)/physics_util/physics_util.a(uni_filter.o):\
  $(d0root)/physics_util/filters/uni_filter.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/uni_filter.f ;\
	mv uni_filter.o $(SCRATCH)/physics_util/a18/uni_filter.o ;\
	)
$(d0root)/physics_util/physics_util.a(unix_tall857.o):\
  $(d0root)/physics_util/general/unix_util_install.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/unix_util_install.f ;\
	mv unix_util_install.o $(SCRATCH)/physics_util/a18/unix_tall857.o ;\
	)
$(d0root)/physics_util/physics_util.a(utag_udst517.o):\
  $(d0root)/physics_util/udst/utag_from_udst.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/utag_data.inc\
  $(d0library)/inc/utag_versions.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/utag_from_udst.f ;\
	mv utag_from_udst.o $(SCRATCH)/physics_util/a18/utag_udst517.o ;\
	)
$(d0root)/physics_util/physics_util.a(utagfl.o):\
  $(d0root)/physics_util/udst/utagfl.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/udst/utagfl.f ;\
	mv utagfl.o $(SCRATCH)/physics_util/a18/utagfl.o ;\
	)
$(d0root)/physics_util/physics_util.a(vbrowse.o):\
  $(d0root)/physics_util/dbank/vbrowse.f\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/trmdef.def\
  $(d0library)/inc/autof.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/dbank/vbrowse.f ;\
	mv vbrowse.o $(SCRATCH)/physics_util/a18/vbrowse.o ;\
	)
$(d0root)/physics_util/physics_util.a(verify_zebra.o):\
  $(d0root)/physics_util/general/verify_zebra.f\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/quest.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/verify_zebra.f ;\
	mv verify_zebra.o $(SCRATCH)/physics_util/a18/verify_zebra.o ;\
	)
$(d0root)/physics_util/physics_util.a(vertelter421.o):\
  $(d0root)/physics_util/filters/vertex_filter.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/filters/vertex_filter.f ;\
	mv vertex_filter.o $(SCRATCH)/physics_util/a18/vertelter421.o ;\
	)
$(d0root)/physics_util/physics_util.a(zb_smg_pn1.o):\
  $(d0root)/physics_util/dbank/zb_smg_pn1.f\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/trmdef.def\
  $(d0library)/inc/autof.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/dbank/zb_smg_pn1.f ;\
	mv zb_smg_pn1.o $(SCRATCH)/physics_util/a18/zb_smg_pn1.o ;\
	)
$(d0root)/physics_util/physics_util.a(zb_smg_pn2.o):\
  $(d0root)/physics_util/dbank/zb_smg_pn2.f\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/trmdef.def\
  $(d0library)/inc/autof.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/dbank/zb_smg_pn2.f ;\
	mv zb_smg_pn2.o $(SCRATCH)/physics_util/a18/zb_smg_pn2.o ;\
	)
$(d0root)/physics_util/physics_util.a(zbank_edit.o):\
  $(d0root)/physics_util/general/zbank_edit.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/zbank_edit.f ;\
	mv zbank_edit.o $(SCRATCH)/physics_util/a18/zbank_edit.o ;\
	)
$(d0root)/physics_util/physics_util.a(zbank_tag366.o):\
  $(d0root)/physics_util/general/zbank_hfn_tag.f
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/zbank_hfn_tag.f ;\
	mv zbank_hfn_tag.o $(SCRATCH)/physics_util/a18/zbank_tag366.o ;\
	)
$(d0root)/physics_util/physics_util.a(zbank_ntuple.o):\
  $(d0root)/physics_util/general/zbank_ntuple.f\
  $(d0library)/offline_util/general/value.def
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/zbank_ntuple.f ;\
	mv zbank_ntuple.o $(SCRATCH)/physics_util/a18/zbank_ntuple.o ;\
	)
$(d0root)/physics_util/physics_util.a(zbank_test.o):\
  $(d0root)/physics_util/general/zbank_test.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/zbank_test.f ;\
	mv zbank_test.o $(SCRATCH)/physics_util/a18/zbank_test.o ;\
	)
$(d0root)/physics_util/physics_util.a(zbank_val.o):\
  $(d0root)/physics_util/general/zbank_val.f\
  $(d0library)/inc/zebcom.inc
	(cd $(d0root);\
	$(F77) $(F77FLAGS) $(OPT) physics_util/general/zbank_val.f ;\
	mv zbank_val.o $(SCRATCH)/physics_util/a18/zbank_val.o ;\
	)
pre:\
  $(d0root)/physics_util/general/amoeba.f\
  $(d0root)/physics_util/dbank/auto_fmt.f\
  $(d0root)/physics_util/filters/b1m_munearjet.f\
  $(d0root)/physics_util/filters/b1m_setup_jets.f\
  $(d0root)/physics_util/general/cacl_esum.f\
  $(d0root)/physics_util/general/cal_emcluster.f\
  $(d0root)/physics_util/general/cal_jets.f\
  $(d0root)/physics_util/general/catalogue_jet_corr.f\
  $(d0root)/physics_util/general/cd_vertex_info.f\
  $(d0root)/physics_util/general/clean_cal_junk.f\
  $(d0root)/physics_util/filters/cm3pos_fix.f\
  $(d0root)/physics_util/general/correct_jets.f\
  $(d0root)/physics_util/general/correct_jets_em_remove.f\
  $(d0root)/physics_util/general/correct_jets_noiseu.f\
  $(d0root)/physics_util/general/correct_jets_num_interactions.f\
  $(d0root)/physics_util/general/correct_jets_ooc.f\
  $(d0root)/physics_util/general/correct_jets_reco_version.f\
  $(d0root)/physics_util/general/correct_jets_scale_factors.f\
  $(d0root)/physics_util/general/correct_jets_width.f\
  $(d0root)/physics_util/general/correct_met.f\
  $(d0root)/physics_util/general/correct_met_jet_fixup.f\
  $(d0root)/physics_util/general/correct_old_getlum.f\
  $(d0root)/physics_util/filters/d0dad_list.f\
  $(d0root)/physics_util/d0hplt/d0hcdir.f\
  $(d0root)/physics_util/d0hplt/d0hchd.f\
  $(d0root)/physics_util/d0hplt/d0hcom.f\
  $(d0root)/physics_util/d0hplt/d0hend.f\
  $(d0root)/physics_util/d0hplt/d0hindex.f\
  $(d0root)/physics_util/d0hplt/d0hinx.f\
  $(d0root)/physics_util/d0hplt/d0hlas.f\
  $(d0root)/physics_util/d0hplt/d0hmaklas.f\
  $(d0root)/physics_util/d0hplt/d0hpld.f\
  $(d0root)/physics_util/d0hplt/d0hple.f\
  $(d0root)/physics_util/d0hplt/d0hplt.f\
  $(d0root)/physics_util/d0hplt/d0hplt_dispatch.f\
  $(d0root)/physics_util/d0hplt/d0hplt_event.f\
  $(d0root)/physics_util/d0hplt/d0htyp.f\
  $(d0root)/physics_util/d0hplt/d0huph.f\
  $(d0root)/physics_util/dbank/dbank.f\
  $(d0root)/physics_util/dbank/dcd_chfrm.f\
  $(d0root)/physics_util/general/deta_to_peta.f\
  $(d0root)/physics_util/general/dmpdef.f\
  $(d0root)/physics_util/general/dot.f\
  $(d0root)/physics_util/general/drop_caph_alg.f\
  $(d0root)/physics_util/general/duplicate_event_filter.f\
  $(d0root)/physics_util/dbank/dzfor1.f\
  $(d0root)/physics_util/filters/el2_b.f\
  $(d0root)/physics_util/filters/elf.f\
  $(d0root)/physics_util/filters/elf_med.f\
  $(d0root)/physics_util/filters/elf_w.f\
  $(d0root)/physics_util/filters/elf_z.f\
  $(d0root)/physics_util/filters/em_etzv.f\
  $(d0root)/physics_util/general/em_fits_mask.f\
  $(d0root)/physics_util/general/em_in_jet.f\
  $(d0root)/physics_util/filters/emag_jets.f\
  $(d0root)/physics_util/filters/emfix.f\
  $(d0root)/physics_util/general/emphifix.f\
  $(d0root)/physics_util/udst/esum_compress.f\
  $(d0root)/physics_util/filters/event_list.f\
  $(d0root)/physics_util/general/ez_setup_compack.f\
  $(d0root)/physics_util/general/ezbank.f\
  $(d0root)/physics_util/filters/fake_e_candidate.f\
  $(d0root)/physics_util/udst/fill_cdtk.f\
  $(d0root)/physics_util/udst/fill_nt.f\
  $(d0root)/physics_util/udst/fill_utag_versions.f\
  $(d0root)/physics_util/general/filter_bits.f\
  $(d0root)/physics_util/udst/filtset.f\
  $(d0root)/physics_util/general/find_nearest_b.f\
  $(d0root)/physics_util/general/find_nearest_c.f\
  $(d0root)/physics_util/general/find_nearest_e.f\
  $(d0root)/physics_util/general/find_nearest_mu.f\
  $(d0root)/physics_util/general/find_nearest_object.f\
  $(d0root)/physics_util/general/find_nearest_tau.f\
  $(d0root)/physics_util/general/find_wlnu.f\
  $(d0root)/physics_util/general/find_wlnu2.f\
  $(d0root)/physics_util/general/find_wlnu2d.f\
  $(d0root)/physics_util/general/find_wlnud.f\
  $(d0root)/physics_util/general/fix_jet_et.f\
  $(d0root)/physics_util/general/flusher.f\
  $(d0root)/physics_util/general/frame_menadd.f\
  $(d0root)/physics_util/general/fwmoment.f\
  $(d0root)/physics_util/general/get_caph_alg.f\
  $(d0root)/physics_util/udst/get_e_variance.f\
  $(d0root)/physics_util/general/get_electrons.f\
  $(d0root)/physics_util/general/get_isaj_children.f\
  $(d0root)/physics_util/general/get_isaj_parton.f\
  $(d0root)/physics_util/general/get_isajet_children.f\
  $(d0root)/physics_util/general/get_isajet_parton.f\
  $(d0root)/physics_util/general/get_isal_children.f\
  $(d0root)/physics_util/general/get_isal_parton.f\
  $(d0root)/physics_util/general/get_isaq_children.f\
  $(d0root)/physics_util/general/get_isaq_parton.f\
  $(d0root)/physics_util/general/get_jets.f\
  $(d0root)/physics_util/general/get_l1_l2_info.f\
  $(d0root)/physics_util/general/get_muons.f\
  $(d0root)/physics_util/general/get_next_particle.f\
  $(d0root)/physics_util/general/get_p2neut.f\
  $(d0root)/physics_util/general/get_partons_in_reco.f\
  $(d0root)/physics_util/general/get_photons.f\
  $(d0root)/physics_util/general/get_pjet_vectors.f\
  $(d0root)/physics_util/general/get_quark_content.f\
  $(d0root)/physics_util/dbank/get_store_div.f\
  $(d0root)/physics_util/udst/get_tags.f\
  $(d0root)/physics_util/general/get_trig_filt_run.f\
  $(d0root)/physics_util/general/getlum.f\
  $(d0root)/physics_util/general/getlum_corr.f\
  $(d0root)/physics_util/dbank/getzeb.f\
  $(d0root)/physics_util/general/good_electron.f\
  $(d0root)/physics_util/filters/good_muon_check.f\
  $(d0root)/physics_util/general/good_particle.f\
  $(d0root)/physics_util/general/good_photon.f\
  $(d0root)/physics_util/filters/good_trigger_mu1b.f\
  $(d0root)/physics_util/udst/gtcdtk.f\
  $(d0root)/physics_util/dbank/gtchfr.f\
  $(d0root)/physics_util/dbank/gtzfrm.f\
  $(d0root)/physics_util/udst/handler.f\
  $(d0root)/physics_util/general/hisprt.f\
  $(d0root)/physics_util/general/hot_cell_in_jet.f\
  $(d0root)/physics_util/d0hplt/hplot_dispatch.f\
  $(d0root)/physics_util/d0hplt/hplot_menu.f\
  $(d0root)/physics_util/d0hplt/hplot_proc_menu.f\
  $(d0root)/physics_util/general/invariant_mass.f\
  $(d0root)/physics_util/general/is_datafile.f\
  $(d0root)/physics_util/general/isol_mu_select.f\
  $(d0root)/physics_util/general/jet_cone_corr.f\
  $(d0root)/physics_util/general/jet_corr.f\
  $(d0root)/physics_util/general/jet_emf_scale.f\
  $(d0root)/physics_util/general/jet_et_true.f\
  $(d0root)/physics_util/filters/jet_info.f\
  $(d0root)/physics_util/general/jet_out_of_cone_corr.f\
  $(d0root)/physics_util/general/jet_undzsp_factor.f\
  $(d0root)/physics_util/udst/jutlfl.f\
  $(d0root)/physics_util/general/l15cal_esum_fill.f\
  $(d0root)/physics_util/general/l1esum_evt.f\
  $(d0root)/physics_util/general/l1esum_muon.f\
  $(d0root)/physics_util/general/l1esum_trgr.f\
  $(d0root)/physics_util/general/l1esum_trgr2.f\
  $(d0root)/physics_util/general/lo_calc.f\
  $(d0root)/physics_util/general/lo_dsig_dt.f\
  $(d0root)/physics_util/general/lo_dsig_dt_sel.f\
  $(d0root)/physics_util/general/lo_in_r.f\
  $(d0root)/physics_util/general/lo_main_r.f\
  $(d0root)/physics_util/general/lo_nt.f\
  $(d0root)/physics_util/general/lo_out.f\
  $(d0root)/physics_util/filters/lqnn.f\
  $(d0root)/physics_util/filters/lqnue.f\
  $(d0root)/physics_util/udst/make_nt.f\
  $(d0root)/physics_util/udst/make_udst.f\
  $(d0root)/physics_util/udst/make_uint.f\
  $(d0root)/physics_util/general/matchem_mc_data.f\
  $(d0root)/physics_util/general/mc_et_corr.f\
  $(d0root)/physics_util/d0hplt/menu_d0hplt.f\
  $(d0root)/physics_util/filters/minbias.f\
  $(d0root)/physics_util/general/miss_et_corr.f\
  $(d0root)/physics_util/general/monte_carlo_data.f\
  $(d0root)/physics_util/general/mpf_fill_jets.f\
  $(d0root)/physics_util/general/mpf_jet_response.f\
  $(d0root)/physics_util/general/mpf_jet_response_fin.f\
  $(d0root)/physics_util/general/mpf_jet_response_ini.f\
  $(d0root)/physics_util/general/mpf_jetcorr.f\
  $(d0root)/physics_util/general/mpf_jets.f\
  $(d0root)/physics_util/general/mpf_low_et_bias_corr.f\
  $(d0root)/physics_util/general/mpf_mc_jetcorr.f\
  $(d0root)/physics_util/general/mpf_photons.f\
  $(d0root)/physics_util/general/mpf_response_fit.f\
  $(d0root)/physics_util/general/mpf_sim.f\
  $(d0root)/physics_util/general/mpf_z_boson.f\
  $(d0root)/physics_util/filters/msp_muon_select.f\
  $(d0root)/physics_util/filters/mu1_b.f\
  $(d0root)/physics_util/filters/mu1_filt.f\
  $(d0root)/physics_util/filters/mu1_wzt.f\
  $(d0root)/physics_util/filters/mu2_b.f\
  $(d0root)/physics_util/filters/mu2_wzt.f\
  $(d0root)/physics_util/filters/mu3_b.f\
  $(d0root)/physics_util/filters/mu_b.f\
  $(d0root)/physics_util/general/mu_unbiased.f\
  $(d0root)/physics_util/filters/muj_b.f\
  $(d0root)/physics_util/general/mujets_isolation.f\
  $(d0root)/physics_util/general/mujets_mu_select.f\
  $(d0root)/physics_util/filters/mulq_muon_select.f\
  $(d0root)/physics_util/filters/mulq_muon_select_tight.f\
  $(d0root)/physics_util/filters/multi_filter.f\
  $(d0root)/physics_util/general/multiple_interaction_tool.f\
  $(d0root)/physics_util/general/multiple_interaction_tool_run1.f\
  $(d0root)/physics_util/filters/muon_select_np.f\
  $(d0root)/physics_util/general/muon_vtx_recalc.f\
  $(d0root)/physics_util/filters/new_gam_jet.f\
  $(d0root)/physics_util/filters/np_clean_tau.f\
  $(d0root)/physics_util/filters/np_di_lepton_loose.f\
  $(d0root)/physics_util/filters/np_di_lepton_tight.f\
  $(d0root)/physics_util/filters/np_had.f\
  $(d0root)/physics_util/filters/np_lep.f\
  $(d0root)/physics_util/filters/np_lq_2em.f\
  $(d0root)/physics_util/filters/np_lq_2em_tight.f\
  $(d0root)/physics_util/filters/np_lq_enu.f\
  $(d0root)/physics_util/filters/np_lss_select.f\
  $(d0root)/physics_util/filters/np_lss_tight.f\
  $(d0root)/physics_util/filters/np_lss_trilep_loose.f\
  $(d0root)/physics_util/filters/np_lss_trilep_tight.f\
  $(d0root)/physics_util/filters/np_met.f\
  $(d0root)/physics_util/filters/np_msp.f\
  $(d0root)/physics_util/filters/np_msp_mu_check.f\
  $(d0root)/physics_util/filters/np_msp_mu_select.f\
  $(d0root)/physics_util/filters/np_mulq.f\
  $(d0root)/physics_util/filters/np_mulq_tight.f\
  $(d0root)/physics_util/filters/np_munu.f\
  $(d0root)/physics_util/filters/np_scalar.f\
  $(d0root)/physics_util/filters/np_scalar_tight.f\
  $(d0root)/physics_util/filters/np_sqgl.f\
  $(d0root)/physics_util/filters/np_sqgl_tight.f\
  $(d0root)/physics_util/filters/np_tau.f\
  $(d0root)/physics_util/filters/np_wright.f\
  $(d0root)/physics_util/filters/np_wright_tight.f\
  $(d0root)/physics_util/general/object_muon1.f\
  $(d0root)/physics_util/general/ordx.f\
  $(d0root)/physics_util/general/particle_select.f\
  $(d0root)/physics_util/filters/phi_difference.f\
  $(d0root)/physics_util/general/phys_cor.f\
  $(d0root)/physics_util/general/planarity.f\
  $(d0root)/physics_util/dbank/prnt_bank.f\
  $(d0root)/physics_util/filters/ptfix.f\
  $(d0root)/physics_util/udst/ptopt.f\
  $(d0root)/physics_util/general/qcd_eta_bias.f\
  $(d0root)/physics_util/general/qcd_eta_resol.f\
  $(d0root)/physics_util/filters/qcd_filt_string.f\
  $(d0root)/physics_util/udst/qcd_filter_bit.f\
  $(d0root)/physics_util/udst/qcd_filter_bit_init.f\
  $(d0root)/physics_util/udst/qcd_filter_trigger.f\
  $(d0root)/physics_util/filters/qcd_gamma.f\
  $(d0root)/physics_util/filters/qcd_gap.f\
  $(d0root)/physics_util/udst/qcd_get_mask.f\
  $(d0root)/physics_util/general/qcd_jet_correction.f\
  $(d0root)/physics_util/filters/qcd_jjj.f\
  $(d0root)/physics_util/udst/qcd_make_dst.f\
  $(d0root)/physics_util/filters/qcd_nol.f\
  $(d0root)/physics_util/filters/qcd_ntr.f\
  $(d0root)/physics_util/filters/qcd_qjt.f\
  $(d0root)/physics_util/filters/qcd_sd.f\
  $(d0root)/physics_util/filters/qcd_sta.f\
  $(d0root)/physics_util/filters/qcdjets.f\
  $(d0root)/physics_util/general/recalc_muon.f\
  $(d0root)/physics_util/general/reco_esum.f\
  $(d0root)/physics_util/general/reco_hsum_fill.f\
  $(d0root)/physics_util/general/reco_version_select.f\
  $(d0root)/physics_util/general/run_select.f\
  $(d0root)/physics_util/general/sel_elec_jets.f\
  $(d0root)/physics_util/general/set_caph_alg.f\
  $(d0root)/physics_util/general/set_jet.f\
  $(d0root)/physics_util/general/set_traceback_evtnum.f\
  $(d0root)/physics_util/general/sm_jet_undzsp_factor.f\
  $(d0root)/physics_util/filters/sort_descend.f\
  $(d0root)/physics_util/general/sort_vect.f\
  $(d0root)/physics_util/filters/sortbanks.f\
  $(d0root)/physics_util/general/sphericity.f\
  $(d0root)/physics_util/general/sphericity0.f\
  $(d0root)/physics_util/udst/split_bitmask.f\
  $(d0root)/physics_util/filters/ss_mu_check.f\
  $(d0root)/physics_util/filters/ss_mu_chk.f\
  $(d0root)/physics_util/filters/ss_mu_sel.f\
  $(d0root)/physics_util/filters/ss_mu_select.f\
  $(d0root)/physics_util/filters/stream_ejets.f\
  $(d0root)/physics_util/general/tag_mu_select.f\
  $(d0root)/physics_util/filters/tau_filter.f\
  $(d0root)/physics_util/filters/threejet_filter.f\
  $(d0root)/physics_util/filters/top_bkg.f\
  $(d0root)/physics_util/filters/top_diem.f\
  $(d0root)/physics_util/filters/top_ee.f\
  $(d0root)/physics_util/filters/top_ejbkg.f\
  $(d0root)/physics_util/filters/top_ejet.f\
  $(d0root)/physics_util/filters/top_ejsig.f\
  $(d0root)/physics_util/filters/top_emu.f\
  $(d0root)/physics_util/general/top_event_cleanup.f\
  $(d0root)/physics_util/filters/top_jets.f\
  $(d0root)/physics_util/filters/top_muj.f\
  $(d0root)/physics_util/filters/top_mujet.f\
  $(d0root)/physics_util/filters/top_mumu.f\
  $(d0root)/physics_util/filters/top_sgl.f\
  $(d0root)/physics_util/filters/top_top_bbbar.f\
  $(d0root)/physics_util/filters/top_top_ee.f\
  $(d0root)/physics_util/filters/top_top_ejet.f\
  $(d0root)/physics_util/filters/top_top_emu.f\
  $(d0root)/physics_util/filters/top_top_jets.f\
  $(d0root)/physics_util/filters/top_top_mujet.f\
  $(d0root)/physics_util/filters/top_top_mumu.f\
  $(d0root)/physics_util/filters/top_top_sgl.f\
  $(d0root)/physics_util/general/topup.f\
  $(d0root)/physics_util/general/topup_bkg_err.f\
  $(d0root)/physics_util/general/transverse_mass.f\
  $(d0root)/physics_util/general/trigger_bit_check.f\
  $(d0root)/physics_util/general/tsum_fill.f\
  $(d0root)/physics_util/general/tsum_names.f\
  $(d0root)/physics_util/filters/twojet_filter.f\
  $(d0root)/physics_util/udst/ucshfl.f\
  $(d0root)/physics_util/udst/ucwxfl.f\
  $(d0root)/physics_util/udst/udst.f\
  $(d0root)/physics_util/udst/udst_book_group.f\
  $(d0root)/physics_util/udst/udst_cdtk_errors_pack.f\
  $(d0root)/physics_util/udst/udst_cdtk_errors_unpack.f\
  $(d0root)/physics_util/udst/udst_fin.f\
  $(d0root)/physics_util/udst/udst_fix_bank.f\
  $(d0root)/physics_util/udst/udst_fix_jets.f\
  $(d0root)/physics_util/udst/udst_fix_pelc.f\
  $(d0root)/physics_util/udst/udst_fix_pmuo.f\
  $(d0root)/physics_util/udst/udst_fix_pnut.f\
  $(d0root)/physics_util/udst/udst_fix_ptau.f\
  $(d0root)/physics_util/udst/udst_fix_vtxt.f\
  $(d0root)/physics_util/udst/udst_get_cash.f\
  $(d0root)/physics_util/udst/udst_get_cawx.f\
  $(d0root)/physics_util/udst/udst_get_emclus.f\
  $(d0root)/physics_util/udst/udst_get_glob.f\
  $(d0root)/physics_util/udst/udst_get_head.f\
  $(d0root)/physics_util/udst/udst_get_jets.f\
  $(d0root)/physics_util/udst/udst_get_jnep.f\
  $(d0root)/physics_util/udst/udst_get_l12_met.f\
  $(d0root)/physics_util/udst/udst_get_magpol.f\
  $(d0root)/physics_util/udst/udst_get_muon.f\
  $(d0root)/physics_util/udst/udst_get_pelc_link.f\
  $(d0root)/physics_util/udst/udst_get_pnu1.f\
  $(d0root)/physics_util/udst/udst_get_pnut.f\
  $(d0root)/physics_util/udst/udst_get_ptau.f\
  $(d0root)/physics_util/udst/udst_get_sahh.f\
  $(d0root)/physics_util/udst/udst_get_track_link.f\
  $(d0root)/physics_util/udst/udst_get_trak.f\
  $(d0root)/physics_util/udst/udst_get_version.f\
  $(d0root)/physics_util/udst/udst_get_vert.f\
  $(d0root)/physics_util/udst/udst_get_vert_link.f\
  $(d0root)/physics_util/udst/udst_get_vfit.f\
  $(d0root)/physics_util/udst/udst_get_vtxt.f\
  $(d0root)/physics_util/udst/udst_get_vtxt_link.f\
  $(d0root)/physics_util/udst/udst_ini.f\
  $(d0root)/physics_util/udst/udst_out.f\
  $(d0root)/physics_util/udst/udst_tag.f\
  $(d0root)/physics_util/udst/udst_to_dst.f\
  $(d0root)/physics_util/udst/udst_to_dst_fill.f\
  $(d0root)/physics_util/udst/udst_to_dst_fill_bank.f\
  $(d0root)/physics_util/udst/udst_to_dst_init.f\
  $(d0root)/physics_util/udst/udst_unpack_version.f\
  $(d0root)/physics_util/udst/udstfl.f\
  $(d0root)/physics_util/udst/uint_to_dst.f\
  $(d0root)/physics_util/general/uncorrect_jets_bank.f\
  $(d0root)/physics_util/filters/uni_filter.f\
  $(d0root)/physics_util/general/unix_util_install.f\
  $(d0root)/physics_util/udst/utag_from_udst.f\
  $(d0root)/physics_util/udst/utagfl.f\
  $(d0root)/physics_util/dbank/vbrowse.f\
  $(d0root)/physics_util/general/verify_zebra.f\
  $(d0root)/physics_util/filters/vertex_filter.f\
  $(d0root)/physics_util/dbank/zb_smg_pn1.f\
  $(d0root)/physics_util/dbank/zb_smg_pn2.f\
  $(d0root)/physics_util/general/zbank_edit.f\
  $(d0root)/physics_util/general/zbank_hfn_tag.f\
  $(d0root)/physics_util/general/zbank_ntuple.f\
  $(d0root)/physics_util/general/zbank_test.f\
  $(d0root)/physics_util/general/zbank_val.f
$(d0root)/physics_util/general/amoeba.f:\
  $(d0root)/physics_util/general/amoeba.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/amoeba.for | vmstounix > $(d0root)/physics_util/general/amoeba.f
$(d0root)/physics_util/dbank/auto_fmt.f:\
  $(d0root)/physics_util/dbank/auto_fmt.for\
  $(d0library)/inc/autof.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebq.inc\
  $(d0library)/inc/mzca.inc\
  $(d0library)/inc/mzcb.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/dbank/auto_fmt.for | vmstounix > $(d0root)/physics_util/dbank/auto_fmt.f
$(d0root)/physics_util/filters/b1m_munearjet.f:\
  $(d0root)/physics_util/filters/b1m_munearjet.for\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/b1m_munearjet.for | vmstounix > $(d0root)/physics_util/filters/b1m_munearjet.f
$(d0root)/physics_util/filters/b1m_setup_jets.f:\
  $(d0root)/physics_util/filters/b1m_setup_jets.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izisae.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/b1m_setup_jets.for | vmstounix > $(d0root)/physics_util/filters/b1m_setup_jets.f
$(d0root)/physics_util/general/cacl_esum.f:\
  $(d0root)/physics_util/general/cacl_esum.for\
  $(d0library)/params/esum.params\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izvert.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/cacl_esum.for | vmstounix > $(d0root)/physics_util/general/cacl_esum.f
$(d0root)/physics_util/general/cal_emcluster.f:\
  $(d0root)/physics_util/general/cal_emcluster.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/cal_emcluster.for | vmstounix > $(d0root)/physics_util/general/cal_emcluster.f
$(d0root)/physics_util/general/cal_jets.f:\
  $(d0root)/physics_util/general/cal_jets.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izjets.link\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/cal_jets.for | vmstounix > $(d0root)/physics_util/general/cal_jets.f
$(d0root)/physics_util/general/catalogue_jet_corr.f:\
  $(d0root)/physics_util/general/catalogue_jet_corr.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/catalogue_jet_corr.for | vmstounix > $(d0root)/physics_util/general/catalogue_jet_corr.f
$(d0root)/physics_util/general/cd_vertex_info.f:\
  $(d0root)/physics_util/general/cd_vertex_info.for\
  $(d0library)/params/vertex_multi.params
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/cd_vertex_info.for | vmstounix > $(d0root)/physics_util/general/cd_vertex_info.f
$(d0root)/physics_util/general/clean_cal_junk.f:\
  $(d0root)/physics_util/general/clean_cal_junk.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/clean_cal_junk.for | vmstounix > $(d0root)/physics_util/general/clean_cal_junk.f
$(d0root)/physics_util/filters/cm3pos_fix.f:\
  $(d0root)/physics_util/filters/cm3pos_fix.for\
  $(d0library)/inc/cemprf.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/cal_offline.params
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/cm3pos_fix.for | vmstounix > $(d0root)/physics_util/filters/cm3pos_fix.f
$(d0root)/physics_util/general/correct_jets.f:\
  $(d0root)/physics_util/general/correct_jets.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcaph.link\
  $(d0library)/links/izjets.link\
  $(d0library)/params/caph.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/correct_jets.for | vmstounix > $(d0root)/physics_util/general/correct_jets.f
$(d0root)/physics_util/general/correct_jets_em_remove.f:\
  $(d0root)/physics_util/general/correct_jets_em_remove.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/correct_jets_em_remove.for | vmstounix > $(d0root)/physics_util/general/correct_jets_em_remove.f
$(d0root)/physics_util/general/correct_jets_noiseu.f:\
  $(d0root)/physics_util/general/correct_jets_noiseu.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/correct_jets_noiseu.for | vmstounix > $(d0root)/physics_util/general/correct_jets_noiseu.f
$(d0root)/physics_util/general/correct_jets_num_interactions.f:\
  $(d0root)/physics_util/general/correct_jets_num_interactions.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/correct_jets_num_interactions.for | vmstounix > $(d0root)/physics_util/general/correct_jets_num_interactions.f
$(d0root)/physics_util/general/correct_jets_ooc.f:\
  $(d0root)/physics_util/general/correct_jets_ooc.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/correct_jets_ooc.for | vmstounix > $(d0root)/physics_util/general/correct_jets_ooc.f
$(d0root)/physics_util/general/correct_jets_reco_version.f:\
  $(d0root)/physics_util/general/correct_jets_reco_version.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/correct_jets_reco_version.for | vmstounix > $(d0root)/physics_util/general/correct_jets_reco_version.f
$(d0root)/physics_util/general/correct_jets_scale_factors.f:\
  $(d0root)/physics_util/general/correct_jets_scale_factors.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/correct_jets_scale_factors.for | vmstounix > $(d0root)/physics_util/general/correct_jets_scale_factors.f
$(d0root)/physics_util/general/correct_jets_width.f:\
  $(d0root)/physics_util/general/correct_jets_width.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/correct_jets_width.for | vmstounix > $(d0root)/physics_util/general/correct_jets_width.f
$(d0root)/physics_util/general/correct_met.f:\
  $(d0root)/physics_util/general/correct_met.for\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izreco.link\
  $(d0library)/links/izjnep.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/correct_met.for | vmstounix > $(d0root)/physics_util/general/correct_met.f
$(d0root)/physics_util/general/correct_met_jet_fixup.f:\
  $(d0root)/physics_util/general/correct_met_jet_fixup.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/correct_met_jet_fixup.for | vmstounix > $(d0root)/physics_util/general/correct_met_jet_fixup.f
$(d0root)/physics_util/general/correct_old_getlum.f:\
  $(d0root)/physics_util/general/correct_old_getlum.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/correct_old_getlum.for | vmstounix > $(d0root)/physics_util/general/correct_old_getlum.f
$(d0root)/physics_util/filters/d0dad_list.f:\
  $(d0root)/physics_util/filters/d0dad_list.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/d0dad_list.for | vmstounix > $(d0root)/physics_util/filters/d0dad_list.f
$(d0root)/physics_util/d0hplt/d0hcdir.f:\
  $(d0root)/physics_util/d0hplt/d0hcdir.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/d0hcdir.for | vmstounix > $(d0root)/physics_util/d0hplt/d0hcdir.f
$(d0root)/physics_util/d0hplt/d0hchd.f:\
  $(d0root)/physics_util/d0hplt/d0hchd.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/d0hchd.for | vmstounix > $(d0root)/physics_util/d0hplt/d0hchd.f
$(d0root)/physics_util/d0hplt/d0hcom.f:\
  $(d0root)/physics_util/d0hplt/d0hcom.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/d0hcom.for | vmstounix > $(d0root)/physics_util/d0hplt/d0hcom.f
$(d0root)/physics_util/d0hplt/d0hend.f:\
  $(d0root)/physics_util/d0hplt/d0hend.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/d0hend.for | vmstounix > $(d0root)/physics_util/d0hplt/d0hend.f
$(d0root)/physics_util/d0hplt/d0hindex.f:\
  $(d0root)/physics_util/d0hplt/d0hindex.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/d0hindex.for | vmstounix > $(d0root)/physics_util/d0hplt/d0hindex.f
$(d0root)/physics_util/d0hplt/d0hinx.f:\
  $(d0root)/physics_util/d0hplt/d0hinx.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/d0hinx.for | vmstounix > $(d0root)/physics_util/d0hplt/d0hinx.f
$(d0root)/physics_util/d0hplt/d0hlas.f:\
  $(d0root)/physics_util/d0hplt/d0hlas.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/d0hlas.for | vmstounix > $(d0root)/physics_util/d0hplt/d0hlas.f
$(d0root)/physics_util/d0hplt/d0hmaklas.f:\
  $(d0root)/physics_util/d0hplt/d0hmaklas.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/d0hmaklas.for | vmstounix > $(d0root)/physics_util/d0hplt/d0hmaklas.f
$(d0root)/physics_util/d0hplt/d0hpld.f:\
  $(d0root)/physics_util/d0hplt/d0hpld.for\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/d0hpld.for | vmstounix > $(d0root)/physics_util/d0hplt/d0hpld.f
$(d0root)/physics_util/d0hplt/d0hple.f:\
  $(d0root)/physics_util/d0hplt/d0hple.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/d0hple.for | vmstounix > $(d0root)/physics_util/d0hplt/d0hple.f
$(d0root)/physics_util/d0hplt/d0hplt.f:\
  $(d0root)/physics_util/d0hplt/d0hplt.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/d0hplt.for | vmstounix > $(d0root)/physics_util/d0hplt/d0hplt.f
$(d0root)/physics_util/d0hplt/d0hplt_dispatch.f:\
  $(d0root)/physics_util/d0hplt/d0hplt_dispatch.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/d0hplt_dispatch.for | vmstounix > $(d0root)/physics_util/d0hplt/d0hplt_dispatch.f
$(d0root)/physics_util/d0hplt/d0hplt_event.f:\
  $(d0root)/physics_util/d0hplt/d0hplt_event.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/d0hplt_event.for | vmstounix > $(d0root)/physics_util/d0hplt/d0hplt_event.f
$(d0root)/physics_util/d0hplt/d0htyp.f:\
  $(d0root)/physics_util/d0hplt/d0htyp.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/d0htyp.for | vmstounix > $(d0root)/physics_util/d0hplt/d0htyp.f
$(d0root)/physics_util/d0hplt/d0huph.f:\
  $(d0root)/physics_util/d0hplt/d0huph.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/d0huph.for | vmstounix > $(d0root)/physics_util/d0hplt/d0huph.f
$(d0root)/physics_util/dbank/dbank.f:\
  $(d0root)/physics_util/dbank/dbank.for\
  $(d0library)/inc/ssdef.def\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebq.inc\
  $(d0library)/inc/mzca.inc\
  $(d0library)/inc/mzcb.inc\
  $(d0library)/inc/autof.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/dbank/dbank.for | vmstounix > $(d0root)/physics_util/dbank/dbank.f
$(d0root)/physics_util/dbank/dcd_chfrm.f:\
  $(d0root)/physics_util/dbank/dcd_chfrm.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/dbank/dcd_chfrm.for | vmstounix > $(d0root)/physics_util/dbank/dcd_chfrm.f
$(d0root)/physics_util/general/deta_to_peta.f:\
  $(d0root)/physics_util/general/deta_to_peta.for\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/deta_to_peta.for | vmstounix > $(d0root)/physics_util/general/deta_to_peta.f
$(d0root)/physics_util/general/dmpdef.f:\
  $(d0root)/physics_util/general/dmpdef.for\
  $(d0library)/inc/dump.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/dmpdef.for | vmstounix > $(d0root)/physics_util/general/dmpdef.f
$(d0root)/physics_util/general/dot.f:\
  $(d0root)/physics_util/general/dot.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/dot.for | vmstounix > $(d0root)/physics_util/general/dot.f
$(d0root)/physics_util/general/drop_caph_alg.f:\
  $(d0root)/physics_util/general/drop_caph_alg.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/caph.def\
  $(d0library)/links/izcaph.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/drop_caph_alg.for | vmstounix > $(d0root)/physics_util/general/drop_caph_alg.f
$(d0root)/physics_util/general/duplicate_event_filter.f:\
  $(d0root)/physics_util/general/duplicate_event_filter.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/duplicate_event_filter.for | vmstounix > $(d0root)/physics_util/general/duplicate_event_filter.f
$(d0root)/physics_util/dbank/dzfor1.f:\
  $(d0root)/physics_util/dbank/dzfor1.for\
  $(d0library)/inc/mzca.inc\
  $(d0library)/inc/mzcb.inc\
  $(d0library)/inc/mzcc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/dbank/dzfor1.for | vmstounix > $(d0root)/physics_util/dbank/dzfor1.f
$(d0root)/physics_util/filters/el2_b.f:\
  $(d0root)/physics_util/filters/el2_b.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpelc.link\
  $(d0library)/links/izppho.link\
  $(d0library)/links/izpmuo.link\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/el2_b.for | vmstounix > $(d0root)/physics_util/filters/el2_b.f
$(d0root)/physics_util/filters/elf.f:\
  $(d0root)/physics_util/filters/elf.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/elf.for | vmstounix > $(d0root)/physics_util/filters/elf.f
$(d0root)/physics_util/filters/elf_med.f:\
  $(d0root)/physics_util/filters/elf_med.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/elf_med.for | vmstounix > $(d0root)/physics_util/filters/elf_med.f
$(d0root)/physics_util/filters/elf_w.f:\
  $(d0root)/physics_util/filters/elf_w.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/elf_w.for | vmstounix > $(d0root)/physics_util/filters/elf_w.f
$(d0root)/physics_util/filters/elf_z.f:\
  $(d0root)/physics_util/filters/elf_z.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/elf_z.for | vmstounix > $(d0root)/physics_util/filters/elf_z.f
$(d0root)/physics_util/filters/em_etzv.f:\
  $(d0root)/physics_util/filters/em_etzv.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izzfit.link\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/em_etzv.for | vmstounix > $(d0root)/physics_util/filters/em_etzv.f
$(d0root)/physics_util/general/em_fits_mask.f:\
  $(d0root)/physics_util/general/em_fits_mask.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/em_fits_mask.for | vmstounix > $(d0root)/physics_util/general/em_fits_mask.f
$(d0root)/physics_util/general/em_in_jet.f:\
  $(d0root)/physics_util/general/em_in_jet.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/em_in_jet.for | vmstounix > $(d0root)/physics_util/general/em_in_jet.f
$(d0root)/physics_util/filters/emag_jets.f:\
  $(d0root)/physics_util/filters/emag_jets.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/emag_jets.for | vmstounix > $(d0root)/physics_util/filters/emag_jets.f
$(d0root)/physics_util/filters/emfix.f:\
  $(d0root)/physics_util/filters/emfix.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/inc/zhmatrix.inc\
  $(d0library)/inc/cemprf.inc\
  $(d0library)/inc/cimpact.inc\
  $(d0library)/inc/ctrak.inc\
  $(d0library)/inc/chmatr_new.inc\
  $(d0library)/inc/more_ntuple.inc\
  $(d0library)/links/izzfit.link\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/emfix.for | vmstounix > $(d0root)/physics_util/filters/emfix.f
$(d0root)/physics_util/general/emphifix.f:\
  $(d0root)/physics_util/general/emphifix.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/emphifix.for | vmstounix > $(d0root)/physics_util/general/emphifix.f
$(d0root)/physics_util/udst/esum_compress.f:\
  $(d0root)/physics_util/udst/esum_compress.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/esum_compress.for | vmstounix > $(d0root)/physics_util/udst/esum_compress.f
$(d0root)/physics_util/filters/event_list.f:\
  $(d0root)/physics_util/filters/event_list.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/event_list.for | vmstounix > $(d0root)/physics_util/filters/event_list.f
$(d0root)/physics_util/general/ez_setup_compack.f:\
  $(d0root)/physics_util/general/ez_setup_compack.for\
  $(d0library)/offline_util/general/value.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/ez_setup_compack.for | vmstounix > $(d0root)/physics_util/general/ez_setup_compack.f
$(d0root)/physics_util/general/ezbank.f:\
  $(d0root)/physics_util/general/ezbank.for\
  $(d0library)/params/srcp.def\
  $(d0library)/params/maxlev.def\
  $(d0library)/inc/comnum.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/ezbank.for | vmstounix > $(d0root)/physics_util/general/ezbank.f
$(d0root)/physics_util/filters/fake_e_candidate.f:\
  $(d0root)/physics_util/filters/fake_e_candidate.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/links/izvert.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/fake_e_candidate.for | vmstounix > $(d0root)/physics_util/filters/fake_e_candidate.f
$(d0root)/physics_util/udst/fill_cdtk.f:\
  $(d0root)/physics_util/udst/fill_cdtk.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izdtrk.link\
  $(d0library)/inc/cdtk_packing.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/fill_cdtk.for | vmstounix > $(d0root)/physics_util/udst/fill_cdtk.f
$(d0root)/physics_util/udst/fill_nt.f:\
  $(d0root)/physics_util/udst/fill_nt.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/fill_nt.for | vmstounix > $(d0root)/physics_util/udst/fill_nt.f
$(d0root)/physics_util/udst/fill_utag_versions.f:\
  $(d0root)/physics_util/udst/fill_utag_versions.for\
  $(d0library)/inc/utag_data.inc\
  $(d0library)/inc/utag_versions.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/fill_utag_versions.for | vmstounix > $(d0root)/physics_util/udst/fill_utag_versions.f
$(d0root)/physics_util/general/filter_bits.f:\
  $(d0root)/physics_util/general/filter_bits.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/filter_bits.for | vmstounix > $(d0root)/physics_util/general/filter_bits.f
$(d0root)/physics_util/udst/filtset.f:\
  $(d0root)/physics_util/udst/filtset.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/filtset.for | vmstounix > $(d0root)/physics_util/udst/filtset.f
$(d0root)/physics_util/general/find_nearest_b.f:\
  $(d0root)/physics_util/general/find_nearest_b.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/find_nearest_b.for | vmstounix > $(d0root)/physics_util/general/find_nearest_b.f
$(d0root)/physics_util/general/find_nearest_c.f:\
  $(d0root)/physics_util/general/find_nearest_c.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/find_nearest_c.for | vmstounix > $(d0root)/physics_util/general/find_nearest_c.f
$(d0root)/physics_util/general/find_nearest_e.f:\
  $(d0root)/physics_util/general/find_nearest_e.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/find_nearest_e.for | vmstounix > $(d0root)/physics_util/general/find_nearest_e.f
$(d0root)/physics_util/general/find_nearest_mu.f:\
  $(d0root)/physics_util/general/find_nearest_mu.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/find_nearest_mu.for | vmstounix > $(d0root)/physics_util/general/find_nearest_mu.f
$(d0root)/physics_util/general/find_nearest_object.f:\
  $(d0root)/physics_util/general/find_nearest_object.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/find_nearest_object.for | vmstounix > $(d0root)/physics_util/general/find_nearest_object.f
$(d0root)/physics_util/general/find_nearest_tau.f:\
  $(d0root)/physics_util/general/find_nearest_tau.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/find_nearest_tau.for | vmstounix > $(d0root)/physics_util/general/find_nearest_tau.f
$(d0root)/physics_util/general/find_wlnu.f:\
  $(d0root)/physics_util/general/find_wlnu.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/find_wlnu.for | vmstounix > $(d0root)/physics_util/general/find_wlnu.f
$(d0root)/physics_util/general/find_wlnu2.f:\
  $(d0root)/physics_util/general/find_wlnu2.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/find_wlnu2.for | vmstounix > $(d0root)/physics_util/general/find_wlnu2.f
$(d0root)/physics_util/general/find_wlnu2d.f:\
  $(d0root)/physics_util/general/find_wlnu2d.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/find_wlnu2d.for | vmstounix > $(d0root)/physics_util/general/find_wlnu2d.f
$(d0root)/physics_util/general/find_wlnud.f:\
  $(d0root)/physics_util/general/find_wlnud.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/find_wlnud.for | vmstounix > $(d0root)/physics_util/general/find_wlnud.f
$(d0root)/physics_util/general/fix_jet_et.f:\
  $(d0root)/physics_util/general/fix_jet_et.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcaph.link\
  $(d0library)/links/izjets.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/fix_jet_et.for | vmstounix > $(d0root)/physics_util/general/fix_jet_et.f
$(d0root)/physics_util/general/flusher.f:\
  $(d0root)/physics_util/general/flusher.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/flusher.for | vmstounix > $(d0root)/physics_util/general/flusher.f
$(d0root)/physics_util/general/frame_menadd.f:\
  $(d0root)/physics_util/general/frame_menadd.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/frame_menadd.for | vmstounix > $(d0root)/physics_util/general/frame_menadd.f
$(d0root)/physics_util/general/fwmoment.f:\
  $(d0root)/physics_util/general/fwmoment.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/fwmoment.for | vmstounix > $(d0root)/physics_util/general/fwmoment.f
$(d0root)/physics_util/general/get_caph_alg.f:\
  $(d0root)/physics_util/general/get_caph_alg.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/caph.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_caph_alg.for | vmstounix > $(d0root)/physics_util/general/get_caph_alg.f
$(d0root)/physics_util/udst/get_e_variance.f:\
  $(d0root)/physics_util/udst/get_e_variance.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/get_e_variance.for | vmstounix > $(d0root)/physics_util/udst/get_e_variance.f
$(d0root)/physics_util/general/get_electrons.f:\
  $(d0root)/physics_util/general/get_electrons.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/links/izhmte.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_electrons.for | vmstounix > $(d0root)/physics_util/general/get_electrons.f
$(d0root)/physics_util/general/get_isaj_children.f:\
  $(d0root)/physics_util/general/get_isaj_children.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_isaj_children.for | vmstounix > $(d0root)/physics_util/general/get_isaj_children.f
$(d0root)/physics_util/general/get_isaj_parton.f:\
  $(d0root)/physics_util/general/get_isaj_parton.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_isaj_parton.for | vmstounix > $(d0root)/physics_util/general/get_isaj_parton.f
$(d0root)/physics_util/general/get_isajet_children.f:\
  $(d0root)/physics_util/general/get_isajet_children.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_isajet_children.for | vmstounix > $(d0root)/physics_util/general/get_isajet_children.f
$(d0root)/physics_util/general/get_isajet_parton.f:\
  $(d0root)/physics_util/general/get_isajet_parton.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_isajet_parton.for | vmstounix > $(d0root)/physics_util/general/get_isajet_parton.f
$(d0root)/physics_util/general/get_isal_children.f:\
  $(d0root)/physics_util/general/get_isal_children.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_isal_children.for | vmstounix > $(d0root)/physics_util/general/get_isal_children.f
$(d0root)/physics_util/general/get_isal_parton.f:\
  $(d0root)/physics_util/general/get_isal_parton.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_isal_parton.for | vmstounix > $(d0root)/physics_util/general/get_isal_parton.f
$(d0root)/physics_util/general/get_isaq_children.f:\
  $(d0root)/physics_util/general/get_isaq_children.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_isaq_children.for | vmstounix > $(d0root)/physics_util/general/get_isaq_children.f
$(d0root)/physics_util/general/get_isaq_parton.f:\
  $(d0root)/physics_util/general/get_isaq_parton.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_isaq_parton.for | vmstounix > $(d0root)/physics_util/general/get_isaq_parton.f
$(d0root)/physics_util/general/get_jets.f:\
  $(d0root)/physics_util/general/get_jets.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/params/caph.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_jets.for | vmstounix > $(d0root)/physics_util/general/get_jets.f
$(d0root)/physics_util/general/get_l1_l2_info.f:\
  $(d0root)/physics_util/general/get_l1_l2_info.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_l1_l2_info.for | vmstounix > $(d0root)/physics_util/general/get_l1_l2_info.f
$(d0root)/physics_util/general/get_muons.f:\
  $(d0root)/physics_util/general/get_muons.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_muons.for | vmstounix > $(d0root)/physics_util/general/get_muons.f
$(d0root)/physics_util/general/get_next_particle.f:\
  $(d0root)/physics_util/general/get_next_particle.for\
  $(d0library)/links/izisae.link\
  $(d0library)/links/izisp1.link\
  $(d0library)/links/izisv1.link\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_next_particle.for | vmstounix > $(d0root)/physics_util/general/get_next_particle.f
$(d0root)/physics_util/general/get_p2neut.f:\
  $(d0root)/physics_util/general/get_p2neut.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_p2neut.for | vmstounix > $(d0root)/physics_util/general/get_p2neut.f
$(d0root)/physics_util/general/get_partons_in_reco.f:\
  $(d0root)/physics_util/general/get_partons_in_reco.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_partons_in_reco.for | vmstounix > $(d0root)/physics_util/general/get_partons_in_reco.f
$(d0root)/physics_util/general/get_photons.f:\
  $(d0root)/physics_util/general/get_photons.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_photons.for | vmstounix > $(d0root)/physics_util/general/get_photons.f
$(d0root)/physics_util/general/get_pjet_vectors.f:\
  $(d0root)/physics_util/general/get_pjet_vectors.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpjet.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_pjet_vectors.for | vmstounix > $(d0root)/physics_util/general/get_pjet_vectors.f
$(d0root)/physics_util/general/get_quark_content.f:\
  $(d0root)/physics_util/general/get_quark_content.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_quark_content.for | vmstounix > $(d0root)/physics_util/general/get_quark_content.f
$(d0root)/physics_util/dbank/get_store_div.f:\
  $(d0root)/physics_util/dbank/get_store_div.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebstp.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/dbank/get_store_div.for | vmstounix > $(d0root)/physics_util/dbank/get_store_div.f
$(d0root)/physics_util/udst/get_tags.f:\
  $(d0root)/physics_util/udst/get_tags.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/udst_dimensions.params
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/get_tags.for | vmstounix > $(d0root)/physics_util/udst/get_tags.f
$(d0root)/physics_util/general/get_trig_filt_run.f:\
  $(d0root)/physics_util/general/get_trig_filt_run.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/get_trig_filt_run.for | vmstounix > $(d0root)/physics_util/general/get_trig_filt_run.f
$(d0root)/physics_util/general/getlum.f:\
  $(d0root)/physics_util/general/getlum.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/getlum.for | vmstounix > $(d0root)/physics_util/general/getlum.f
$(d0root)/physics_util/general/getlum_corr.f:\
  $(d0root)/physics_util/general/getlum_corr.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/getlum_corr.for | vmstounix > $(d0root)/physics_util/general/getlum_corr.f
$(d0root)/physics_util/dbank/getzeb.f:\
  $(d0root)/physics_util/dbank/getzeb.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/dbank/getzeb.for | vmstounix > $(d0root)/physics_util/dbank/getzeb.f
$(d0root)/physics_util/general/good_electron.f:\
  $(d0root)/physics_util/general/good_electron.for\
  $(d0library)/params/good_electron_photon.params\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/good_electron.for | vmstounix > $(d0root)/physics_util/general/good_electron.f
$(d0root)/physics_util/filters/good_muon_check.f:\
  $(d0root)/physics_util/filters/good_muon_check.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/good_muon_check.for | vmstounix > $(d0root)/physics_util/filters/good_muon_check.f
$(d0root)/physics_util/general/good_particle.f:\
  $(d0root)/physics_util/general/good_particle.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/good_particle.for | vmstounix > $(d0root)/physics_util/general/good_particle.f
$(d0root)/physics_util/general/good_photon.f:\
  $(d0root)/physics_util/general/good_photon.for\
  $(d0library)/params/good_electron_photon.params\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/good_photon.for | vmstounix > $(d0root)/physics_util/general/good_photon.f
$(d0root)/physics_util/filters/good_trigger_mu1b.f:\
  $(d0root)/physics_util/filters/good_trigger_mu1b.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/good_trigger_mu1b.for | vmstounix > $(d0root)/physics_util/filters/good_trigger_mu1b.f
$(d0root)/physics_util/udst/gtcdtk.f:\
  $(d0root)/physics_util/udst/gtcdtk.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcdtk.link\
  $(d0library)/inc/cdtk_packing.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/gtcdtk.for | vmstounix > $(d0root)/physics_util/udst/gtcdtk.f
$(d0root)/physics_util/dbank/gtchfr.f:\
  $(d0root)/physics_util/dbank/gtchfr.for\
  $(d0library)/inc/autof.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zebq.inc\
  $(d0library)/inc/mzca.inc\
  $(d0library)/inc/mzcb.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/dbank/gtchfr.for | vmstounix > $(d0root)/physics_util/dbank/gtchfr.f
$(d0root)/physics_util/dbank/gtzfrm.f:\
  $(d0root)/physics_util/dbank/gtzfrm.for\
  $(d0library)/inc/mzca.inc\
  $(d0library)/inc/mzcb.inc\
  $(d0library)/inc/mzcc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/dbank/gtzfrm.for | vmstounix > $(d0root)/physics_util/dbank/gtzfrm.f
$(d0root)/physics_util/udst/handler.f:\
  $(d0root)/physics_util/udst/handler.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/handler.for | vmstounix > $(d0root)/physics_util/udst/handler.f
$(d0root)/physics_util/general/hisprt.f:\
  $(d0root)/physics_util/general/hisprt.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/hisprt.for | vmstounix > $(d0root)/physics_util/general/hisprt.f
$(d0root)/physics_util/general/hot_cell_in_jet.f:\
  $(d0root)/physics_util/general/hot_cell_in_jet.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/byte_order.params\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/hot_cell_in_jet.for | vmstounix > $(d0root)/physics_util/general/hot_cell_in_jet.f
$(d0root)/physics_util/d0hplt/hplot_dispatch.f:\
  $(d0root)/physics_util/d0hplt/hplot_dispatch.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/hplot_dispatch.for | vmstounix > $(d0root)/physics_util/d0hplt/hplot_dispatch.f
$(d0root)/physics_util/d0hplt/hplot_menu.f:\
  $(d0root)/physics_util/d0hplt/hplot_menu.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/hplot_menu.for | vmstounix > $(d0root)/physics_util/d0hplt/hplot_menu.f
$(d0root)/physics_util/d0hplt/hplot_proc_menu.f:\
  $(d0root)/physics_util/d0hplt/hplot_proc_menu.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/hplot_proc_menu.for | vmstounix > $(d0root)/physics_util/d0hplt/hplot_proc_menu.f
$(d0root)/physics_util/general/invariant_mass.f:\
  $(d0root)/physics_util/general/invariant_mass.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/invariant_mass.for | vmstounix > $(d0root)/physics_util/general/invariant_mass.f
$(d0root)/physics_util/general/is_datafile.f:\
  $(d0root)/physics_util/general/is_datafile.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/is_datafile.for | vmstounix > $(d0root)/physics_util/general/is_datafile.f
$(d0root)/physics_util/general/isol_mu_select.f:\
  $(d0root)/physics_util/general/isol_mu_select.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/isol_mu_select.for | vmstounix > $(d0root)/physics_util/general/isol_mu_select.f
$(d0root)/physics_util/general/jet_cone_corr.f:\
  $(d0root)/physics_util/general/jet_cone_corr.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/jet_cone_corr.for | vmstounix > $(d0root)/physics_util/general/jet_cone_corr.f
$(d0root)/physics_util/general/jet_corr.f:\
  $(d0root)/physics_util/general/jet_corr.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izvert.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/jet_corr.for | vmstounix > $(d0root)/physics_util/general/jet_corr.f
$(d0root)/physics_util/general/jet_emf_scale.f:\
  $(d0root)/physics_util/general/jet_emf_scale.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/jet_emf_scale.for | vmstounix > $(d0root)/physics_util/general/jet_emf_scale.f
$(d0root)/physics_util/general/jet_et_true.f:\
  $(d0root)/physics_util/general/jet_et_true.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/jet_et_true.for | vmstounix > $(d0root)/physics_util/general/jet_et_true.f
$(d0root)/physics_util/filters/jet_info.f:\
  $(d0root)/physics_util/filters/jet_info.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/jet_info.for | vmstounix > $(d0root)/physics_util/filters/jet_info.f
$(d0root)/physics_util/general/jet_out_of_cone_corr.f:\
  $(d0root)/physics_util/general/jet_out_of_cone_corr.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/jet_out_of_cone_corr.for | vmstounix > $(d0root)/physics_util/general/jet_out_of_cone_corr.f
$(d0root)/physics_util/general/jet_undzsp_factor.f:\
  $(d0root)/physics_util/general/jet_undzsp_factor.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/jet_undzsp_factor.for | vmstounix > $(d0root)/physics_util/general/jet_undzsp_factor.f
$(d0root)/physics_util/udst/jutlfl.f:\
  $(d0root)/physics_util/udst/jutlfl.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcaph.link\
  $(d0library)/links/izjtcs.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/jutlfl.for | vmstounix > $(d0root)/physics_util/udst/jutlfl.f
$(d0root)/physics_util/general/l15cal_esum_fill.f:\
  $(d0root)/physics_util/general/l15cal_esum_fill.for\
  $(d0library)/params/esum.params\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/level1_lookup.params\
  $(d0library)/params/l1_caltrig.params\
  $(d0library)/params/l1php.params\
  $(d0library)/inc/dbitt.inc\
  $(d0library)/params/byte_order.params\
  $(d0library)/params/l15caldbb_data_block.params
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/l15cal_esum_fill.for | vmstounix > $(d0root)/physics_util/general/l15cal_esum_fill.f
$(d0root)/physics_util/general/l1esum_evt.f:\
  $(d0root)/physics_util/general/l1esum_evt.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/l1esum_evt.for | vmstounix > $(d0root)/physics_util/general/l1esum_evt.f
$(d0root)/physics_util/general/l1esum_muon.f:\
  $(d0root)/physics_util/general/l1esum_muon.for\
  $(d0library)/params/esum.params
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/l1esum_muon.for | vmstounix > $(d0root)/physics_util/general/l1esum_muon.f
$(d0root)/physics_util/general/l1esum_trgr.f:\
  $(d0root)/physics_util/general/l1esum_trgr.for\
  $(d0library)/params/esum.params\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/level1_lookup.params\
  $(d0library)/params/l1_caltrig.params\
  $(d0library)/params/l1php.params\
  $(d0library)/inc/dbitt.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/l1esum_trgr.for | vmstounix > $(d0root)/physics_util/general/l1esum_trgr.f
$(d0root)/physics_util/general/l1esum_trgr2.f:\
  $(d0root)/physics_util/general/l1esum_trgr2.for\
  $(d0library)/params/esum.params\
  $(d0library)/params/level1_lookup.params\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/l1esum_trgr2.for | vmstounix > $(d0root)/physics_util/general/l1esum_trgr2.f
$(d0root)/physics_util/general/lo_calc.f:\
  $(d0root)/physics_util/general/lo_calc.for\
  $(d0library)/inc/lo_com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/lo_calc.for | vmstounix > $(d0root)/physics_util/general/lo_calc.f
$(d0root)/physics_util/general/lo_dsig_dt.f:\
  $(d0root)/physics_util/general/lo_dsig_dt.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/lo_dsig_dt.for | vmstounix > $(d0root)/physics_util/general/lo_dsig_dt.f
$(d0root)/physics_util/general/lo_dsig_dt_sel.f:\
  $(d0root)/physics_util/general/lo_dsig_dt_sel.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/lo_dsig_dt_sel.for | vmstounix > $(d0root)/physics_util/general/lo_dsig_dt_sel.f
$(d0root)/physics_util/general/lo_in_r.f:\
  $(d0root)/physics_util/general/lo_in_r.for\
  $(d0library)/inc/lo_com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/lo_in_r.for | vmstounix > $(d0root)/physics_util/general/lo_in_r.f
$(d0root)/physics_util/general/lo_main_r.f:\
  $(d0root)/physics_util/general/lo_main_r.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/lo_main_r.for | vmstounix > $(d0root)/physics_util/general/lo_main_r.f
$(d0root)/physics_util/general/lo_nt.f:\
  $(d0root)/physics_util/general/lo_nt.for\
  $(d0library)/inc/lo_com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/lo_nt.for | vmstounix > $(d0root)/physics_util/general/lo_nt.f
$(d0root)/physics_util/general/lo_out.f:\
  $(d0root)/physics_util/general/lo_out.for\
  $(d0library)/inc/lo_com.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/lo_out.for | vmstounix > $(d0root)/physics_util/general/lo_out.f
$(d0root)/physics_util/filters/lqnn.f:\
  $(d0root)/physics_util/filters/lqnn.for\
  $(d0library)/links/izpelc.link\
  $(d0library)/links/izppho.link\
  $(d0library)/links/izpnut.link\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/lqnn.for | vmstounix > $(d0root)/physics_util/filters/lqnn.f
$(d0root)/physics_util/filters/lqnue.f:\
  $(d0root)/physics_util/filters/lqnue.for\
  $(d0library)/links/izpelc.link\
  $(d0library)/links/izppho.link\
  $(d0library)/links/izpnut.link\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/lqnue.for | vmstounix > $(d0root)/physics_util/filters/lqnue.f
$(d0root)/physics_util/udst/make_nt.f:\
  $(d0root)/physics_util/udst/make_nt.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/make_nt.for | vmstounix > $(d0root)/physics_util/udst/make_nt.f
$(d0root)/physics_util/udst/make_udst.f:\
  $(d0root)/physics_util/udst/make_udst.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izvert.link\
  $(d0library)/links/izztrk.link\
  $(d0library)/links/izvtxt.link\
  $(d0library)/links/izjets.link\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/inc/udst_links.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/make_udst.for | vmstounix > $(d0root)/physics_util/udst/make_udst.f
$(d0root)/physics_util/udst/make_uint.f:\
  $(d0root)/physics_util/udst/make_uint.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izuint.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/make_uint.for | vmstounix > $(d0root)/physics_util/udst/make_uint.f
$(d0root)/physics_util/general/matchem_mc_data.f:\
  $(d0root)/physics_util/general/matchem_mc_data.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/matchem_mc_data.for | vmstounix > $(d0root)/physics_util/general/matchem_mc_data.f
$(d0root)/physics_util/general/mc_et_corr.f:\
  $(d0root)/physics_util/general/mc_et_corr.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mc_et_corr.for | vmstounix > $(d0root)/physics_util/general/mc_et_corr.f
$(d0root)/physics_util/d0hplt/menu_d0hplt.f:\
  $(d0root)/physics_util/d0hplt/menu_d0hplt.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/d0hplt/menu_d0hplt.for | vmstounix > $(d0root)/physics_util/d0hplt/menu_d0hplt.f
$(d0root)/physics_util/filters/minbias.f:\
  $(d0root)/physics_util/filters/minbias.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/minbias.for | vmstounix > $(d0root)/physics_util/filters/minbias.f
$(d0root)/physics_util/general/miss_et_corr.f:\
  $(d0root)/physics_util/general/miss_et_corr.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/miss_et_corr.for | vmstounix > $(d0root)/physics_util/general/miss_et_corr.f
$(d0root)/physics_util/general/monte_carlo_data.f:\
  $(d0root)/physics_util/general/monte_carlo_data.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/monte_carlo_data.for | vmstounix > $(d0root)/physics_util/general/monte_carlo_data.f
$(d0root)/physics_util/general/mpf_fill_jets.f:\
  $(d0root)/physics_util/general/mpf_fill_jets.for\
  $(d0library)/inc/mpf_jetres.inc\
  $(d0library)/inc/mpf_filljet.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mpf_fill_jets.for | vmstounix > $(d0root)/physics_util/general/mpf_fill_jets.f
$(d0root)/physics_util/general/mpf_jet_response.f:\
  $(d0root)/physics_util/general/mpf_jet_response.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/mpf_jetres.inc\
  $(d0library)/inc/mpf_filljet.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mpf_jet_response.for | vmstounix > $(d0root)/physics_util/general/mpf_jet_response.f
$(d0root)/physics_util/general/mpf_jet_response_fin.f:\
  $(d0root)/physics_util/general/mpf_jet_response_fin.for\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/mpf_jetres.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mpf_jet_response_fin.for | vmstounix > $(d0root)/physics_util/general/mpf_jet_response_fin.f
$(d0root)/physics_util/general/mpf_jet_response_ini.f:\
  $(d0root)/physics_util/general/mpf_jet_response_ini.for\
  $(d0library)/inc/mpf_jetres.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mpf_jet_response_ini.for | vmstounix > $(d0root)/physics_util/general/mpf_jet_response_ini.f
$(d0root)/physics_util/general/mpf_jetcorr.f:\
  $(d0root)/physics_util/general/mpf_jetcorr.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mpf_jetcorr.for | vmstounix > $(d0root)/physics_util/general/mpf_jetcorr.f
$(d0root)/physics_util/general/mpf_jets.f:\
  $(d0root)/physics_util/general/mpf_jets.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/mpf_jetres.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mpf_jets.for | vmstounix > $(d0root)/physics_util/general/mpf_jets.f
$(d0root)/physics_util/general/mpf_low_et_bias_corr.f:\
  $(d0root)/physics_util/general/mpf_low_et_bias_corr.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mpf_low_et_bias_corr.for | vmstounix > $(d0root)/physics_util/general/mpf_low_et_bias_corr.f
$(d0root)/physics_util/general/mpf_mc_jetcorr.f:\
  $(d0root)/physics_util/general/mpf_mc_jetcorr.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mpf_mc_jetcorr.for | vmstounix > $(d0root)/physics_util/general/mpf_mc_jetcorr.f
$(d0root)/physics_util/general/mpf_photons.f:\
  $(d0root)/physics_util/general/mpf_photons.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izjets.link\
  $(d0library)/inc/mpf_jetres.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mpf_photons.for | vmstounix > $(d0root)/physics_util/general/mpf_photons.f
$(d0root)/physics_util/general/mpf_response_fit.f:\
  $(d0root)/physics_util/general/mpf_response_fit.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mpf_response_fit.for | vmstounix > $(d0root)/physics_util/general/mpf_response_fit.f
$(d0root)/physics_util/general/mpf_sim.f:\
  $(d0root)/physics_util/general/mpf_sim.for\
  $(d0library)/inc/mpf_jetres.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mpf_sim.for | vmstounix > $(d0root)/physics_util/general/mpf_sim.f
$(d0root)/physics_util/general/mpf_z_boson.f:\
  $(d0root)/physics_util/general/mpf_z_boson.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/mpf_jetres.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mpf_z_boson.for | vmstounix > $(d0root)/physics_util/general/mpf_z_boson.f
$(d0root)/physics_util/filters/msp_muon_select.f:\
  $(d0root)/physics_util/filters/msp_muon_select.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/msp_muon_select.for | vmstounix > $(d0root)/physics_util/filters/msp_muon_select.f
$(d0root)/physics_util/filters/mu1_b.f:\
  $(d0root)/physics_util/filters/mu1_b.for\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/mu1_b.for | vmstounix > $(d0root)/physics_util/filters/mu1_b.f
$(d0root)/physics_util/filters/mu1_filt.f:\
  $(d0root)/physics_util/filters/mu1_filt.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/mu1_filt.for | vmstounix > $(d0root)/physics_util/filters/mu1_filt.f
$(d0root)/physics_util/filters/mu1_wzt.f:\
  $(d0root)/physics_util/filters/mu1_wzt.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/mu1_wzt.for | vmstounix > $(d0root)/physics_util/filters/mu1_wzt.f
$(d0root)/physics_util/filters/mu2_b.f:\
  $(d0root)/physics_util/filters/mu2_b.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izmuot.link\
  $(d0library)/links/izpmuo.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/mu2_b.for | vmstounix > $(d0root)/physics_util/filters/mu2_b.f
$(d0root)/physics_util/filters/mu2_wzt.f:\
  $(d0root)/physics_util/filters/mu2_wzt.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/mu2_wzt.for | vmstounix > $(d0root)/physics_util/filters/mu2_wzt.f
$(d0root)/physics_util/filters/mu3_b.f:\
  $(d0root)/physics_util/filters/mu3_b.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpmuo.link\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/mu3_b.for | vmstounix > $(d0root)/physics_util/filters/mu3_b.f
$(d0root)/physics_util/filters/mu_b.f:\
  $(d0root)/physics_util/filters/mu_b.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/mu_b.for | vmstounix > $(d0root)/physics_util/filters/mu_b.f
$(d0root)/physics_util/general/mu_unbiased.f:\
  $(d0root)/physics_util/general/mu_unbiased.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mu_unbiased.for | vmstounix > $(d0root)/physics_util/general/mu_unbiased.f
$(d0root)/physics_util/filters/muj_b.f:\
  $(d0root)/physics_util/filters/muj_b.for\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/muj_b.for | vmstounix > $(d0root)/physics_util/filters/muj_b.f
$(d0root)/physics_util/general/mujets_isolation.f:\
  $(d0root)/physics_util/general/mujets_isolation.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/mujets_mu_select.params
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mujets_isolation.for | vmstounix > $(d0root)/physics_util/general/mujets_isolation.f
$(d0root)/physics_util/general/mujets_mu_select.f:\
  $(d0root)/physics_util/general/mujets_mu_select.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/params/mujets_mu_select.params
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/mujets_mu_select.for | vmstounix > $(d0root)/physics_util/general/mujets_mu_select.f
$(d0root)/physics_util/filters/mulq_muon_select.f:\
  $(d0root)/physics_util/filters/mulq_muon_select.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/mulq_muon_select.for | vmstounix > $(d0root)/physics_util/filters/mulq_muon_select.f
$(d0root)/physics_util/filters/mulq_muon_select_tight.f:\
  $(d0root)/physics_util/filters/mulq_muon_select_tight.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/mulq_muon_select_tight.for | vmstounix > $(d0root)/physics_util/filters/mulq_muon_select_tight.f
$(d0root)/physics_util/filters/multi_filter.f:\
  $(d0root)/physics_util/filters/multi_filter.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/multi_filter.for | vmstounix > $(d0root)/physics_util/filters/multi_filter.f
$(d0root)/physics_util/general/multiple_interaction_tool.f:\
  $(d0root)/physics_util/general/multiple_interaction_tool.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/vertex_multi.params
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/multiple_interaction_tool.for | vmstounix > $(d0root)/physics_util/general/multiple_interaction_tool.f
$(d0root)/physics_util/general/multiple_interaction_tool_run1.f:\
  $(d0root)/physics_util/general/multiple_interaction_tool_run1.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/vertex_multi.params
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/multiple_interaction_tool_run1.for | vmstounix > $(d0root)/physics_util/general/multiple_interaction_tool_run1.f
$(d0root)/physics_util/filters/muon_select_np.f:\
  $(d0root)/physics_util/filters/muon_select_np.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/muon_select_np.for | vmstounix > $(d0root)/physics_util/filters/muon_select_np.f
$(d0root)/physics_util/general/muon_vtx_recalc.f:\
  $(d0root)/physics_util/general/muon_vtx_recalc.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/muon_vtx_recalc.for | vmstounix > $(d0root)/physics_util/general/muon_vtx_recalc.f
$(d0root)/physics_util/filters/new_gam_jet.f:\
  $(d0root)/physics_util/filters/new_gam_jet.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/new_gam_jet.for | vmstounix > $(d0root)/physics_util/filters/new_gam_jet.f
$(d0root)/physics_util/filters/np_clean_tau.f:\
  $(d0root)/physics_util/filters/np_clean_tau.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_clean_tau.for | vmstounix > $(d0root)/physics_util/filters/np_clean_tau.f
$(d0root)/physics_util/filters/np_di_lepton_loose.f:\
  $(d0root)/physics_util/filters/np_di_lepton_loose.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_di_lepton_loose.for | vmstounix > $(d0root)/physics_util/filters/np_di_lepton_loose.f
$(d0root)/physics_util/filters/np_di_lepton_tight.f:\
  $(d0root)/physics_util/filters/np_di_lepton_tight.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_di_lepton_tight.for | vmstounix > $(d0root)/physics_util/filters/np_di_lepton_tight.f
$(d0root)/physics_util/filters/np_had.f:\
  $(d0root)/physics_util/filters/np_had.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_had.for | vmstounix > $(d0root)/physics_util/filters/np_had.f
$(d0root)/physics_util/filters/np_lep.f:\
  $(d0root)/physics_util/filters/np_lep.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_lep.for | vmstounix > $(d0root)/physics_util/filters/np_lep.f
$(d0root)/physics_util/filters/np_lq_2em.f:\
  $(d0root)/physics_util/filters/np_lq_2em.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_lq_2em.for | vmstounix > $(d0root)/physics_util/filters/np_lq_2em.f
$(d0root)/physics_util/filters/np_lq_2em_tight.f:\
  $(d0root)/physics_util/filters/np_lq_2em_tight.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_lq_2em_tight.for | vmstounix > $(d0root)/physics_util/filters/np_lq_2em_tight.f
$(d0root)/physics_util/filters/np_lq_enu.f:\
  $(d0root)/physics_util/filters/np_lq_enu.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_lq_enu.for | vmstounix > $(d0root)/physics_util/filters/np_lq_enu.f
$(d0root)/physics_util/filters/np_lss_select.f:\
  $(d0root)/physics_util/filters/np_lss_select.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_lss_select.for | vmstounix > $(d0root)/physics_util/filters/np_lss_select.f
$(d0root)/physics_util/filters/np_lss_tight.f:\
  $(d0root)/physics_util/filters/np_lss_tight.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_lss_tight.for | vmstounix > $(d0root)/physics_util/filters/np_lss_tight.f
$(d0root)/physics_util/filters/np_lss_trilep_loose.f:\
  $(d0root)/physics_util/filters/np_lss_trilep_loose.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/links/izjpts.link\
  $(d0library)/links/izjtsh.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_lss_trilep_loose.for | vmstounix > $(d0root)/physics_util/filters/np_lss_trilep_loose.f
$(d0root)/physics_util/filters/np_lss_trilep_tight.f:\
  $(d0root)/physics_util/filters/np_lss_trilep_tight.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/links/izjpts.link\
  $(d0library)/links/izjtsh.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_lss_trilep_tight.for | vmstounix > $(d0root)/physics_util/filters/np_lss_trilep_tight.f
$(d0root)/physics_util/filters/np_met.f:\
  $(d0root)/physics_util/filters/np_met.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_met.for | vmstounix > $(d0root)/physics_util/filters/np_met.f
$(d0root)/physics_util/filters/np_msp.f:\
  $(d0root)/physics_util/filters/np_msp.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_msp.for | vmstounix > $(d0root)/physics_util/filters/np_msp.f
$(d0root)/physics_util/filters/np_msp_mu_check.f:\
  $(d0root)/physics_util/filters/np_msp_mu_check.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_msp_mu_check.for | vmstounix > $(d0root)/physics_util/filters/np_msp_mu_check.f
$(d0root)/physics_util/filters/np_msp_mu_select.f:\
  $(d0root)/physics_util/filters/np_msp_mu_select.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_msp_mu_select.for | vmstounix > $(d0root)/physics_util/filters/np_msp_mu_select.f
$(d0root)/physics_util/filters/np_mulq.f:\
  $(d0root)/physics_util/filters/np_mulq.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpmuo.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_mulq.for | vmstounix > $(d0root)/physics_util/filters/np_mulq.f
$(d0root)/physics_util/filters/np_mulq_tight.f:\
  $(d0root)/physics_util/filters/np_mulq_tight.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpmuo.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_mulq_tight.for | vmstounix > $(d0root)/physics_util/filters/np_mulq_tight.f
$(d0root)/physics_util/filters/np_munu.f:\
  $(d0root)/physics_util/filters/np_munu.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpnut.link\
  $(d0library)/links/izpmuo.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_munu.for | vmstounix > $(d0root)/physics_util/filters/np_munu.f
$(d0root)/physics_util/filters/np_scalar.f:\
  $(d0root)/physics_util/filters/np_scalar.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_scalar.for | vmstounix > $(d0root)/physics_util/filters/np_scalar.f
$(d0root)/physics_util/filters/np_scalar_tight.f:\
  $(d0root)/physics_util/filters/np_scalar_tight.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_scalar_tight.for | vmstounix > $(d0root)/physics_util/filters/np_scalar_tight.f
$(d0root)/physics_util/filters/np_sqgl.f:\
  $(d0root)/physics_util/filters/np_sqgl.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_sqgl.for | vmstounix > $(d0root)/physics_util/filters/np_sqgl.f
$(d0root)/physics_util/filters/np_sqgl_tight.f:\
  $(d0root)/physics_util/filters/np_sqgl_tight.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def\
  $(d0library)/links/izhmte.link\
  $(d0library)/links/izhmtp.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_sqgl_tight.for | vmstounix > $(d0root)/physics_util/filters/np_sqgl_tight.f
$(d0root)/physics_util/filters/np_tau.f:\
  $(d0root)/physics_util/filters/np_tau.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_tau.for | vmstounix > $(d0root)/physics_util/filters/np_tau.f
$(d0root)/physics_util/filters/np_wright.f:\
  $(d0root)/physics_util/filters/np_wright.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_wright.for | vmstounix > $(d0root)/physics_util/filters/np_wright.f
$(d0root)/physics_util/filters/np_wright_tight.f:\
  $(d0root)/physics_util/filters/np_wright_tight.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/np_wright_tight.for | vmstounix > $(d0root)/physics_util/filters/np_wright_tight.f
$(d0root)/physics_util/general/object_muon1.f:\
  $(d0root)/physics_util/general/object_muon1.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/object_muon1.for | vmstounix > $(d0root)/physics_util/general/object_muon1.f
$(d0root)/physics_util/general/ordx.f:\
  $(d0root)/physics_util/general/ordx.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/ordx.for | vmstounix > $(d0root)/physics_util/general/ordx.f
$(d0root)/physics_util/general/particle_select.f:\
  $(d0root)/physics_util/general/particle_select.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/particle_select.for | vmstounix > $(d0root)/physics_util/general/particle_select.f
$(d0root)/physics_util/filters/phi_difference.f:\
  $(d0root)/physics_util/filters/phi_difference.for\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/phi_difference.for | vmstounix > $(d0root)/physics_util/filters/phi_difference.f
$(d0root)/physics_util/general/phys_cor.f:\
  $(d0root)/physics_util/general/phys_cor.for\
  $(d0library)/params/cal_offline.params\
  $(d0library)/params/l1php.params\
  $(d0library)/inc/cl2_rings.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/phys_cor.for | vmstounix > $(d0root)/physics_util/general/phys_cor.f
$(d0root)/physics_util/general/planarity.f:\
  $(d0root)/physics_util/general/planarity.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/planarity.for | vmstounix > $(d0root)/physics_util/general/planarity.f
$(d0root)/physics_util/dbank/prnt_bank.f:\
  $(d0root)/physics_util/dbank/prnt_bank.for\
  $(d0library)/inc/autof.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/dbank/prnt_bank.for | vmstounix > $(d0root)/physics_util/dbank/prnt_bank.f
$(d0root)/physics_util/filters/ptfix.f:\
  $(d0root)/physics_util/filters/ptfix.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/ptfix.for | vmstounix > $(d0root)/physics_util/filters/ptfix.f
$(d0root)/physics_util/udst/ptopt.f:\
  $(d0root)/physics_util/udst/ptopt.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izvert.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/ptopt.for | vmstounix > $(d0root)/physics_util/udst/ptopt.f
$(d0root)/physics_util/general/qcd_eta_bias.f:\
  $(d0root)/physics_util/general/qcd_eta_bias.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/qcd_eta_bias.for | vmstounix > $(d0root)/physics_util/general/qcd_eta_bias.f
$(d0root)/physics_util/general/qcd_eta_resol.f:\
  $(d0root)/physics_util/general/qcd_eta_resol.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/qcd_eta_resol.for | vmstounix > $(d0root)/physics_util/general/qcd_eta_resol.f
$(d0root)/physics_util/filters/qcd_filt_string.f:\
  $(d0root)/physics_util/filters/qcd_filt_string.for\
  $(d0library)/inc/qcd_bit_names.inc\
  $(d0library)/inc/qcd_bit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/qcd_filt_string.for | vmstounix > $(d0root)/physics_util/filters/qcd_filt_string.f
$(d0root)/physics_util/udst/qcd_filter_bit.f:\
  $(d0root)/physics_util/udst/qcd_filter_bit.for\
  $(d0library)/inc/qcd_bit_names.inc\
  $(d0library)/inc/qcd_bit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/qcd_filter_bit.for | vmstounix > $(d0root)/physics_util/udst/qcd_filter_bit.f
$(d0root)/physics_util/udst/qcd_filter_bit_init.f:\
  $(d0root)/physics_util/udst/qcd_filter_bit_init.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/qcd_bit_names.inc\
  $(d0library)/inc/qcd_bit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/qcd_filter_bit_init.for | vmstounix > $(d0root)/physics_util/udst/qcd_filter_bit_init.f
$(d0root)/physics_util/udst/qcd_filter_trigger.f:\
  $(d0root)/physics_util/udst/qcd_filter_trigger.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/esum.params
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/qcd_filter_trigger.for | vmstounix > $(d0root)/physics_util/udst/qcd_filter_trigger.f
$(d0root)/physics_util/filters/qcd_gamma.f:\
  $(d0root)/physics_util/filters/qcd_gamma.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/qcd_gamma.for | vmstounix > $(d0root)/physics_util/filters/qcd_gamma.f
$(d0root)/physics_util/filters/qcd_gap.f:\
  $(d0root)/physics_util/filters/qcd_gap.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/qcd_gap.for | vmstounix > $(d0root)/physics_util/filters/qcd_gap.f
$(d0root)/physics_util/udst/qcd_get_mask.f:\
  $(d0root)/physics_util/udst/qcd_get_mask.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/qcd_bit_names.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/qcd_get_mask.for | vmstounix > $(d0root)/physics_util/udst/qcd_get_mask.f
$(d0root)/physics_util/general/qcd_jet_correction.f:\
  $(d0root)/physics_util/general/qcd_jet_correction.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/qcd_jet_correction.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/qcd_jet_correction.for | vmstounix > $(d0root)/physics_util/general/qcd_jet_correction.f
$(d0root)/physics_util/filters/qcd_jjj.f:\
  $(d0root)/physics_util/filters/qcd_jjj.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/qcd_jjj.for | vmstounix > $(d0root)/physics_util/filters/qcd_jjj.f
$(d0root)/physics_util/udst/qcd_make_dst.f:\
  $(d0root)/physics_util/udst/qcd_make_dst.for\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izanls.link\
  $(d0library)/links/izcaeq.link\
  $(d0library)/links/izcaid.link\
  $(d0library)/links/izjtcs.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/qcd_make_dst.for | vmstounix > $(d0root)/physics_util/udst/qcd_make_dst.f
$(d0root)/physics_util/filters/qcd_nol.f:\
  $(d0root)/physics_util/filters/qcd_nol.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/qcd_nol.for | vmstounix > $(d0root)/physics_util/filters/qcd_nol.f
$(d0root)/physics_util/filters/qcd_ntr.f:\
  $(d0root)/physics_util/filters/qcd_ntr.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/qcd_ntr.for | vmstounix > $(d0root)/physics_util/filters/qcd_ntr.f
$(d0root)/physics_util/filters/qcd_qjt.f:\
  $(d0root)/physics_util/filters/qcd_qjt.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/qcd_qjt.for | vmstounix > $(d0root)/physics_util/filters/qcd_qjt.f
$(d0root)/physics_util/filters/qcd_sd.f:\
  $(d0root)/physics_util/filters/qcd_sd.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/qcd_sd.for | vmstounix > $(d0root)/physics_util/filters/qcd_sd.f
$(d0root)/physics_util/filters/qcd_sta.f:\
  $(d0root)/physics_util/filters/qcd_sta.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/qcd_sta.for | vmstounix > $(d0root)/physics_util/filters/qcd_sta.f
$(d0root)/physics_util/filters/qcdjets.f:\
  $(d0root)/physics_util/filters/qcdjets.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/qcd_bit_names.inc\
  $(d0library)/inc/qcd_bit.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/qcdjets.for | vmstounix > $(d0root)/physics_util/filters/qcdjets.f
$(d0root)/physics_util/general/recalc_muon.f:\
  $(d0root)/physics_util/general/recalc_muon.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/recalc_muon.for | vmstounix > $(d0root)/physics_util/general/recalc_muon.f
$(d0root)/physics_util/general/reco_esum.f:\
  $(d0root)/physics_util/general/reco_esum.for\
  $(d0library)/params/esum.params\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izvert.link\
  $(d0library)/links/izcaph.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/reco_esum.for | vmstounix > $(d0root)/physics_util/general/reco_esum.f
$(d0root)/physics_util/general/reco_hsum_fill.f:\
  $(d0root)/physics_util/general/reco_hsum_fill.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/reco_hsum_fill.for | vmstounix > $(d0root)/physics_util/general/reco_hsum_fill.f
$(d0root)/physics_util/general/reco_version_select.f:\
  $(d0root)/physics_util/general/reco_version_select.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/reco_version_select.for | vmstounix > $(d0root)/physics_util/general/reco_version_select.f
$(d0root)/physics_util/general/run_select.f:\
  $(d0root)/physics_util/general/run_select.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/run_select.for | vmstounix > $(d0root)/physics_util/general/run_select.f
$(d0root)/physics_util/general/sel_elec_jets.f:\
  $(d0root)/physics_util/general/sel_elec_jets.for\
  $(d0library)/params/isajet_codes.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/sel_elec_jets.for | vmstounix > $(d0root)/physics_util/general/sel_elec_jets.f
$(d0root)/physics_util/general/set_caph_alg.f:\
  $(d0root)/physics_util/general/set_caph_alg.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/set_caph_alg.for | vmstounix > $(d0root)/physics_util/general/set_caph_alg.f
$(d0root)/physics_util/general/set_jet.f:\
  $(d0root)/physics_util/general/set_jet.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/set_jet.for | vmstounix > $(d0root)/physics_util/general/set_jet.f
$(d0root)/physics_util/general/set_traceback_evtnum.f:\
  $(d0root)/physics_util/general/set_traceback_evtnum.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/set_traceback_evtnum.for | vmstounix > $(d0root)/physics_util/general/set_traceback_evtnum.f
$(d0root)/physics_util/general/sm_jet_undzsp_factor.f:\
  $(d0root)/physics_util/general/sm_jet_undzsp_factor.for\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/sm_jet_undzsp_factor.for | vmstounix > $(d0root)/physics_util/general/sm_jet_undzsp_factor.f
$(d0root)/physics_util/filters/sort_descend.f:\
  $(d0root)/physics_util/filters/sort_descend.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/sort_descend.for | vmstounix > $(d0root)/physics_util/filters/sort_descend.f
$(d0root)/physics_util/general/sort_vect.f:\
  $(d0root)/physics_util/general/sort_vect.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/sort_vect.for | vmstounix > $(d0root)/physics_util/general/sort_vect.f
$(d0root)/physics_util/filters/sortbanks.f:\
  $(d0root)/physics_util/filters/sortbanks.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/sortbanks.for | vmstounix > $(d0root)/physics_util/filters/sortbanks.f
$(d0root)/physics_util/general/sphericity.f:\
  $(d0root)/physics_util/general/sphericity.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/sphericity.for | vmstounix > $(d0root)/physics_util/general/sphericity.f
$(d0root)/physics_util/general/sphericity0.f:\
  $(d0root)/physics_util/general/sphericity0.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/sphericity0.for | vmstounix > $(d0root)/physics_util/general/sphericity0.f
$(d0root)/physics_util/udst/split_bitmask.f:\
  $(d0root)/physics_util/udst/split_bitmask.for\
  $(d0library)/params/byte_order.params
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/split_bitmask.for | vmstounix > $(d0root)/physics_util/udst/split_bitmask.f
$(d0root)/physics_util/filters/ss_mu_check.f:\
  $(d0root)/physics_util/filters/ss_mu_check.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/ss_mu_check.for | vmstounix > $(d0root)/physics_util/filters/ss_mu_check.f
$(d0root)/physics_util/filters/ss_mu_chk.f:\
  $(d0root)/physics_util/filters/ss_mu_chk.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/ss_mu_chk.for | vmstounix > $(d0root)/physics_util/filters/ss_mu_chk.f
$(d0root)/physics_util/filters/ss_mu_sel.f:\
  $(d0root)/physics_util/filters/ss_mu_sel.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/ss_mu_sel.for | vmstounix > $(d0root)/physics_util/filters/ss_mu_sel.f
$(d0root)/physics_util/filters/ss_mu_select.f:\
  $(d0root)/physics_util/filters/ss_mu_select.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/ss_mu_select.for | vmstounix > $(d0root)/physics_util/filters/ss_mu_select.f
$(d0root)/physics_util/filters/stream_ejets.f:\
  $(d0root)/physics_util/filters/stream_ejets.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/stream_ejets.for | vmstounix > $(d0root)/physics_util/filters/stream_ejets.f
$(d0root)/physics_util/general/tag_mu_select.f:\
  $(d0root)/physics_util/general/tag_mu_select.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/tag_mu_select.for | vmstounix > $(d0root)/physics_util/general/tag_mu_select.f
$(d0root)/physics_util/filters/tau_filter.f:\
  $(d0root)/physics_util/filters/tau_filter.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/tau_filter.for | vmstounix > $(d0root)/physics_util/filters/tau_filter.f
$(d0root)/physics_util/filters/threejet_filter.f:\
  $(d0root)/physics_util/filters/threejet_filter.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/threejet_filter.for | vmstounix > $(d0root)/physics_util/filters/threejet_filter.f
$(d0root)/physics_util/filters/top_bkg.f:\
  $(d0root)/physics_util/filters/top_bkg.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_bkg.for | vmstounix > $(d0root)/physics_util/filters/top_bkg.f
$(d0root)/physics_util/filters/top_diem.f:\
  $(d0root)/physics_util/filters/top_diem.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_diem.for | vmstounix > $(d0root)/physics_util/filters/top_diem.f
$(d0root)/physics_util/filters/top_ee.f:\
  $(d0root)/physics_util/filters/top_ee.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_ee.for | vmstounix > $(d0root)/physics_util/filters/top_ee.f
$(d0root)/physics_util/filters/top_ejbkg.f:\
  $(d0root)/physics_util/filters/top_ejbkg.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_ejbkg.for | vmstounix > $(d0root)/physics_util/filters/top_ejbkg.f
$(d0root)/physics_util/filters/top_ejet.f:\
  $(d0root)/physics_util/filters/top_ejet.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_ejet.for | vmstounix > $(d0root)/physics_util/filters/top_ejet.f
$(d0root)/physics_util/filters/top_ejsig.f:\
  $(d0root)/physics_util/filters/top_ejsig.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_ejsig.for | vmstounix > $(d0root)/physics_util/filters/top_ejsig.f
$(d0root)/physics_util/filters/top_emu.f:\
  $(d0root)/physics_util/filters/top_emu.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_emu.for | vmstounix > $(d0root)/physics_util/filters/top_emu.f
$(d0root)/physics_util/general/top_event_cleanup.f:\
  $(d0root)/physics_util/general/top_event_cleanup.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/top_event_cleanup.for | vmstounix > $(d0root)/physics_util/general/top_event_cleanup.f
$(d0root)/physics_util/filters/top_jets.f:\
  $(d0root)/physics_util/filters/top_jets.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_jets.for | vmstounix > $(d0root)/physics_util/filters/top_jets.f
$(d0root)/physics_util/filters/top_muj.f:\
  $(d0root)/physics_util/filters/top_muj.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_muj.for | vmstounix > $(d0root)/physics_util/filters/top_muj.f
$(d0root)/physics_util/filters/top_mujet.f:\
  $(d0root)/physics_util/filters/top_mujet.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_mujet.for | vmstounix > $(d0root)/physics_util/filters/top_mujet.f
$(d0root)/physics_util/filters/top_mumu.f:\
  $(d0root)/physics_util/filters/top_mumu.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_mumu.for | vmstounix > $(d0root)/physics_util/filters/top_mumu.f
$(d0root)/physics_util/filters/top_sgl.f:\
  $(d0root)/physics_util/filters/top_sgl.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_sgl.for | vmstounix > $(d0root)/physics_util/filters/top_sgl.f
$(d0root)/physics_util/filters/top_top_bbbar.f:\
  $(d0root)/physics_util/filters/top_top_bbbar.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_top_bbbar.for | vmstounix > $(d0root)/physics_util/filters/top_top_bbbar.f
$(d0root)/physics_util/filters/top_top_ee.f:\
  $(d0root)/physics_util/filters/top_top_ee.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_top_ee.for | vmstounix > $(d0root)/physics_util/filters/top_top_ee.f
$(d0root)/physics_util/filters/top_top_ejet.f:\
  $(d0root)/physics_util/filters/top_top_ejet.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinka.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_top_ejet.for | vmstounix > $(d0root)/physics_util/filters/top_top_ejet.f
$(d0root)/physics_util/filters/top_top_emu.f:\
  $(d0root)/physics_util/filters/top_top_emu.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_top_emu.for | vmstounix > $(d0root)/physics_util/filters/top_top_emu.f
$(d0root)/physics_util/filters/top_top_jets.f:\
  $(d0root)/physics_util/filters/top_top_jets.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_top_jets.for | vmstounix > $(d0root)/physics_util/filters/top_top_jets.f
$(d0root)/physics_util/filters/top_top_mujet.f:\
  $(d0root)/physics_util/filters/top_top_mujet.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_top_mujet.for | vmstounix > $(d0root)/physics_util/filters/top_top_mujet.f
$(d0root)/physics_util/filters/top_top_mumu.f:\
  $(d0root)/physics_util/filters/top_top_mumu.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_top_mumu.for | vmstounix > $(d0root)/physics_util/filters/top_top_mumu.f
$(d0root)/physics_util/filters/top_top_sgl.f:\
  $(d0root)/physics_util/filters/top_top_sgl.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/top_top_sgl.for | vmstounix > $(d0root)/physics_util/filters/top_top_sgl.f
$(d0root)/physics_util/general/topup.f:\
  $(d0root)/physics_util/general/topup.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/topup.for | vmstounix > $(d0root)/physics_util/general/topup.f
$(d0root)/physics_util/general/topup_bkg_err.f:\
  $(d0root)/physics_util/general/topup_bkg_err.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/topup_bkg_err.for | vmstounix > $(d0root)/physics_util/general/topup_bkg_err.f
$(d0root)/physics_util/general/transverse_mass.f:\
  $(d0root)/physics_util/general/transverse_mass.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/transverse_mass.for | vmstounix > $(d0root)/physics_util/general/transverse_mass.f
$(d0root)/physics_util/general/trigger_bit_check.f:\
  $(d0root)/physics_util/general/trigger_bit_check.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/trigger_bit_check.for | vmstounix > $(d0root)/physics_util/general/trigger_bit_check.f
$(d0root)/physics_util/general/tsum_fill.f:\
  $(d0root)/physics_util/general/tsum_fill.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/tsum_fill.for | vmstounix > $(d0root)/physics_util/general/tsum_fill.f
$(d0root)/physics_util/general/tsum_names.f:\
  $(d0root)/physics_util/general/tsum_names.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/tsum_names.for | vmstounix > $(d0root)/physics_util/general/tsum_names.f
$(d0root)/physics_util/filters/twojet_filter.f:\
  $(d0root)/physics_util/filters/twojet_filter.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/pi.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/twojet_filter.for | vmstounix > $(d0root)/physics_util/filters/twojet_filter.f
$(d0root)/physics_util/udst/ucshfl.f:\
  $(d0root)/physics_util/udst/ucshfl.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/ucshfl.for | vmstounix > $(d0root)/physics_util/udst/ucshfl.f
$(d0root)/physics_util/udst/ucwxfl.f:\
  $(d0root)/physics_util/udst/ucwxfl.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/ucwxfl.for | vmstounix > $(d0root)/physics_util/udst/ucwxfl.f
$(d0root)/physics_util/udst/udst.f:\
  $(d0root)/physics_util/udst/udst.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/iztrgr.link\
  $(d0library)/links/izmud1.link\
  $(d0library)/links/izcdd1.link\
  $(d0library)/links/izcdd2.link\
  $(d0library)/links/izcdd3.link\
  $(d0library)/links/izcdd4.link\
  $(d0library)/links/izcad1.link\
  $(d0library)/links/izcad2.link\
  $(d0library)/links/izreco.link\
  $(d0library)/links/izfilt.link\
  $(d0library)/links/izhsum.link\
  $(d0library)/links/izesum.link\
  $(d0library)/links/iztsum.link\
  $(d0library)/links/izucsh.link\
  $(d0library)/links/izutag.link\
  $(d0library)/links/izuint.link\
  $(d0library)/links/izcdtk.link\
  $(d0library)/links/izucwx.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst.for | vmstounix > $(d0root)/physics_util/udst/udst.f
$(d0root)/physics_util/udst/udst_book_group.f:\
  $(d0root)/physics_util/udst/udst_book_group.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/udst_dimensions.params
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_book_group.for | vmstounix > $(d0root)/physics_util/udst/udst_book_group.f
$(d0root)/physics_util/udst/udst_cdtk_errors_pack.f:\
  $(d0root)/physics_util/udst/udst_cdtk_errors_pack.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_cdtk_errors_pack.for | vmstounix > $(d0root)/physics_util/udst/udst_cdtk_errors_pack.f
$(d0root)/physics_util/udst/udst_cdtk_errors_unpack.f:\
  $(d0root)/physics_util/udst/udst_cdtk_errors_unpack.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_cdtk_errors_unpack.for | vmstounix > $(d0root)/physics_util/udst/udst_cdtk_errors_unpack.f
$(d0root)/physics_util/udst/udst_fin.f:\
  $(d0root)/physics_util/udst/udst_fin.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_fin.for | vmstounix > $(d0root)/physics_util/udst/udst_fin.f
$(d0root)/physics_util/udst/udst_fix_bank.f:\
  $(d0root)/physics_util/udst/udst_fix_bank.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_fix_bank.for | vmstounix > $(d0root)/physics_util/udst/udst_fix_bank.f
$(d0root)/physics_util/udst/udst_fix_jets.f:\
  $(d0root)/physics_util/udst/udst_fix_jets.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_fix_jets.for | vmstounix > $(d0root)/physics_util/udst/udst_fix_jets.f
$(d0root)/physics_util/udst/udst_fix_pelc.f:\
  $(d0root)/physics_util/udst/udst_fix_pelc.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_fix_pelc.for | vmstounix > $(d0root)/physics_util/udst/udst_fix_pelc.f
$(d0root)/physics_util/udst/udst_fix_pmuo.f:\
  $(d0root)/physics_util/udst/udst_fix_pmuo.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_fix_pmuo.for | vmstounix > $(d0root)/physics_util/udst/udst_fix_pmuo.f
$(d0root)/physics_util/udst/udst_fix_pnut.f:\
  $(d0root)/physics_util/udst/udst_fix_pnut.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_fix_pnut.for | vmstounix > $(d0root)/physics_util/udst/udst_fix_pnut.f
$(d0root)/physics_util/udst/udst_fix_ptau.f:\
  $(d0root)/physics_util/udst/udst_fix_ptau.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_fix_ptau.for | vmstounix > $(d0root)/physics_util/udst/udst_fix_ptau.f
$(d0root)/physics_util/udst/udst_fix_vtxt.f:\
  $(d0root)/physics_util/udst/udst_fix_vtxt.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_fix_vtxt.for | vmstounix > $(d0root)/physics_util/udst/udst_fix_vtxt.f
$(d0root)/physics_util/udst/udst_get_cash.f:\
  $(d0root)/physics_util/udst/udst_get_cash.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_cash.for | vmstounix > $(d0root)/physics_util/udst/udst_get_cash.f
$(d0root)/physics_util/udst/udst_get_cawx.f:\
  $(d0root)/physics_util/udst/udst_get_cawx.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izcawx.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_cawx.for | vmstounix > $(d0root)/physics_util/udst/udst_get_cawx.f
$(d0root)/physics_util/udst/udst_get_emclus.f:\
  $(d0root)/physics_util/udst/udst_get_emclus.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izvert.link\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zlinkc.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_emclus.for | vmstounix > $(d0root)/physics_util/udst/udst_get_emclus.f
$(d0root)/physics_util/udst/udst_get_glob.f:\
  $(d0root)/physics_util/udst/udst_get_glob.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izfilt.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_glob.for | vmstounix > $(d0root)/physics_util/udst/udst_get_glob.f
$(d0root)/physics_util/udst/udst_get_head.f:\
  $(d0root)/physics_util/udst/udst_get_head.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_head.for | vmstounix > $(d0root)/physics_util/udst/udst_get_head.f
$(d0root)/physics_util/udst/udst_get_jets.f:\
  $(d0root)/physics_util/udst/udst_get_jets.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_jets.for | vmstounix > $(d0root)/physics_util/udst/udst_get_jets.f
$(d0root)/physics_util/udst/udst_get_jnep.f:\
  $(d0root)/physics_util/udst/udst_get_jnep.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_jnep.for | vmstounix > $(d0root)/physics_util/udst/udst_get_jnep.f
$(d0root)/physics_util/udst/udst_get_l12_met.f:\
  $(d0root)/physics_util/udst/udst_get_l12_met.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/esum.params
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_l12_met.for | vmstounix > $(d0root)/physics_util/udst/udst_get_l12_met.f
$(d0root)/physics_util/udst/udst_get_magpol.f:\
  $(d0root)/physics_util/udst/udst_get_magpol.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_magpol.for | vmstounix > $(d0root)/physics_util/udst/udst_get_magpol.f
$(d0root)/physics_util/udst/udst_get_muon.f:\
  $(d0root)/physics_util/udst/udst_get_muon.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_muon.for | vmstounix > $(d0root)/physics_util/udst/udst_get_muon.f
$(d0root)/physics_util/udst/udst_get_pelc_link.f:\
  $(d0root)/physics_util/udst/udst_get_pelc_link.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izpelc.link\
  $(d0library)/links/izppho.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_pelc_link.for | vmstounix > $(d0root)/physics_util/udst/udst_get_pelc_link.f
$(d0root)/physics_util/udst/udst_get_pnu1.f:\
  $(d0root)/physics_util/udst/udst_get_pnu1.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_pnu1.for | vmstounix > $(d0root)/physics_util/udst/udst_get_pnu1.f
$(d0root)/physics_util/udst/udst_get_pnut.f:\
  $(d0root)/physics_util/udst/udst_get_pnut.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_pnut.for | vmstounix > $(d0root)/physics_util/udst/udst_get_pnut.f
$(d0root)/physics_util/udst/udst_get_ptau.f:\
  $(d0root)/physics_util/udst/udst_get_ptau.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_ptau.for | vmstounix > $(d0root)/physics_util/udst/udst_get_ptau.f
$(d0root)/physics_util/udst/udst_get_sahh.f:\
  $(d0root)/physics_util/udst/udst_get_sahh.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_sahh.for | vmstounix > $(d0root)/physics_util/udst/udst_get_sahh.f
$(d0root)/physics_util/udst/udst_get_track_link.f:\
  $(d0root)/physics_util/udst/udst_get_track_link.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/udst_links.inc\
  $(d0library)/links/izztrk.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_track_link.for | vmstounix > $(d0root)/physics_util/udst/udst_get_track_link.f
$(d0root)/physics_util/udst/udst_get_trak.f:\
  $(d0root)/physics_util/udst/udst_get_trak.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_trak.for | vmstounix > $(d0root)/physics_util/udst/udst_get_trak.f
$(d0root)/physics_util/udst/udst_get_version.f:\
  $(d0root)/physics_util/udst/udst_get_version.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_version.for | vmstounix > $(d0root)/physics_util/udst/udst_get_version.f
$(d0root)/physics_util/udst/udst_get_vert.f:\
  $(d0root)/physics_util/udst/udst_get_vert.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_vert.for | vmstounix > $(d0root)/physics_util/udst/udst_get_vert.f
$(d0root)/physics_util/udst/udst_get_vert_link.f:\
  $(d0root)/physics_util/udst/udst_get_vert_link.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/udst_links.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_vert_link.for | vmstounix > $(d0root)/physics_util/udst/udst_get_vert_link.f
$(d0root)/physics_util/udst/udst_get_vfit.f:\
  $(d0root)/physics_util/udst/udst_get_vfit.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_vfit.for | vmstounix > $(d0root)/physics_util/udst/udst_get_vfit.f
$(d0root)/physics_util/udst/udst_get_vtxt.f:\
  $(d0root)/physics_util/udst/udst_get_vtxt.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_vtxt.for | vmstounix > $(d0root)/physics_util/udst/udst_get_vtxt.f
$(d0root)/physics_util/udst/udst_get_vtxt_link.f:\
  $(d0root)/physics_util/udst/udst_get_vtxt_link.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/udst_links.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_get_vtxt_link.for | vmstounix > $(d0root)/physics_util/udst/udst_get_vtxt_link.f
$(d0root)/physics_util/udst/udst_ini.f:\
  $(d0root)/physics_util/udst/udst_ini.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_ini.for | vmstounix > $(d0root)/physics_util/udst/udst_ini.f
$(d0root)/physics_util/udst/udst_out.f:\
  $(d0root)/physics_util/udst/udst_out.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izreco.link\
  $(d0library)/links/izfilt.link\
  $(d0library)/links/izhsum.link\
  $(d0library)/links/izesum.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_out.for | vmstounix > $(d0root)/physics_util/udst/udst_out.f
$(d0root)/physics_util/udst/udst_tag.f:\
  $(d0root)/physics_util/udst/udst_tag.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/params/udst_dimensions.params\
  $(d0library)/inc/utag_data.inc\
  $(d0library)/inc/utag_versions.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_tag.for | vmstounix > $(d0root)/physics_util/udst/udst_tag.f
$(d0root)/physics_util/udst/udst_to_dst.f:\
  $(d0root)/physics_util/udst/udst_to_dst.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/inc/utag_data.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_to_dst.for | vmstounix > $(d0root)/physics_util/udst/udst_to_dst.f
$(d0root)/physics_util/udst/udst_to_dst_fill.f:\
  $(d0root)/physics_util/udst/udst_to_dst_fill.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/zlinkc.inc\
  $(d0library)/inc/quest.inc\
  $(d0library)/inc/utag_data.inc\
  $(d0library)/params/caph.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_to_dst_fill.for | vmstounix > $(d0root)/physics_util/udst/udst_to_dst_fill.f
$(d0root)/physics_util/udst/udst_to_dst_fill_bank.f:\
  $(d0root)/physics_util/udst/udst_to_dst_fill_bank.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_to_dst_fill_bank.for | vmstounix > $(d0root)/physics_util/udst/udst_to_dst_fill_bank.f
$(d0root)/physics_util/udst/udst_to_dst_init.f:\
  $(d0root)/physics_util/udst/udst_to_dst_init.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izutag.link\
  $(d0library)/links/izanls.link\
  $(d0library)/inc/utag_data.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_to_dst_init.for | vmstounix > $(d0root)/physics_util/udst/udst_to_dst_init.f
$(d0root)/physics_util/udst/udst_unpack_version.f:\
  $(d0root)/physics_util/udst/udst_unpack_version.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udst_unpack_version.for | vmstounix > $(d0root)/physics_util/udst/udst_unpack_version.f
$(d0root)/physics_util/udst/udstfl.f:\
  $(d0root)/physics_util/udst/udstfl.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/udstfl.for | vmstounix > $(d0root)/physics_util/udst/udstfl.f
$(d0root)/physics_util/udst/uint_to_dst.f:\
  $(d0root)/physics_util/udst/uint_to_dst.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/links/izuint.link
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/uint_to_dst.for | vmstounix > $(d0root)/physics_util/udst/uint_to_dst.f
$(d0root)/physics_util/general/uncorrect_jets_bank.f:\
  $(d0root)/physics_util/general/uncorrect_jets_bank.for\
  $(d0library)/inc/pi.def\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/uncorrect_jets_bank.for | vmstounix > $(d0root)/physics_util/general/uncorrect_jets_bank.f
$(d0root)/physics_util/filters/uni_filter.f:\
  $(d0root)/physics_util/filters/uni_filter.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/uni_filter.for | vmstounix > $(d0root)/physics_util/filters/uni_filter.f
$(d0root)/physics_util/general/unix_util_install.f:\
  $(d0root)/physics_util/general/unix_util_install.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/unix_util_install.for | vmstounix > $(d0root)/physics_util/general/unix_util_install.f
$(d0root)/physics_util/udst/utag_from_udst.f:\
  $(d0root)/physics_util/udst/utag_from_udst.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/utag_data.inc\
  $(d0library)/inc/utag_versions.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/utag_from_udst.for | vmstounix > $(d0root)/physics_util/udst/utag_from_udst.f
$(d0root)/physics_util/udst/utagfl.f:\
  $(d0root)/physics_util/udst/utagfl.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/udst/utagfl.for | vmstounix > $(d0root)/physics_util/udst/utagfl.f
$(d0root)/physics_util/dbank/vbrowse.f:\
  $(d0root)/physics_util/dbank/vbrowse.for\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/trmdef.def\
  $(d0library)/inc/autof.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/dbank/vbrowse.for | vmstounix > $(d0root)/physics_util/dbank/vbrowse.f
$(d0root)/physics_util/general/verify_zebra.f:\
  $(d0root)/physics_util/general/verify_zebra.for\
  $(d0library)/inc/zebcom.inc\
  $(d0library)/inc/quest.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/verify_zebra.for | vmstounix > $(d0root)/physics_util/general/verify_zebra.f
$(d0root)/physics_util/filters/vertex_filter.f:\
  $(d0root)/physics_util/filters/vertex_filter.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/filters/vertex_filter.for | vmstounix > $(d0root)/physics_util/filters/vertex_filter.f
$(d0root)/physics_util/dbank/zb_smg_pn1.f:\
  $(d0root)/physics_util/dbank/zb_smg_pn1.for\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/trmdef.def\
  $(d0library)/inc/autof.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/dbank/zb_smg_pn1.for | vmstounix > $(d0root)/physics_util/dbank/zb_smg_pn1.f
$(d0root)/physics_util/dbank/zb_smg_pn2.f:\
  $(d0root)/physics_util/dbank/zb_smg_pn2.for\
  $(d0library)/inc/smgdef.def\
  $(d0library)/inc/trmdef.def\
  $(d0library)/inc/autof.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/dbank/zb_smg_pn2.for | vmstounix > $(d0root)/physics_util/dbank/zb_smg_pn2.f
$(d0root)/physics_util/general/zbank_edit.f:\
  $(d0root)/physics_util/general/zbank_edit.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/zbank_edit.for | vmstounix > $(d0root)/physics_util/general/zbank_edit.f
$(d0root)/physics_util/general/zbank_hfn_tag.f:\
  $(d0root)/physics_util/general/zbank_hfn_tag.for
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/zbank_hfn_tag.for | vmstounix > $(d0root)/physics_util/general/zbank_hfn_tag.f
$(d0root)/physics_util/general/zbank_ntuple.f:\
  $(d0root)/physics_util/general/zbank_ntuple.for\
  $(d0library)/offline_util/general/value.def
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/zbank_ntuple.for | vmstounix > $(d0root)/physics_util/general/zbank_ntuple.f
$(d0root)/physics_util/general/zbank_test.f:\
  $(d0root)/physics_util/general/zbank_test.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/zbank_test.for | vmstounix > $(d0root)/physics_util/general/zbank_test.f
$(d0root)/physics_util/general/zbank_val.f:\
  $(d0root)/physics_util/general/zbank_val.for\
  $(d0library)/inc/zebcom.inc
	tasteofd0 $(FLAVOR) < $(d0root)/physics_util/general/zbank_val.for | vmstounix > $(d0root)/physics_util/general/zbank_val.f
for:
	touch $(d0root)/physics_util/general/amoeba.for
	touch $(d0root)/physics_util/dbank/auto_fmt.for
	touch $(d0root)/physics_util/filters/b1m_munearjet.for
	touch $(d0root)/physics_util/filters/b1m_setup_jets.for
	touch $(d0root)/physics_util/general/cacl_esum.for
	touch $(d0root)/physics_util/general/cal_emcluster.for
	touch $(d0root)/physics_util/general/cal_jets.for
	touch $(d0root)/physics_util/general/catalogue_jet_corr.for
	touch $(d0root)/physics_util/general/cd_vertex_info.for
	touch $(d0root)/physics_util/general/clean_cal_junk.for
	touch $(d0root)/physics_util/filters/cm3pos_fix.for
	touch $(d0root)/physics_util/general/correct_jets.for
	touch $(d0root)/physics_util/general/correct_jets_em_remove.for
	touch $(d0root)/physics_util/general/correct_jets_noiseu.for
	touch $(d0root)/physics_util/general/correct_jets_num_interactions.for
	touch $(d0root)/physics_util/general/correct_jets_ooc.for
	touch $(d0root)/physics_util/general/correct_jets_reco_version.for
	touch $(d0root)/physics_util/general/correct_jets_scale_factors.for
	touch $(d0root)/physics_util/general/correct_jets_width.for
	touch $(d0root)/physics_util/general/correct_met.for
	touch $(d0root)/physics_util/general/correct_met_jet_fixup.for
	touch $(d0root)/physics_util/general/correct_old_getlum.for
	touch $(d0root)/physics_util/filters/d0dad_list.for
	touch $(d0root)/physics_util/d0hplt/d0hcdir.for
	touch $(d0root)/physics_util/d0hplt/d0hchd.for
	touch $(d0root)/physics_util/d0hplt/d0hcom.for
	touch $(d0root)/physics_util/d0hplt/d0hend.for
	touch $(d0root)/physics_util/d0hplt/d0hindex.for
	touch $(d0root)/physics_util/d0hplt/d0hinx.for
	touch $(d0root)/physics_util/d0hplt/d0hlas.for
	touch $(d0root)/physics_util/d0hplt/d0hmaklas.for
	touch $(d0root)/physics_util/d0hplt/d0hpld.for
	touch $(d0root)/physics_util/d0hplt/d0hple.for
	touch $(d0root)/physics_util/d0hplt/d0hplt.for
	touch $(d0root)/physics_util/d0hplt/d0hplt_dispatch.for
	touch $(d0root)/physics_util/d0hplt/d0hplt_event.for
	touch $(d0root)/physics_util/d0hplt/d0htyp.for
	touch $(d0root)/physics_util/d0hplt/d0huph.for
	touch $(d0root)/physics_util/dbank/dbank.for
	touch $(d0root)/physics_util/dbank/dcd_chfrm.for
	touch $(d0root)/physics_util/general/deta_to_peta.for
	touch $(d0root)/physics_util/general/dmpdef.for
	touch $(d0root)/physics_util/general/dot.for
	touch $(d0root)/physics_util/general/drop_caph_alg.for
	touch $(d0root)/physics_util/general/duplicate_event_filter.for
	touch $(d0root)/physics_util/dbank/dzfor1.for
	touch $(d0root)/physics_util/filters/el2_b.for
	touch $(d0root)/physics_util/filters/elf.for
	touch $(d0root)/physics_util/filters/elf_med.for
	touch $(d0root)/physics_util/filters/elf_w.for
	touch $(d0root)/physics_util/filters/elf_z.for
	touch $(d0root)/physics_util/filters/em_etzv.for
	touch $(d0root)/physics_util/general/em_fits_mask.for
	touch $(d0root)/physics_util/general/em_in_jet.for
	touch $(d0root)/physics_util/filters/emag_jets.for
	touch $(d0root)/physics_util/filters/emfix.for
	touch $(d0root)/physics_util/general/emphifix.for
	touch $(d0root)/physics_util/udst/esum_compress.for
	touch $(d0root)/physics_util/filters/event_list.for
	touch $(d0root)/physics_util/general/ez_setup_compack.for
	touch $(d0root)/physics_util/general/ezbank.for
	touch $(d0root)/physics_util/filters/fake_e_candidate.for
	touch $(d0root)/physics_util/udst/fill_cdtk.for
	touch $(d0root)/physics_util/udst/fill_nt.for
	touch $(d0root)/physics_util/udst/fill_utag_versions.for
	touch $(d0root)/physics_util/general/filter_bits.for
	touch $(d0root)/physics_util/udst/filtset.for
	touch $(d0root)/physics_util/general/find_nearest_b.for
	touch $(d0root)/physics_util/general/find_nearest_c.for
	touch $(d0root)/physics_util/general/find_nearest_e.for
	touch $(d0root)/physics_util/general/find_nearest_mu.for
	touch $(d0root)/physics_util/general/find_nearest_object.for
	touch $(d0root)/physics_util/general/find_nearest_tau.for
	touch $(d0root)/physics_util/general/find_wlnu.for
	touch $(d0root)/physics_util/general/find_wlnu2.for
	touch $(d0root)/physics_util/general/find_wlnu2d.for
	touch $(d0root)/physics_util/general/find_wlnud.for
	touch $(d0root)/physics_util/general/fix_jet_et.for
	touch $(d0root)/physics_util/general/flusher.for
	touch $(d0root)/physics_util/general/frame_menadd.for
	touch $(d0root)/physics_util/general/fwmoment.for
	touch $(d0root)/physics_util/general/get_caph_alg.for
	touch $(d0root)/physics_util/udst/get_e_variance.for
	touch $(d0root)/physics_util/general/get_electrons.for
	touch $(d0root)/physics_util/general/get_isaj_children.for
	touch $(d0root)/physics_util/general/get_isaj_parton.for
	touch $(d0root)/physics_util/general/get_isajet_children.for
	touch $(d0root)/physics_util/general/get_isajet_parton.for
	touch $(d0root)/physics_util/general/get_isal_children.for
	touch $(d0root)/physics_util/general/get_isal_parton.for
	touch $(d0root)/physics_util/general/get_isaq_children.for
	touch $(d0root)/physics_util/general/get_isaq_parton.for
	touch $(d0root)/physics_util/general/get_jets.for
	touch $(d0root)/physics_util/general/get_l1_l2_info.for
	touch $(d0root)/physics_util/general/get_muons.for
	touch $(d0root)/physics_util/general/get_next_particle.for
	touch $(d0root)/physics_util/general/get_p2neut.for
	touch $(d0root)/physics_util/general/get_partons_in_reco.for
	touch $(d0root)/physics_util/general/get_photons.for
	touch $(d0root)/physics_util/general/get_pjet_vectors.for
	touch $(d0root)/physics_util/general/get_quark_content.for
	touch $(d0root)/physics_util/dbank/get_store_div.for
	touch $(d0root)/physics_util/udst/get_tags.for
	touch $(d0root)/physics_util/general/get_trig_filt_run.for
	touch $(d0root)/physics_util/general/getlum.for
	touch $(d0root)/physics_util/general/getlum_corr.for
	touch $(d0root)/physics_util/dbank/getzeb.for
	touch $(d0root)/physics_util/general/good_electron.for
	touch $(d0root)/physics_util/filters/good_muon_check.for
	touch $(d0root)/physics_util/general/good_particle.for
	touch $(d0root)/physics_util/general/good_photon.for
	touch $(d0root)/physics_util/filters/good_trigger_mu1b.for
	touch $(d0root)/physics_util/udst/gtcdtk.for
	touch $(d0root)/physics_util/dbank/gtchfr.for
	touch $(d0root)/physics_util/dbank/gtzfrm.for
	touch $(d0root)/physics_util/udst/handler.for
	touch $(d0root)/physics_util/general/hisprt.for
	touch $(d0root)/physics_util/general/hot_cell_in_jet.for
	touch $(d0root)/physics_util/d0hplt/hplot_dispatch.for
	touch $(d0root)/physics_util/d0hplt/hplot_menu.for
	touch $(d0root)/physics_util/d0hplt/hplot_proc_menu.for
	touch $(d0root)/physics_util/general/invariant_mass.for
	touch $(d0root)/physics_util/general/is_datafile.for
	touch $(d0root)/physics_util/general/isol_mu_select.for
	touch $(d0root)/physics_util/general/jet_cone_corr.for
	touch $(d0root)/physics_util/general/jet_corr.for
	touch $(d0root)/physics_util/general/jet_emf_scale.for
	touch $(d0root)/physics_util/general/jet_et_true.for
	touch $(d0root)/physics_util/filters/jet_info.for
	touch $(d0root)/physics_util/general/jet_out_of_cone_corr.for
	touch $(d0root)/physics_util/general/jet_undzsp_factor.for
	touch $(d0root)/physics_util/udst/jutlfl.for
	touch $(d0root)/physics_util/general/l15cal_esum_fill.for
	touch $(d0root)/physics_util/general/l1esum_evt.for
	touch $(d0root)/physics_util/general/l1esum_muon.for
	touch $(d0root)/physics_util/general/l1esum_trgr.for
	touch $(d0root)/physics_util/general/l1esum_trgr2.for
	touch $(d0root)/physics_util/general/lo_calc.for
	touch $(d0root)/physics_util/general/lo_dsig_dt.for
	touch $(d0root)/physics_util/general/lo_dsig_dt_sel.for
	touch $(d0root)/physics_util/general/lo_in_r.for
	touch $(d0root)/physics_util/general/lo_main_r.for
	touch $(d0root)/physics_util/general/lo_nt.for
	touch $(d0root)/physics_util/general/lo_out.for
	touch $(d0root)/physics_util/filters/lqnn.for
	touch $(d0root)/physics_util/filters/lqnue.for
	touch $(d0root)/physics_util/udst/make_nt.for
	touch $(d0root)/physics_util/udst/make_udst.for
	touch $(d0root)/physics_util/udst/make_uint.for
	touch $(d0root)/physics_util/general/matchem_mc_data.for
	touch $(d0root)/physics_util/general/mc_et_corr.for
	touch $(d0root)/physics_util/d0hplt/menu_d0hplt.for
	touch $(d0root)/physics_util/filters/minbias.for
	touch $(d0root)/physics_util/general/miss_et_corr.for
	touch $(d0root)/physics_util/general/monte_carlo_data.for
	touch $(d0root)/physics_util/general/mpf_fill_jets.for
	touch $(d0root)/physics_util/general/mpf_jet_response.for
	touch $(d0root)/physics_util/general/mpf_jet_response_fin.for
	touch $(d0root)/physics_util/general/mpf_jet_response_ini.for
	touch $(d0root)/physics_util/general/mpf_jetcorr.for
	touch $(d0root)/physics_util/general/mpf_jets.for
	touch $(d0root)/physics_util/general/mpf_low_et_bias_corr.for
	touch $(d0root)/physics_util/general/mpf_mc_jetcorr.for
	touch $(d0root)/physics_util/general/mpf_photons.for
	touch $(d0root)/physics_util/general/mpf_response_fit.for
	touch $(d0root)/physics_util/general/mpf_sim.for
	touch $(d0root)/physics_util/general/mpf_z_boson.for
	touch $(d0root)/physics_util/filters/msp_muon_select.for
	touch $(d0root)/physics_util/filters/mu1_b.for
	touch $(d0root)/physics_util/filters/mu1_filt.for
	touch $(d0root)/physics_util/filters/mu1_wzt.for
	touch $(d0root)/physics_util/filters/mu2_b.for
	touch $(d0root)/physics_util/filters/mu2_wzt.for
	touch $(d0root)/physics_util/filters/mu3_b.for
	touch $(d0root)/physics_util/filters/mu_b.for
	touch $(d0root)/physics_util/general/mu_unbiased.for
	touch $(d0root)/physics_util/filters/muj_b.for
	touch $(d0root)/physics_util/general/mujets_isolation.for
	touch $(d0root)/physics_util/general/mujets_mu_select.for
	touch $(d0root)/physics_util/filters/mulq_muon_select.for
	touch $(d0root)/physics_util/filters/mulq_muon_select_tight.for
	touch $(d0root)/physics_util/filters/multi_filter.for
	touch $(d0root)/physics_util/general/multiple_interaction_tool.for
	touch $(d0root)/physics_util/general/multiple_interaction_tool_run1.for
	touch $(d0root)/physics_util/filters/muon_select_np.for
	touch $(d0root)/physics_util/general/muon_vtx_recalc.for
	touch $(d0root)/physics_util/filters/new_gam_jet.for
	touch $(d0root)/physics_util/filters/np_clean_tau.for
	touch $(d0root)/physics_util/filters/np_di_lepton_loose.for
	touch $(d0root)/physics_util/filters/np_di_lepton_tight.for
	touch $(d0root)/physics_util/filters/np_had.for
	touch $(d0root)/physics_util/filters/np_lep.for
	touch $(d0root)/physics_util/filters/np_lq_2em.for
	touch $(d0root)/physics_util/filters/np_lq_2em_tight.for
	touch $(d0root)/physics_util/filters/np_lq_enu.for
	touch $(d0root)/physics_util/filters/np_lss_select.for
	touch $(d0root)/physics_util/filters/np_lss_tight.for
	touch $(d0root)/physics_util/filters/np_lss_trilep_loose.for
	touch $(d0root)/physics_util/filters/np_lss_trilep_tight.for
	touch $(d0root)/physics_util/filters/np_met.for
	touch $(d0root)/physics_util/filters/np_msp.for
	touch $(d0root)/physics_util/filters/np_msp_mu_check.for
	touch $(d0root)/physics_util/filters/np_msp_mu_select.for
	touch $(d0root)/physics_util/filters/np_mulq.for
	touch $(d0root)/physics_util/filters/np_mulq_tight.for
	touch $(d0root)/physics_util/filters/np_munu.for
	touch $(d0root)/physics_util/filters/np_scalar.for
	touch $(d0root)/physics_util/filters/np_scalar_tight.for
	touch $(d0root)/physics_util/filters/np_sqgl.for
	touch $(d0root)/physics_util/filters/np_sqgl_tight.for
	touch $(d0root)/physics_util/filters/np_tau.for
	touch $(d0root)/physics_util/filters/np_wright.for
	touch $(d0root)/physics_util/filters/np_wright_tight.for
	touch $(d0root)/physics_util/general/object_muon1.for
	touch $(d0root)/physics_util/general/ordx.for
	touch $(d0root)/physics_util/general/particle_select.for
	touch $(d0root)/physics_util/filters/phi_difference.for
	touch $(d0root)/physics_util/general/phys_cor.for
	touch $(d0root)/physics_util/general/planarity.for
	touch $(d0root)/physics_util/dbank/prnt_bank.for
	touch $(d0root)/physics_util/filters/ptfix.for
	touch $(d0root)/physics_util/udst/ptopt.for
	touch $(d0root)/physics_util/general/qcd_eta_bias.for
	touch $(d0root)/physics_util/general/qcd_eta_resol.for
	touch $(d0root)/physics_util/filters/qcd_filt_string.for
	touch $(d0root)/physics_util/udst/qcd_filter_bit.for
	touch $(d0root)/physics_util/udst/qcd_filter_bit_init.for
	touch $(d0root)/physics_util/udst/qcd_filter_trigger.for
	touch $(d0root)/physics_util/filters/qcd_gamma.for
	touch $(d0root)/physics_util/filters/qcd_gap.for
	touch $(d0root)/physics_util/udst/qcd_get_mask.for
	touch $(d0root)/physics_util/general/qcd_jet_correction.for
	touch $(d0root)/physics_util/filters/qcd_jjj.for
	touch $(d0root)/physics_util/udst/qcd_make_dst.for
	touch $(d0root)/physics_util/filters/qcd_nol.for
	touch $(d0root)/physics_util/filters/qcd_ntr.for
	touch $(d0root)/physics_util/filters/qcd_qjt.for
	touch $(d0root)/physics_util/filters/qcd_sd.for
	touch $(d0root)/physics_util/filters/qcd_sta.for
	touch $(d0root)/physics_util/filters/qcdjets.for
	touch $(d0root)/physics_util/general/recalc_muon.for
	touch $(d0root)/physics_util/general/reco_esum.for
	touch $(d0root)/physics_util/general/reco_hsum_fill.for
	touch $(d0root)/physics_util/general/reco_version_select.for
	touch $(d0root)/physics_util/general/run_select.for
	touch $(d0root)/physics_util/general/sel_elec_jets.for
	touch $(d0root)/physics_util/general/set_caph_alg.for
	touch $(d0root)/physics_util/general/set_jet.for
	touch $(d0root)/physics_util/general/set_traceback_evtnum.for
	touch $(d0root)/physics_util/general/sm_jet_undzsp_factor.for
	touch $(d0root)/physics_util/filters/sort_descend.for
	touch $(d0root)/physics_util/general/sort_vect.for
	touch $(d0root)/physics_util/filters/sortbanks.for
	touch $(d0root)/physics_util/general/sphericity.for
	touch $(d0root)/physics_util/general/sphericity0.for
	touch $(d0root)/physics_util/udst/split_bitmask.for
	touch $(d0root)/physics_util/filters/ss_mu_check.for
	touch $(d0root)/physics_util/filters/ss_mu_chk.for
	touch $(d0root)/physics_util/filters/ss_mu_sel.for
	touch $(d0root)/physics_util/filters/ss_mu_select.for
	touch $(d0root)/physics_util/filters/stream_ejets.for
	touch $(d0root)/physics_util/general/tag_mu_select.for
	touch $(d0root)/physics_util/filters/tau_filter.for
	touch $(d0root)/physics_util/filters/threejet_filter.for
	touch $(d0root)/physics_util/filters/top_bkg.for
	touch $(d0root)/physics_util/filters/top_diem.for
	touch $(d0root)/physics_util/filters/top_ee.for
	touch $(d0root)/physics_util/filters/top_ejbkg.for
	touch $(d0root)/physics_util/filters/top_ejet.for
	touch $(d0root)/physics_util/filters/top_ejsig.for
	touch $(d0root)/physics_util/filters/top_emu.for
	touch $(d0root)/physics_util/general/top_event_cleanup.for
	touch $(d0root)/physics_util/filters/top_jets.for
	touch $(d0root)/physics_util/filters/top_muj.for
	touch $(d0root)/physics_util/filters/top_mujet.for
	touch $(d0root)/physics_util/filters/top_mumu.for
	touch $(d0root)/physics_util/filters/top_sgl.for
	touch $(d0root)/physics_util/filters/top_top_bbbar.for
	touch $(d0root)/physics_util/filters/top_top_ee.for
	touch $(d0root)/physics_util/filters/top_top_ejet.for
	touch $(d0root)/physics_util/filters/top_top_emu.for
	touch $(d0root)/physics_util/filters/top_top_jets.for
	touch $(d0root)/physics_util/filters/top_top_mujet.for
	touch $(d0root)/physics_util/filters/top_top_mumu.for
	touch $(d0root)/physics_util/filters/top_top_sgl.for
	touch $(d0root)/physics_util/general/topup.for
	touch $(d0root)/physics_util/general/topup_bkg_err.for
	touch $(d0root)/physics_util/general/transverse_mass.for
	touch $(d0root)/physics_util/general/trigger_bit_check.for
	touch $(d0root)/physics_util/general/tsum_fill.for
	touch $(d0root)/physics_util/general/tsum_names.for
	touch $(d0root)/physics_util/filters/twojet_filter.for
	touch $(d0root)/physics_util/udst/ucshfl.for
	touch $(d0root)/physics_util/udst/ucwxfl.for
	touch $(d0root)/physics_util/udst/udst.for
	touch $(d0root)/physics_util/udst/udst_book_group.for
	touch $(d0root)/physics_util/udst/udst_cdtk_errors_pack.for
	touch $(d0root)/physics_util/udst/udst_cdtk_errors_unpack.for
	touch $(d0root)/physics_util/udst/udst_fin.for
	touch $(d0root)/physics_util/udst/udst_fix_bank.for
	touch $(d0root)/physics_util/udst/udst_fix_jets.for
	touch $(d0root)/physics_util/udst/udst_fix_pelc.for
	touch $(d0root)/physics_util/udst/udst_fix_pmuo.for
	touch $(d0root)/physics_util/udst/udst_fix_pnut.for
	touch $(d0root)/physics_util/udst/udst_fix_ptau.for
	touch $(d0root)/physics_util/udst/udst_fix_vtxt.for
	touch $(d0root)/physics_util/udst/udst_get_cash.for
	touch $(d0root)/physics_util/udst/udst_get_cawx.for
	touch $(d0root)/physics_util/udst/udst_get_emclus.for
	touch $(d0root)/physics_util/udst/udst_get_glob.for
	touch $(d0root)/physics_util/udst/udst_get_head.for
	touch $(d0root)/physics_util/udst/udst_get_jets.for
	touch $(d0root)/physics_util/udst/udst_get_jnep.for
	touch $(d0root)/physics_util/udst/udst_get_l12_met.for
	touch $(d0root)/physics_util/udst/udst_get_magpol.for
	touch $(d0root)/physics_util/udst/udst_get_muon.for
	touch $(d0root)/physics_util/udst/udst_get_pelc_link.for
	touch $(d0root)/physics_util/udst/udst_get_pnu1.for
	touch $(d0root)/physics_util/udst/udst_get_pnut.for
	touch $(d0root)/physics_util/udst/udst_get_ptau.for
	touch $(d0root)/physics_util/udst/udst_get_sahh.for
	touch $(d0root)/physics_util/udst/udst_get_track_link.for
	touch $(d0root)/physics_util/udst/udst_get_trak.for
	touch $(d0root)/physics_util/udst/udst_get_version.for
	touch $(d0root)/physics_util/udst/udst_get_vert.for
	touch $(d0root)/physics_util/udst/udst_get_vert_link.for
	touch $(d0root)/physics_util/udst/udst_get_vfit.for
	touch $(d0root)/physics_util/udst/udst_get_vtxt.for
	touch $(d0root)/physics_util/udst/udst_get_vtxt_link.for
	touch $(d0root)/physics_util/udst/udst_ini.for
	touch $(d0root)/physics_util/udst/udst_out.for
	touch $(d0root)/physics_util/udst/udst_tag.for
	touch $(d0root)/physics_util/udst/udst_to_dst.for
	touch $(d0root)/physics_util/udst/udst_to_dst_fill.for
	touch $(d0root)/physics_util/udst/udst_to_dst_fill_bank.for
	touch $(d0root)/physics_util/udst/udst_to_dst_init.for
	touch $(d0root)/physics_util/udst/udst_unpack_version.for
	touch $(d0root)/physics_util/udst/udstfl.for
	touch $(d0root)/physics_util/udst/uint_to_dst.for
	touch $(d0root)/physics_util/general/uncorrect_jets_bank.for
	touch $(d0root)/physics_util/filters/uni_filter.for
	touch $(d0root)/physics_util/general/unix_util_install.for
	touch $(d0root)/physics_util/udst/utag_from_udst.for
	touch $(d0root)/physics_util/udst/utagfl.for
	touch $(d0root)/physics_util/dbank/vbrowse.for
	touch $(d0root)/physics_util/general/verify_zebra.for
	touch $(d0root)/physics_util/filters/vertex_filter.for
	touch $(d0root)/physics_util/dbank/zb_smg_pn1.for
	touch $(d0root)/physics_util/dbank/zb_smg_pn2.for
	touch $(d0root)/physics_util/general/zbank_edit.for
	touch $(d0root)/physics_util/general/zbank_hfn_tag.for
	touch $(d0root)/physics_util/general/zbank_ntuple.for
	touch $(d0root)/physics_util/general/zbank_test.for
	touch $(d0root)/physics_util/general/zbank_val.for
