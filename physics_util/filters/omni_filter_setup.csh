#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#==============================================================================
#
# Name: omni_filter.csh
#
# Purpose: Setup environment and symbolic links for omni_filter program
# 
# 
# Created  25-Feb-1993  Herbert Greenlee
# Modified 07-Jul-1993  Kirill Denisenko
#          13-Sep-1993  Kirill Denisenko V11
#==============================================================================
#
# Generate Symbolic links
#
ln -sf `uff $d0physics_util/fake_e_candidate.rcp` fake_e_candidate_rcp
ln -sf `uff $d0physics_util/top_top_ee.rcp` top_top_ee_rcp
ln -sf `uff $d0physics_util/top_top_ejet.rcp` top_top_ejet_rcp
ln -sf `uff $d0physics_util/top_top_emu.rcp` top_top_emu_rcp
ln -sf `uff $d0physics_util/top_top_jets.rcp` top_top_jets_rcp
ln -sf `uff $d0physics_util/top_top_mujet.rcp` top_top_mujet_rcp
ln -sf `uff $d0physics_util/top_top_mumu.rcp` top_top_mumu_rcp
ln -sf `uff $d0physics_util/top_ee.rcp` top_ee_rcp
ln -sf `uff $d0physics_util/top_ejet.rcp` top_ejet_rcp
ln -sf `uff $d0physics_util/top_emu.rcp` top_emu_rcp
ln -sf `uff $d0physics_util/top_jets.rcp` top_jets_rcp
ln -sf `uff $d0physics_util/top_mujet.rcp` top_mujet_rcp
ln -sf `uff $d0physics_util/top_mumu.rcp` top_mumu_rcp
ln -sf `uff $d0wz/wz_elf_mu.rcp` wz_elf_mu_rcp
ln -sf `uff $d0physics_util/elf_med.rcp` elf_med_rcp
ln -sf `uff $d0physics_util/elf_w.rcp` elf_w_rcp
ln -sf `uff $d0physics_util/elf_z.rcp` elf_z_rcp
ln -sf `uff $d0physics_util/mu1_wzt.rcp` mu1_wzt_rcp
ln -sf `uff $d0physics_util/mu2_wzt.rcp` mu2_wzt_rcp
ln -sf `uff $d0physics_util/mu_b.rcp` mu_b_rcp
ln -sf `uff $d0physics_util/mu2_b.rcp` mu2_b_rcp
ln -sf `uff $d0physics_util/el2_b.rcp` el2_b_rcp
ln -sf `uff $d0physics_util/lqnue.rcp` lqnue_rcp
ln -sf `uff $d0physics_util/lqnn.rcp` lqnn_rcp
ln -sf `uff $d0physics_util/muon_select_np.rcp` muon_select_np_rcp
ln -sf `uff $d0physics_util/ss_mu_sel.rcp` ss_mu_sel_rcp
ln -sf `uff $d0physics_util/np_lq_2em.rcp` np_lq_2em_rcp
ln -sf `uff $d0physics_util/np_lq_2em_tight.rcp` np_lq_2em_tight_rcp
ln -sf `uff $d0physics_util/np_lq_enu.rcp` np_lq_enu_rcp
ln -sf `uff $d0physics_util/np_lss_3lep.rcp` np_lss_3lep_rcp
ln -sf `uff $d0physics_util/np_lss_tight.rcp` np_lss_tight_rcp
ln -sf `uff $d0physics_util/np_scalar.rcp` np_scalar_rcp
ln -sf `uff $d0physics_util/np_scalar_tight.rcp` np_scalar_tight_rcp
ln -sf `uff $d0physics_util/np_sqgl.rcp` np_sqgl_rcp
ln -sf `uff $d0physics_util/np_sqgl_tight.rcp` np_sqgl_tight_rcp
ln -sf `uff $d0physics_util/np_tau.rcp` np_tau_rcp
ln -sf `uff $d0physics_util/np_clean_tau.rcp` np_clean_tau_rcp
ln -sf `uff $d0physics_util/np_msp.rcp` np_msp_rcp
ln -sf `uff $d0physics_util/np_msp_mu_select.rcp` np_msp_mu_select_rcp
ln -sf `uff $d0physics_util/np_lss_select.rcp` np_lss_select_rcp
ln -sf `uff $d0physics_util/np_wright_tight.rcp` np_wright_tight_rcp
ln -sf `uff $d0physics_util/np_wright.rcp` np_wright_rcp
ln -sf `uff $d0physics_util/ss_mu_select.rcp` ss_mu_select_rcp
ln -sf `uff $d0physics_util/qcd_gamma.rcp` qcd_gamma_rcp
ln -sf `uff $d0physics_util/stream_filter_omni_wzp.rcp` multi_filter_rcp
ln -sf `uff $d0physics_util/stream_filter_omni_wzp.rcp` stream_filter_rcp
