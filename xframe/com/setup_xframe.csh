#! /bin/csh -f 

setenv UIDPATH `uff $d0xframe/source/d0xuid`
setenv d0x_work_dir `pwd`
alias clean_d0x `uff $d0xframe/clean_d0x.csh`
alias d0x `uff $d0xframe/d0x` -title "'Non Illegitimus Carborundum'"
alias d0x_color "setenv UIDPATH `uff $d0xframe/source/d0xuid`"
alias d0x_bw "setenv UIDPATH `uff $d0xframe/source/d0xbw`"

echo " ********************************************************* "
echo " * d0x    now available - default for COLOR              * "
echo " * for B&W use the alias     d0x_bw                      * "
echo " * for COLOR use the alias   d0x_color                   * "
echo " ********************************************************* "
#
# setup symbolic links
#
ln -sf `uff $d0physics_util/general/qcd_jet_correction.rcp` \
    qcd_jet_correction_rcp
echo " link created for QCD_JET_CORRECTION"

ln -sf `uff $d0fdc_util/ftraks.rcp` ftraks_rcp
echo " link created for FTRAKS_RCP"

ln -sf `uff $d0physics_util/tags_map.rcp` tags_map_rcp
echo " link created for TAGS_MAP_RCP"

ln -sf `uff $d0calor_off/calevt.rcp` calevt_rcp
echo " link created for CALEVT_RCP"

ln -sf `uff $d0calor_off/cafix.rcp` cafix_rcp
echo " link created for CAFIX_RCP"

ln -sf `uff $d0calor_off/correctem.rcp` correctem_rcp
echo " link created for CORRECTEM_RCP"

ln -sf `uff $d0calor_off/caphel.rcp` caphel_rcp
echo " link created for CAPHEL_RCP"

ln -sf `uff $d0muon_reco/cleanmu.rcp` cleanmu_rcp
echo " link created for CLEANMU_RCP"

ln -sf `uff $d0calor_util/cleanem.rcp` cleanem_rcp
echo " link created for CLEANEM_RCP"

ln -sf `uff $d0trd_util/trd_analysis.rcp` trd_analysis_rcp
echo " link created for TRD_ANALYSIS_RCP"

ln -sf `uff $d0trd_util/trd.rcp` trd_rcp
echo " link created for TRD_RCP"

ln -sf `uff $d0calor_off/cahits.rcp` cahits_rcp
echo " link created for CAHITS_RCP"

ln -sf `uff $d0calor_off/calevt.rcp` calevt_rcp
echo " link created for CALEVT_RCP"

ln -sf `uff $d0calor_off/calico.rcp` calico_rcp
echo " link created for CALICO_RCP"

ln -sf `uff $d0calor_off/cal_module.rcp` cal_module_rcp
echo " link created for CAL_MODULE_RCP"

ln -sf `uff $d0calor_off/csf_ccem.rcp` csf_ccem_rcp
echo " link created for CSF_CCEM_RCP"

ln -sf `uff $d0calor_off/csf_ecem.rcp` csf_ecem_rcp
echo " link created for CSF_ECEM_RCP"

ln -sf `uff $d0calor_off/csf_icd_1a.rcp` csf_icd_1a_rcp
echo " link created for CSF_ICD_1A_RCP"

ln -sf `uff $d0calor_off/csf_icd.rcp` csf_icd_rcp
echo " link created for CSF_ICD_RCP"

ln -sf `uff $d0calor_off/csf.rcp` csf_rcp
echo " link created for CSF_RCP"



echo " "
echo " ********************************************************* "
echo " * to clean up these RCP files, use the following alias: * "
echo " *                                                       * "
echo " * clean_d0x                                             * "
echo " ********************************************************* "
