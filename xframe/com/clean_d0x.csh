#!/bin/csh -f

# clean_d0x.csh

# Remove the symbolic links to RCP files used by D0x.
# The environment variable d0x_work_dir must be set to the directory
# where setup_xframe.csh was invoked.

echo 'Deleting RCP links in ' $d0x_work_dir

# Move to the d0x directory, saving the current directory to the stack
# so the user can run this script from anywhere
pushd $d0x_work_dir >/dev/null

set links = ( qcd_jet_correction_rcp  ftraks_rcp  tags_map_rcp     \
              calevt_rcp  cafix_rcp  correctem_rcp  caphel_rcp     \
              cleanmu_rcp  cleanem_rcp  trd_analysis_rcp  trd_rcp  \
              cahits_rcp calevt_rcp calico_rcp cal_module_rcp      \
              csf_ccem_rcp csf_ecem_rcp csf_icd_1a_rcp csf_icd_rcp \
              csf_rcp \
        )

foreach rcp ( $links )

    echo "Deleting link $rcp"
    rm -f $rcp
end

# Restore the user's current directory
popd >/dev/null

exit 0

