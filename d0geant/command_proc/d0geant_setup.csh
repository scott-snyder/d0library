#        D0GEANT_SETUP.COM
#
#     GEANT 'INIT' FILES
ln -sf gsave.dat fort.1
ln -sf gsave.dat fort.2
#
# ZEBRA geometry file
ln -sf `uff $d0stp/gen_stpfile.dat` gen_stpfile
ln -sf `uff $d0stp/cal_stpfile.dat` cal_stpfile
ln -sf `uff $d0stp/cdc_stpfile.dat` cdc_stpfile
ln -sf `uff $d0stp/trd_stpfile.dat` trd_stpfile
ln -sf `uff $d0stp/vtx_stpfile.dat` vtx_stpfile
ln -sf `uff $d0stp/lv0_stpfile.dat` lv0_stpfile
ln -sf `uff $d0stp/sam_d0stpfile.dat` sam_stpfile
ln -sf `uff $d0stp/fdc_mcstpfile.dat` fdc_stpfile
ln -sf `uff $d0stp/muo_stpfile_srv_2.dat` muo_stpfile
#
#  DEFINE TRD FILES
#     The integrated energy distributions files
#        TRD xray distribution
ln -sf `uff $d0d0geant/data/xraydist.dat` XRAYDIST
#        TRD delta ray distribution
ln -sf `uff $d0d0geant/data/deldist.dat` DELDIST
#       TRD shape distribution
ln -sf `uff $d0d0geant/data/shape_trd.dat` SHAPE_TRD
#       TRD Xray spectrum
ln -sf `uff $d0d0geant/data/xrspect.dat` XRSPECT
#       TRD Cathode Strip weights
ln -sf `uff $d0d0geant/data/csweight.dat` TRD_STRIPS
ln -sf `uff $d0d0geant/data/trd_nosub0.dat` NOSUB0
ln -sf `uff $d0d0geant/data/trd_urmc.dat` URL1L2
#
#  DEFINE VTX DATA FILE FOR PULSE SHAPES
#
ln -sf `uff $d0d0geant/data/pulse_param.dat` pulse_param
#
#  DEFINE RCP FILES FOR CALORIMETER
#
ln -sf `uff $d0calor_off/mc_cadmake.rcp` cadmake_rcp
#
# DEFINE RCP FILE FOR LEVEL 0
#
ln -sf `uff $d0d0geant/mclevel0.rcp` mclevel0_rcp
#
# DEFINE RCP FILE FOR SAMUS
#
ln -sf `uff $d0muon_reco/samreco.rcp` samreco_rcp
#
#   NOW DO THE TEST DATA FILES - THESE ARE ISAJET FILES
ln -sf `uff $d0d0geant/data/ttb_140.dat` fort.31
ln -sf ttb_140.gen fort.32
#
# `uff $d0showerlibrary/setup_shlb_use.csh`
