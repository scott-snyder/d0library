#! /bin/csh -f
#        D0GEANT_SETUP.COM
#    ASSIGNS LOGICAL VARIABLES TO RUN D0GEANT.EXE
#
#    ANY COMMENTS/COMPLAINTS ETC SHOULD BE DIRECTED TO ME AT:
#    FNAL::JONCKHEERE
#
#     GEANT 'INIT' FILES
ln -sf gsave.dat fort.1 >& /dev/null
ln -sf gsave.dat fort.2 >& /dev/null
#
# ZEBRA GEOMETRY FILE
ln -sf `uff $d0stp/gen_stpfile.dat` gen_stpfile >& /dev/null
ln -sf `uff $d0stp/cal_stpfile.dat` cal_stpfile >& /dev/null
ln -sf `uff $d0stp/cdc_stpfile.dat` cdc_stpfile >& /dev/null
ln -sf `uff $d0stp/fdc_stpfile.dat` fdc_stpfile >& /dev/null
ln -sf `uff $d0stp/trd_stpfile.dat` trd_stpfile >& /dev/null
ln -sf `uff $d0stp/vtx_stpfile.dat` vtx_stpfile >& /dev/null
ln -sf `uff $d0stp/lv0_stpfile.dat` lv0_stpfile >& /dev/null
ln -sf `uff $d0stp/sam_stpfile.dat` sam_stpfile >& /dev/null
ln -sf `uff $d0stp/muo_stpfile_1.dat` muo_stpfile >& /dev/null
#
#  DEFINE TRD FILES:
#     The integrated energy distributions files
#        TRD xray distribution
ln -sf `uff $d0d0geant/data/xraydist.dat` xraydist >& /dev/null
#        TRD delta ray distribution
ln -sf `uff $d0d0geant/data/deldist.dat` deldist >& /dev/null
#       TRD shape distribution
ln -sf `uff $d0d0geant/data/shape_trd.dat` shape_trd >& /dev/null
#       TRD Xray spectrum
ln -sf `uff $d0d0geant/data/xrspect.dat` xrspect >& /dev/null
#       TRD Cathode Strip weights
ln -sf `uff $d0d0geant/data/csweight.dat` trd_strips >& /dev/null
