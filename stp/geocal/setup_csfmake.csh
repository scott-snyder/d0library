#! /bin/csh -f
#========================================================================
#
# Name      : SETUP_CSFMAKE
#
# Purpose   : Define logicals for CSFMAKE
#
# Arguments :
#
# Created   2-MAR-1992   Harrison B. Prosper, Chip Stewart
#
#========================================================================
#
#================================================
#   RCP file containing Conversion factor and
#   Weights
#================================================
ln -sf `uff $d0calor_off/csf.rcp` csf_rcp >& /dev/null
#================================================
#   RCP files containing Sampling Fractions
#================================================
ln -sf `uff $d0calor_off/csf_ccem.rcp` csf_ccem_rcp >& /dev/null
ln -sf `uff $d0calor_off/csf_ecem.rcp` csf_ecem_rcp >& /dev/null
ln -sf `uff $d0calor_off/csf_icd.rcp` csf_icd_rcp >& /dev/null
#================================================
#   Output file for conversion constants
#================================================
ln -sf csf_stpfile.dat csf_stpfile
