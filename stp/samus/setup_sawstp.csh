#! /bin/csh -f
#------------------------------------------------
#
# Name      : SETUP_SAWSTP.COM
#
# Purpose   : Setup logical names and commands for
#             programs which create the SAMUS
#             geometrical data in GEANT format.
#
# Created  06-MAY-1991   Vladimir Glebov
#
#------------------------------------------------
#
set INPUT = "$d0stp/samus/"
set OUTPUT = ""
set tag = `echo $1 | tr '[A-Z]' '[a-z]'`
#
#------------------------------------------------
#   Define INPUT files
#------------------------------------------------
#
ln -sf `uff ${INPUT}samus_beam.rcp` samus_beam_rcp >& /dev/null
ls -l samus_beam_rcp
#
ln -sf `uff ${INPUT}samus_magnet.rcp` samus_magnet_rcp >& /dev/null
ls -l samus_magnet_rcp
#
ln -sf `uff ${INPUT}samus_d0station${tag}.rcp` samus_station_rcp >& /dev/null
ls -l samus_station_rcp
#
echo
#------------------------------------------------
#   Define OUTPUT files
#------------------------------------------------
#
ln -sf ${OUTPUT}sam_d0stpfile${tag}.dat sam_stpfile
ls -l sam_stpfile
#
exit:
exit
