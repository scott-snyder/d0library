#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#------------------------------------------------
#
# Name      : CAWSTP
#
# Purpose   : Create and run CAWSTP.EXE to create
#             files CAL_STPFILE.DAT and LV0_STPFILE.DAT
#
# Arguments : None
#
# Created  10-FEB-1989   Harrison B. Prosper
#
#------------------------------------------------
#
if( $1 == debug )then
  set prefix = deb_
else
  set prefix = ''
endif
echo
echo " Create:        ${prefix}cawstp"
echo
#
ar x `uff $d0stp/${prefix}geocal.a` cawstp.o
$f77 $ldflags -o ${prefix}cawstp cawstp.o \
  `uff $d0stp/${prefix}geocal.a` \
  `uff $d0calor_util/${prefix}calor_util.a` \
  `uff $d0general/${prefix}general.a` \
  `uff $d0unix/${prefix}unix.a` \
  `uff $d0cernlib/geant315.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
#
echo
echo " Invoke:        setup_cawstp.csh"
echo
eval `uff $d0stp/geocal/setup_cawstp.csh`
#
if( $1 == debug )then
  echo
  echo "        ${prefix}cawstp created"
  echo "          will not be run"
  echo
  goto exit
endif
#
echo
echo " Run:           ${prefix}cawstp"
echo
#
${prefix}cawstp
#
#------------------------------------------------
#   Tidy up a little
#------------------------------------------------
#
rm ${prefix}cawstp cawstp.o
rm srcp_rest.dat srcp_ecal.dat srcp_ucal.dat srcp_lv0.dat
rm fort.*
exit:
exit
