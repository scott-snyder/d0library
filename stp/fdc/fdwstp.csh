#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#========================================================================
#
# Name      : FDWSTP
#
# Purpose   : Creates the two FDC_STPFILEs for FDC event data processing.
#
# Arguments : none
#
#    Created  10-MAY-1988   Jeffrey Bantly
#    2-JUL-1991   Jeffrey Bantly   generate two files now that are
#                                  used, one for Monte Carlo runs
#                                  and one for D0 Hall runs
#
#========================================================================
#
if( $1 == debug )then
  set prefix = deb_
else
  set prefix = ''
endif
#
echo "Linking MC FDC/STP object library"
#
ar x `uff $d0stp/${prefix}fdc.a` fdwstp_mc.o
$f77 $ldflags -o ${prefix}fdwstp fdwstp_mc.o \
  `uff $d0stp/${prefix}fdc.a` \
  `uff $d0fdc_util/${prefix}fdc_util.a` \
  `uff $d0general/${prefix}general.a` \
  `uff $d0unix/${prefix}unix.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
#
echo "Running MC FDC/STP image"
#
ln -sf fdc_mcstpfile.dat fdc_mcstpfile >& /dev/null
eval ${prefix}fdwstp
rm ${prefix}fdwstp fdwstp_mc.o fort.*
#
#-------------------------------------------------------------------
#
echo "Linking D0 FDC/STP object library"
#
ar x `uff $d0stp/${prefix}fdc.a` fdwstp_d0.o
$f77 $ldflags -o ${prefix}fdwstp fdwstp_d0.o \
  `uff $d0stp/${prefix}fdc.a` \
  `uff $d0fdc_util/${prefix}fdc_util.a` \
  `uff $d0general/${prefix}general.a` \
  `uff $d0unix/${prefix}unix.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
#
echo "Running D0 FDC/STP image"
#
ln -sf `uff $d0stp/fdc/fstp.rcp` fstp_rcp >& /dev/null
ln -sf fdc_d0stpfile.dat fdc_d0stpfile >& /dev/null
eval ${prefix}fdwstp
rm ${prefix}fdwstp fdwstp_d0.o fort.*
#
