#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#------------------------------------------------
#
#     Ghita Rahal-Callot      17-FEB-1988
#
#   Creates the VTX_STPFILE data file for VTX processing
#   it uses the files from D0$STP:VTX
#
#
#------------------------------------------------
#
if( $1 == debug )then
  set prefix = deb_
else
  set prefix = ''
endif
#
echo "Linking VTX/STP object library"
ar x `uff $d0stp/${prefix}vtx.a` vtwstp.o
$f77 $ldflags -o ${prefix}vtwstp vtwstp.o \
  `uff $d0stp/${prefix}vtx.a` \
  `uff $d0vtx_util/${prefix}vtx_util.a` \
  `uff $d0cd_util/${prefix}cd_util.a` \
  `uff $d0general/${prefix}general.a` \
  `uff $d0unix/${prefix}unix.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
echo "Running VTX/STP image"
ln -sf vtx_stpfile.dat vtx_stpfile >& /dev/null
ln -sf vtx_d0stpfile.dat vtx_d0stpfile >& /dev/null
ln -sf `uff $d0stp/vtx/vtwstp.rcp` vtwstp_rcp
eval ${prefix}vtwstp
rm ${prefix}vtwstp vtwstp.o fort.*
