#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#------------------------------------------------
#
#     R.Raja 31-Jul-1989
#
#   Creates the GEN_STPFILE data file for GEN processing
#   it uses the files from D0$STP:GEOGEN
#
#
#------------------------------------------------
#
if( $1 == debug )then
  set prefix = deb_
else
  set prefix = ''
endif
echo  "Linking GEOGEN/STP object library"
ar x `uff $d0stp/${prefix}geogen.a` gnwstp.o
$f77 $ldflags -o ${prefix}gnwstp gnwstp.o \
  `uff $d0stp/${prefix}geogen.a` \
  `uff $d0general/${prefix}general.a` \
  `uff $d0unix/${prefix}unix.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
echo "Running GEOGEN/STP image"
ln -sf `uff $d0stp/geogen/gnwstp.rcp` gnwstp_rcp >& /dev/null
ln -sf gen_stpfile.dat gen_stpfile >& /dev/null
${prefix}gnwstp
rm ${prefix}gnwstp gnwstp.o
rm fort.*
