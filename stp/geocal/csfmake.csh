#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#------------------------------------------------
#
# Name      : CSFMAKE
#
# Purpose   : Create and run CSFMAKE.EXE to create
#             file CSF_STPFILE.DAT
#
# Arguments : None
#
# Created  15-APR-1992   Chip Stewart
#
#------------------------------------------------
#
if( $1 == debug )then
  set prefix = deb_
else
  set prefix = ''
endif
#
echo "  "
echo " Create:        ${prefix}csfmake"
echo "  "
#
ar x `uff $d0stp/${prefix}geocal.a` csfmake.o
$f77 $ldflags -o ${prefix}csfmake csfmake.o \
  `uff $d0stp/${prefix}geocal.a` \
  `uff $d0calor_util/${prefix}calor_util.a` \
  `uff $d0general/${prefix}general.a` \
  `uff $d0unix/${prefix}unix.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
echo "  "
echo " Invoke:        setup_csfmake.csh"
echo "  "
eval `uff $d0stp/geocal/setup_csfmake.csh`
#
if( $1 == debug )then
  echo
  echo "        deb_csfmake created"
  echo "          will not be run"
  echo
  goto exit
endif
#
echo "  "
echo " Run:           csfmake.exe"
echo "  "
#
eval ${prefix}csfmake
#
#------------------------------------------------
#   Tidy up a little
#------------------------------------------------
rm ${prefix}csfmake csfmake.o fort.*
exit:
exit
