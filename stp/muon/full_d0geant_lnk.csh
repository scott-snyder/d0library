#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#------------------------------------------------
#
# Name      : full_d0geant_lnk.csh
#
# Purpose   : Link full_d0geant
#
# Arguments : $1 "debug" to create deb_full_d0geant.exe
#
# Created   23-Jun-1992  Herbert Greenlee
#
#------------------------------------------------
#
if( $1 == debug )then
 set prefix = deb_
else
 set prefix = ''
endif
#
#
echo "linking ${prefix}full_d0geant"
echo
ar x `uff $d0d0geant/${prefix}d0geant.a` dzero.o
$f77 $ldflags -o ${prefix}full_d0geant dzero.o \
  `uff $d0d0geant/full_d0geant.o` \
  `uff $d0d0geant/${prefix}d0geant.a` \
  `uff $d0showerlibrary/${prefix}showerlibrary.a` \
  `uff $d0calor_util/${prefix}calor_util.a` \
  `uff $d0cd_util/${prefix}cd_util.a` \
  `uff $d0trd_util/${prefix}trd_util.a` \
  `uff $d0vtx_util/${prefix}vtx_util.a` \
  `uff $d0cdc_util/${prefix}cdc_util.a` \
  `uff $d0fdc_util/${prefix}fdc_util.a` \
  `uff $d0cd_util/${prefix}cd_util.a` \
  `uff $d0muon_util/${prefix}muon_util.a` \
  `uff $d0general/${prefix}general.a` \
  `uff $d0unix/${prefix}unix.a` \
  `uff $d0cernlib/gxint.o` \
  `uff $d0cernlib/geant.a` \
  `uff $d0cernlib/pawlib.a` \
  `uff $d0cernlib/graflib.a` \
  `uff $d0cernlib/grafX11.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/genlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
echo "Link done"
echo
#
exit:
exit
