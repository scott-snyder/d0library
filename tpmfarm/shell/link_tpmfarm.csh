#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
###############################################################################
#
# Name:  link_tpmfarm.csh
#
# Purpose: UNIX release script to link tpmfarm main programs.
#
# Created 19-Nov-1994  Herbert Greenlee
#
##############################################################################
set prefix = ''
libloop:
set mainlib = `uff $d0tpmfarm/${prefix}mains.a`
foreach main ( `ar t $mainlib` )
  set exe = ${prefix}${main:r}.x
  echo "Linking $exe"
  ar x $mainlib $main
  $f77 -o $exe $ldflags $main \
    `uff $d0tpmfarm/${prefix}tpmfarm.a` \
    `uff $d0general/${prefix}general.a` \
    `uff $d0unix/${prefix}unix.a` \
    `uff $d0cernlib/packlib.a` \
    `uff $d0cernlib/kernlib.a` \
    $RBIO_LIB \
    $syslibs
  rm $main
end
if( "$prefix" == '' )then
  set prefix = deb_
  goto libloop
endif

