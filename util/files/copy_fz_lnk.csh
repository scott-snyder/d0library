#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#------------------------------------------------
#
# Name      : COPY_FZ
#
# Purpose   : Compile and link COPY_FZ.EXE
#
# Arguments : P1 = DEBUG or ""
#
# Created   3-NOV-1989   Alan M. Jonckheere
#
#------------------------------------------------
#
if( `echo $1 | cut -c1 | tr D d` == d )then
  set deb_     = "deb_"
  echo "Linking copy_fz debug executable."
else
  set deb_     = ""
  echo "Linking copy_fz optimized executable."
endif
#
ar x `uff $d0util/${deb_}util.a` copy_fz.o
$f77 $ldflags -o ${deb_}copy_fz.x copy_fz.o \
  `uff $d0util/${deb_}util.a` \
  `uff $d0general/${deb_}general.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  `uff $d0tpmfarm/${deb_}tpmfarm.a` \
  `uff $d0unix/${deb_}unix.a` \
  $syslibs
