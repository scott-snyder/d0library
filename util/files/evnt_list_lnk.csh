#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#------------------------------------------------
#
# Name      : EVNT_LIST
#
# Purpose   : Compile and link EVNT_LIST.EXE
#
# Arguments : P1 = DEBUG or ""
#
# Created   3-NOV-1989   Alan M. Jonckheere
#
#------------------------------------------------
#
if( `echo $1 | cut -c1 | tr D d` == d )then
  set deb_     = "deb_"
  echo "Linking evnt_list debug executable."
else
  set deb_     = ""
  echo "Linking evnt_list optimized executable."
endif
#
ar x `uff $d0util/${deb_}util.a` evnt_list.o
$f77 $ldflags -o ${deb_}evnt_list.x evnt_list.o \
  `uff $d0util/${deb_}util.a` \
  `uff $d0calor_util/${deb_}calor_util.a` \
  `uff $d0general/${deb_}general.a` \
  `uff $d0tpmfarm/${deb_}tpmfarm.a` \
  `uff $d0general/${deb_}general.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  `uff $d0unix/${deb_}unix.a` \
  $syslibs
