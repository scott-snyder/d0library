#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
#========================================================================
#
# Name      : SAWSTP.COM
#
# Purpose   : Creates the SAM_STPFILE data file for SAMUS processing
#
# Arguments : None
#
# Created  28-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov
# Modified  5-MAY-1991   Vladimir Glebov : Update to RCP files
#
#========================================================================
#
if( $1 == debug )then
  set prefix = deb_
else
  set prefix = ''
endif
#
echo "    Linking ${prefix}samus/stp"
ar x `uff $d0stp/${prefix}samus.a` sawstp.o
$f77 $ldflags -o ${prefix}sawstp sawstp.o \
  `uff $d0stp/${prefix}samus.a` \
  `uff $d0muon_util/${prefix}muon_util.a` \
  `uff $d0general/${prefix}general.a` \
  `uff $d0unix/${prefix}unix.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
#
if( $1 != debug )then
  echo "    Running samus/stp image"
  `uff $d0stp/samus/setup_sawstp.csh`
  ${prefix}sawstp
  `uff $d0stp/samus/setup_sawstp.csh` _29JAN93
  ${prefix}sawstp
  `uff $d0stp/samus/setup_sawstp.csh` _02DEC93
  ${prefix}sawstp
  `uff $d0stp/samus/setup_sawstp.csh` _25FEB94
  ${prefix}sawstp
  rm ${prefix}sawstp sawstp.o fort.*
else
  echo "    Will not run deb_sawstp image"
endif
exit:
exit
