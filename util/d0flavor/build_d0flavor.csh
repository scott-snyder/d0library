#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
###############################################################################
#
# Name:  d0flavor.csh
#
# Purpose: UNIX release script to build d0flavor executables.
#
# Usage Notes:
#
# 1.  This script should be executed from the top directory of the
#     util library section.
#
# 2.  This script should be released via the VMS d0library release procedures
#     in the d0flavor group of the util library section.
#
# Created 25-Jun-1992  Herbert Greenlee
#
##############################################################################
#
# Build d0flavor executables
#
ar x `uff $d0util/d0flavor.a` d0flavor.o
$f77 $ldflags -o d0flavor.x d0flavor.o \
  `uff $d0util/d0flavor.a` \
  `uff $d0unix/unix.a`
ar x `uff $d0util/deb_d0flavor.a` d0flavor.o
$f77 $ldflags -o deb_d0flavor.x d0flavor.o \
  `uff $d0util/deb_d0flavor.a` \
  `uff $d0unix/deb_unix.a`
rm d0flavor.o >& /dev/null
