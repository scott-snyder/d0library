#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
###############################################################################
#
# Name:  build_fzdiff.csh
#
# Purpose: UNIX release script to build fzdiff executables.
#
# Usage Notes:
#
# 1.  This script should be executed from the top directory of the
#     util library section.
#
# 2.  This script should be released via the VMS d0library release procedures
#     in the fzdiff group of the util library section.
#
# Created 25-Jun-1992  Herbert Greenlee
#
##############################################################################
#
# Build fzdiff executables
#
ar x `uff $d0util/fzdiff.a` fzdiff.o
$f77 $ldflags -o fzdiff.x fzdiff.o \
  `uff $d0util/fzdiff.a` \
  `uff $d0physics_util/physics_util.a` \
  `uff $d0general/general.a` \
  `uff $d0unix/unix.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
ar x `uff $d0util/deb_fzdiff.a` fzdiff.o
$f77 $ldflags -o deb_fzdiff.x fzdiff.o \
  `uff $d0util/deb_fzdiff.a` \
  `uff $d0physics_util/deb_physics_util.a` \
  `uff $d0general/deb_general.a` \
  `uff $d0unix/deb_unix.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
rm fzdiff.o >& /dev/null
#
# Build fzsort executables
#
ar x `uff $d0util/fzdiff.a` fzsort.o
$f77 $ldflags -o fzsort.x fzsort.o \
  `uff $d0util/fzdiff.a` \
  `uff $d0general/general.a` \
  `uff $d0unix/unix.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
ar x `uff $d0util/deb_fzdiff.a` fzsort.o
$f77 $ldflags -o deb_fzsort.x fzsort.o \
  `uff $d0util/deb_fzdiff.a` \
  `uff $d0general/deb_general.a` \
  `uff $d0unix/deb_unix.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
rm fzsort.o >& /dev/null
#
# Build fzscramble executables
#
ar x `uff $d0util/fzdiff.a` fzscramble.o
$f77 $ldflags -o fzscramble.x fzscramble.o \
  `uff $d0util/fzdiff.a` \
  `uff $d0general/general.a` \
  `uff $d0unix/unix.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
ar x `uff $d0util/deb_fzdiff.a` fzscramble.o
$f77 $ldflags -o deb_fzscramble.x fzscramble.o \
  `uff $d0util/deb_fzdiff.a` \
  `uff $d0general/deb_general.a` \
  `uff $d0unix/deb_unix.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/kernlib.a` \
  $syslibs
rm fzscramble.o >& /dev/null
#
# Build fzton executables
#
ar x `uff $d0util/fzdiff.a` fzton.o
$f77 $ldflags -o fzton.x fzton.o \
  `uff $d0util/fzdiff.a` \
  `uff $d0calor_util/calor_util.a` \
  `uff $d0cd_util/cd_util.a` \
  `uff $d0cdc_util/cdc_util.a` \
  `uff $d0fdc_util/fdc_util.a` \
  `uff $d0vtx_util/vtx_util.a` \
  `uff $d0trd_util/trd_util.a` \
  `uff $d0cd_util/cd_util.a` \
  `uff $d0muon_util/muon_util.a` \
  `uff $d0lcompack/compack.a` \
  `uff $d0general/general.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/mathlib.a` \
  `uff $d0cernlib/kernlib.a` \
  `uff $d0unix/deb_unix.a` \
  $syslibs
ar x `uff $d0util/deb_fzdiff.a` fzton.o
$f77 $ldflags -o deb_fzton.x fzton.o \
  `uff $d0util/deb_fzdiff.a` \
  `uff $d0calor_util/deb_calor_util.a` \
  `uff $d0cd_util/deb_cd_util.a` \
  `uff $d0cdc_util/deb_cdc_util.a` \
  `uff $d0fdc_util/deb_fdc_util.a` \
  `uff $d0vtx_util/deb_vtx_util.a` \
  `uff $d0trd_util/deb_trd_util.a` \
  `uff $d0cd_util/deb_cd_util.a` \
  `uff $d0muon_util/deb_muon_util.a` \
  `uff $d0lcompack/deb_compack.a` \
  `uff $d0general/deb_general.a` \
  `uff $d0cernlib/packlib.a` \
  `uff $d0cernlib/mathlib.a` \
  `uff $d0cernlib/kernlib.a` \
  `uff $d0unix/deb_unix.a` \
  $syslibs
rm fzton.o >& /dev/null
