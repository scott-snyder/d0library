#! /bin/csh -f
source `uff $d0unix/d0local.cshrc`
###############################################################################
#
# Name:  build_tpmfarm.csh
#
# Purpose: UNIX release script to build tpmfarm library.
#
# Usage Notes:
#
# 1.  This script should be executed from the top directory of the
#     util library section.
#
# 2.  This script should be released via the VMS d0library release procedures
#     in the tpmfarm group of the util library section.
#
# Created 25-Jun-1992  Herbert Greenlee
#
##############################################################################
#
# Generate makefile
#
ufn '$d0tpmfarm/source/*.for' '$d0tpmfarm/source/*.[cf]' \
  '$d0tpmfarm/tcpip/*.[cf]' '$d0tpmfarm/shrutil/*.c' \
  '$d0tpmfarm/dbl3server/*.for' | \
  userlib -c $d0root -l tpmfarm.a -o tpmfarm.makefile -
#
# Build library.
#
make -f tpmfarm.makefile for
make -f tpmfarm.makefile pre
make -f tpmfarm.makefile debug
make -f tpmfarm.makefile opt
#
# Move main programs from tpmfarm.a and deb_tpmfarm.a to 
# mains.a and deb_mains.a
#
d0echo "\nBuilding main program libraries\n"
foreach main (`main_list tpmfarm.a`)
  ar x tpmfarm.a $main
  ar $arflags mains.a $main >& /dev/null
  ar dsl tpmfarm.a $main >& /dev/null
  rm $main
end
foreach main (`main_list deb_tpmfarm.a`)
  ar x deb_tpmfarm.a $main
  ar $arflags deb_mains.a $main >& /dev/null
  ar dsl deb_tpmfarm.a $main >& /dev/null
  rm $main
end
