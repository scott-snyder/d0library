#! /bin/csh -f
#------------------------------------------------------------------------------
#
# Name      : libtest.csh
#
# Purpuse   : Put a library into test mode.  If the test directory does not
#             exist, then the library remains in official mode.
#
# Arguments : The name of the library section to be libtested or 'all'.
#
# Created 21-JUN-1991   Herbert B. Greenlee
#
# This script must be sourced to change the calling process's environment.  
# This is normally done by the alias libtest deifned in d0local.cshrc.
#
# Usage:
#
# libtest [-bgtop] [-a area] [section ... |all]
#
# Options:
#
# -a Alpha mode.  The alpha area is specified as an argument to this option.
# -b Beta mode (root $d0beta).
# -g Gamma mode (root $d0gamma).
# -t Test mode (root $d0test).  This is default.
# -o Official mode (root $d0library).  Turns off all non-official libraries.
# -p Print values of environment variables that are set.
#
#------------------------------------------------------------------------------
#
# parse options
#
set argv=(`getopt a:bgtop $argv:q`)
set libtest_alpha=0
set libtest_beta=0          # beta mode flag
set libtest_gamma=0         # gamma mode flag
set libtest_test=1          # test mode flag
set libtest_print=0         # print mode
while("$argv[1]" != '--')
  switch($argv[1])
  case -a:                # alpha mode
    set libtest_alpha=1
    set libtest_area=$argv[2]
    repeat 2 shift
    breaksw
  case -b:                # beta mode
    set libtest_beta=1
    shift
    breaksw
  case -g:                # gamma mode
    set libtest_gamma=1
    shift
    breaksw
  case -t:                # test mode
    set libtest_test=1
    shift
    breaksw
  case -o:                # official mode
    set libtest_alpha=0
    set libtest_beta=0
    set libtest_gamma=0
    set libtest_test=0
    shift
    breaksw
  case -p:                # print mode
    set libtest_print=1
    shift
    breaksw
  default:
    echo "libtest: bad option: $argv"
    exit 1
  endsw
end
shift     # drop final --
#
# Construct search list
#
if($argv == all)set argv=`all_sect`
foreach libtest_sect ($argv)
  set libtest_lib=''
  if($libtest_alpha)then
    if(-d $libtest_area)set libtest_lib=($libtest_lib $libtest_area)
  endif
  if($libtest_beta && $?d0beta)then
    if(-d $d0beta/$libtest_sect) \
      set libtest_lib=($libtest_lib $d0beta/$libtest_sect)
  endif
  if($libtest_gamma && $?d0gamma)then
    if(-d $d0gamma/$libtest_sect) \
      set libtest_lib=($libtest_lib $d0gamma/$libtest_sect)
  endif
  if($libtest_test && $?d0test)then
    if(-d $d0test/$libtest_sect) \
      set libtest_lib=($libtest_lib $d0test/$libtest_sect)
  endif
  if(-d $d0library/$libtest_sect) \
    set libtest_lib=($libtest_lib $d0library/$libtest_sect)
#
# Set d0 environment variable
#
  if($#libtest_lib > 1) \
    set libtest_lib="{`echo $libtest_lib | tr -s ' ' ','`}"
  setenv d0$libtest_sect "$libtest_lib"
  if($libtest_print)echo d0$libtest_sect="$libtest_lib"
#
# Fix PATH
#
  if(`echo $path | grep -c $libtest_sect`)then
    set subdir = `echo $PATH | sed 's@^.*'${libtest_sect}'\([^:]*\).*$@\1@g'`
    set libtest_path = `echo ${libtest_lib}${subdir} | tr -s ' ' ':'`
    setenv PATH `echo $PATH | sed 's@[^:]*/'${libtest_sect}'[^:]*@@g'`
    setenv PATH `echo ${PATH}: | sed 's@:::*@:'$libtest_path':@'`
    setenv PATH `echo $PATH | sed 's/\(.*\):/\1/'`
    rehash
    if($libtest_print)echo PATH=$PATH
  endif
end
