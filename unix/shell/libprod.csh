#! /bin/csh -f
#------------------------------------------------------------------------------
#
# Name      : libprod.csh
#
# Purpuse   : Define production release environmant variables.
#
# Arguments : Package name
#
# Created 10-JUN-1992   Herbert B. Greenlee
#
# This script must be sourced to change the calling process's environment.  
# This is normally done by the alias libprod deifned in d0local.cshrc. 
#
# Usage:
#
# libprod [-cnps] [-r <package_root>] <package>
#
# Options:
#
# -c Current.  EV's are set to a search list pointing to top of
#    prod. area, then to subdirectories
# -n New.  EV's are set to a search list pointing to the new subdirectory,
#    then the top of the production area, then to d0library subdirectories.
# -p Print values of environment variables that are set.
# -r Specify root of production area (default $prod/<package>).
# -s Subdirectories.  Env. variables point to subdirectories only.
#
# One one of -c, -n or -s should be specified.  If none is specified, -n is 
# default if a new subdirectory exists.
#
# Created  11-Jun-1992  Herbert Greenlee
#
#------------------------------------------------------------------------------
#
# parse options
#
set argv=(`getopt cnsr:p $argv:q`)
set libprod_root=''         # root of production area
set libprod_print=0         # print mode
set libprod_current = 0     # Current
set libprod_new = 0         # New
set libprod_sub = 0
while("$argv[1]" != '--')
  switch($argv[1])
  case -c:                # print mode
    set libprod_current=1
    shift
    breaksw
  case -n:                # print mode
    set libprod_new=1
    shift
    breaksw
  case -r:                # production root
    set libprod_root=$argv[2]
    repeat 2 shift
    breaksw
  case -p:                # print mode
    set libprod_print=1
    shift
    breaksw
  case -s:                # print mode
    set libprod_sub=1
    shift
    breaksw
  default:
    echo "libprod: bad option: $argv"
    exit 1
  endsw
end
shift     # drop final --
#
# Check flags & supply defaults
#
@ libprod_sum = $libprod_current + $libprod_new + $libprod_sub
if( $libprod_sum > 1 )then
  echo "Libprod: specify only one of -c -n -s."
  exit 1
endif
if( $libprod_sum == 0 )set libprod_new = 1
#
# Get package name
#
if($#argv != 1 )then
  echo "Usage: libprod [-cnps] [-r root] package"
  exit 1
endif
set libprod_package = $argv[1]
#
# Check root of production area
#
if( $libprod_root == '' && ! $?prod )then
  echo "Environment variable prod is undefined."
  exit 1
endif
if( $libprod_root == '' )then
  set libprod_root = $prod/$libprod_package
endif
if( ! -d $libprod_root )then
  echo "Production area $libprod_root does not exist."
  exit 1
endif
#
# If new mode has been specified, and the new subdirectory doesn't exist,
# then drop back to current mode.
#
if( $libprod_new && ! -d $libprod_root/new )then
  set libprod_new = 0
  set libprod_current = 1
endif
#
# Set environment variable for each library in package.  Libraries not in 
# package list point to d0library.
#
@ libprod_first = `grep -n '\* *Library *\*' $libprod_root/${libprod_package}_package.lis | line | cut -d: -f1` + 1
set libprod_sects = `tail +$libprod_first $libprod_root/${libprod_package}_package.lis | tr '[A-Z]' '[a-z]' | grep -v cernlib`
foreach libprod_sect ( `echo $libprod_sects` )
  if( $libprod_sect == cernlib )continue
  if( $libprod_sect == tpmfarm )continue
  if( $libprod_sect == unix )continue
  if( $libprod_sect == util )continue
  if( $libprod_current )then
    set libprod_val = $libprod_root
  else if( $libprod_new )then
    set libprod_val = ( $libprod_root/new $libprod_root )
  else
    set libprod_val = ''
  endif
  if( -d $libprod_root/$libprod_sect )set libprod_val = ( $libprod_val $libprod_root/$libprod_sect )
  set libprod_val = "{"`echo $libprod_val | tr ' ' ','`"}"
  setenv d0$libprod_sect "$libprod_val"
  if($libprod_print)echo d0$libprod_sect="$libprod_val"
end
#
# Second level subdirectories handled here as special cases.
#
if( $libprod_current )then
  setenv d0calor_off__data "{$libprod_root,$libprod_root/calor_off/data}"
  if( $libprod_print )echo d0calor_off__data="$d0calor_off__data"
  setenv d0vtx_util__source "{$libprod_root,$libprod_root/vtx_util/source}"
  if( $libprod_print )echo d0vtx_util__source="$d0vtx_util__source"
  setenv d0d0geant__data "{$libprod_root/new,$libprod_root,$libprod_root/d0geant/data}"
  if( $libprod_print )echo d0d0geant__data="$d0d0geant__data"
endif
if( $libprod_new )then
  setenv d0calor_off__data "{$libprod_root/new,$libprod_root,$libprod_root/calor_off/data}"
  if( $libprod_print )echo d0calor_off__data="$d0calor_off__data"
  setenv d0vtx_util__source "{$libprod_root/new,$libprod_root,$libprod_root/vtx_util/source}"
  if( $libprod_print )echo d0vtx_util__source="$d0vtx_util__source"
  setenv d0d0geant__data "{$libprod_root/new,$libprod_root,$libprod_root/d0geant/data}"
  if( $libprod_print )echo d0d0geant__data="$d0d0geant__data"
endif
#
# Set top level environment variable.
#
if( $libprod_current )then
  set libprod_val = $libprod_root
else if( $libprod_new )then
  set libprod_val = "{$libprod_root/new,$libprod_root}"
else
  set libprod_val = $libprod_root
endif
setenv prod$libprod_package "$libprod_val"
if($libprod_print)echo prod$libprod_package="$libprod_val"
