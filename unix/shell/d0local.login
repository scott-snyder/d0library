#------------------------------------------------------------------------------
#
# Name      : d0local.login
#
# Purpuse   : Standard d0 .login
#
# Arguments : None
#
# Created 25-JUN-1991   Herbert B. Greenlee
# Updated 16-MAY-1994   John Drinkard - Put in some SUN dependence
#
# This file sets standard d0 environment variables.  It should be sourced
# from the user's .login.
#
#------------------------------------------------------------------------------
#
# Set environment variables
#
setenv LIB_ROOT
setenv INCROOT		$LIB_ROOT/d0library
#
# Do not redefine d0library if it has already been defined
#
if( ! $?d0library )then
  setenv d0library	/d0library
endif
if( ! $?PRJ_DIR )then
  setenv PRJ_DIR	/usr/local/etc
endif
setenv LIB_ROOT		`echo $d0library | sed -n 's@^\(.*\)/.*$@\1@p'`
setenv INCROOT		$d0library
setenv d0root		$d0library
setenv d0test		$d0library/test
setenv d0gamma		$d0library/gamma
setenv d0beta		$d0library/beta
set sys = `uname -s`
if( $sys == AIX )then
  setenv D0FLAVOR IBMAIX
else if( $sys == ULTRIX )then
  setenv D0FLAVOR ULTRIX
else if( $sys == SunOS )then
  setenv D0FLAVOR SUNOS
else if( $sys == OSF1 )then
  setenv D0FLAVOR ALFOSF
else if( $sys == Linux )then
  setenv D0FLAVOR LINUX
else
  setenv D0FLAVOR SIUNIX
endif
#
# Remove D0 directories from PATH (in case this script has already been
# executed).
#
setenv PATH `echo $PATH | sed 's@:'$d0library'[^:]*@@g'`
#
# Add D0 directories to PATH
#
setenv PATH "${PATH}:$d0library/program_builder:$d0library/unix:$d0library/util:$d0library/xframe:$d0library/dspack/bin"
#
# Edit any non-existent directories out of PATH
#
foreach dir ( $path )
  if(! -d $dir )setenv PATH `echo $PATH | sed 's@:'$dir'[^:]*@@g'`
end
rehash
#
# Set library section EV's
#
foreach sect (`all_sect`)
    setenv d0$sect	$d0library/$sect
end
#
# Set ZEB search list
#
if($?d0zeb)then
  setenv d0zeblst "$d0zeb/*"
endif
