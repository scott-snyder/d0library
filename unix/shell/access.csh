#! /bin/csh -f
#------------------------------------------------------------------------------
#
# Name      : access.csh
#
# Purpose   : Search the project/temp disk databases for a given project.
#
# Usage     :
#
# % access.csh project [cluster]
#
# Options   :
#
# Arguments : project - The name of the project/temp disk (eg top_5)
#             cluster - 1,2,3 or 4 to choose the hundreds digit of the
#                  prj$root number for duplicate defnitions.
#
# Returns :   0 - success
#            >0 - error
#
# Created 15-AUG-1995   John D Hobbs
#
#------------------------------------------------------------------------------

set database = ($PRJ_DIR/access.prj_disks)
if( ! -r $database ) then
  echo "Could not find $database.  Exit"
  exit 1
endif

set prj_path = (`egrep "^${1}	" $database[1-] | cut -f2 -d'	'`)

# Check for multiple occurances and the user doesn't seem to know it

if ( $#prj_path > 1 && $#argv != 2 ) then
  echo "Duplicate entries for ${1}: $prj_path[1-]"
  echo " -> Use 'access $1 n' where n is the first digit on the prj_root number"
  exit 2
endif

# Check for no match

if( $#prj_path == 0 ) then
  echo Could not find a match for $1
  exit 3
endif

# Unique match

if ( $#prj_path == 1 ) then
  setenv $1 $prj_path
  echo ${1}: $prj_path
  goto done_all
endif

# Multiple matches, user breaks degeneracy.

@ n = 1
degeneracy:
  if ( $n > $#prj_path ) goto done_all
  set pathname = `echo $prj_path[$n] | egrep $2`
  if ( $pathname != "" ) then
    setenv $1 $prj_path[$n] 
    echo ${1}: $prj_path[$n]
    goto done_all
  endif
  @ n += 1
goto degeneracy

done_all:
exit 0
