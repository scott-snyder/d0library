#! /bin/csh -f
#------------------------------------------------------------------------------
#
# Name      : Setup_batch
#
# Purpose   : Create a series of directories to run Geant in batch
#
# Usage     : 
#
# % setup_batch  [-v] [-e ndir] [-n ndir step] [-r ndir]
#
# Options   :
#
# -e   - Executes user script in existing directories
# -n   - Creates directories and executes user script
# -r   - Removes directories
# -v   - Verbose
#
# Created 5-FEB-1994   Michael J. Wendling
#
#------------------------------------------------------------------------------
#
#  Make sure some things start out false.
#
set do_e = 0
set do_n = 0
set do_r = 0
set do_v = 0
#
set argv = (`getopt e:n:r:v $argv`)
#
if("$argv[1]" == '--') then
    echo "usage: setup_batch  [-v] [-r ndir] [-n ndir step] [-e ndir] "
    exit 0
endif
#
while("$argv[1]" != '--')
  switch($argv[1])
  case -e:
    set do_e = 1
    set ndir = $argv[2]
    repeat 2 shift
    breaksw
  case -v:
    set do_v = 1
    shift
    breaksw
  case -n:
    set do_n = 1
    set ndir = $argv[2]
    shift
    set step = $argv[3]
    shift
    breaksw
  case -r:
    set do_r = 1 
    set old = $argv[2]
    repeat 2 shift
    breaksw
  endsw
end
#
# Must either create new dir's or use old ones, not both.
#
if($do_n && $do_e) then
   /bin/echo 'choose only one of -e and -n'
   exit 0
endif
#
# Choosing only the verbose flag doesn't make much sense
#
if($do_r || $do_n || $do_e) then
else 
   /bin/echo "usage: setup_batch  [-v] [-r ndir] [-n ndir step] [-e ndir] "
   exit 0
endif
#
#
if ($do_n || $do_e) then
   if ($do_v) then
      /bin/echo ''
      /bin/echo '****************************************************************'
      /bin/echo '* This script will create a series of directories beneath the  *'
      /bin/echo '* the current directory and execute a user supplied script in  *'
      /bin/echo '* each directory                                               *'
      /bin/echo '*                                                              *'
      /bin/echo '* The user is required to place a executable script named      *'
      /bin/echo '* "setup_user" in the current directory                        *'
      /bin/echo '*                                                              *'
      /bin/echo '* If Geant events are to be run, and ffread data cards used,   *'
      /bin/echo '* make sure that the user script copies a file named fort.9    *'
      /bin/echo '* into each of directories.                                    *'
      /bin/echo '****************************************************************'
      /bin/echo '' 
   endif
endif
#
# Remove directories from 0 to $old
#
if ($do_r) then
   set curr = 0
   setenv oldd 0
   @ oldd = $old - 1
   #
   while ( $old > $curr )
      if($do_v) /bin/echo "Removing $PWD/$curr "
      rm -rf $PWD/$curr
      @ curr ++	
   end
endif
#
# Setup directories
#
if($do_n || $do_e) then
   #
   # Test for executability of user script
   #
   set userscript = $PWD/setup_user 
   #
   if 	( ! (-x "$userscript")) then
	/bin/echo $userscript is not executable
	exit 0
   endif
   #
   # Since we start at 0 we want to stop at $ndir -1
   #
   if ($ndir == 0) exit 0
   @ stop = $ndir - 1
   #
   # Set the numbers for TRIG and ZBIO
   #
   if ($do_n) then 
	if ($step == 0) exit 0
	set skip = 0
	set trig = $step
   endif
   #
   set dir = 0
   #
   # Loop over all directories
   #
   while 	( $dir <= $stop )
   #
	if ($do_n) then 
	   /bin/rm -r 	 $PWD/$dir >& /dev/null 
	   /bin/mkdir -p $PWD/$dir
	endif
	cd $PWD/$dir
	#
        # Execute the user's script
        #
        $userscript
	#
	# Hack on the ffread file
	#
	if ($do_n) then 
	   if( ! (-r fort.9)) then 
	      /bin/echo "you idiot, there is no fort.9 file"
              exit 0
	   endif
           #
	   cp -f fort.9 ffread
	   /bin/rm -r fort.9 >& /dev/null
	   /usr/bin/touch fort.9
	   cat ffread | grep -iv zbio | grep -iv trig | grep -iv stop >> fort.9
	   /bin/echo 'ZBIO 31 32' $skip >> fort.9
	   /bin/echo 'TRIG' $trig >> fort.9
	   /bin/echo 'STOP' >> fort.9
	   /bin/rm -r ffread >& /dev/null	
	endif
        #
	if($do_v) /bin/echo Setting-up $PWD/$dir
	#
        # Move on to the next directory 
	#
	@ dir ++
	if ($do_n) then 
		@ skip =  $skip + $step
		@ trig =  $trig + $step
	endif 
   end
endif
#
exit 0
