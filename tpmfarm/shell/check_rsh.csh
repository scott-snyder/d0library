#!/bin/csh -f
#Test whether a process PID = $2 is running for
#more than $3 seconds with increments of $4 seconds
#$1 is a name of the calling script for printing
#Kirill Denisenko, 02/15/93
#Last Modified:

set IW = 0
waitrsh:
sleep $4
ps -p $2 >& /dev/null
if ( $status == 0 ) then
  @ IW = ( $IW + $4 )
  if ( $IW <= $3 ) then
    goto waitrsh
  else
    kill -9 $2
    echo "Rsh got stuck in $1; killed" | /usr/sbin/Mail unix_proman
    exit 1
  endif
endif

#This is all
exit 0
