#!/bin/csh -f
#A script to create a tape_moved.WMXXXX file
#Kirill Denisenko, 02/17/93
#Last Modified:
#First argument - VSN of the tape
#Second argument - logical node name

#If such file already exists - bail out
if ( -e $PRODIR/history/tapes_moved.$1 ) then
  goto outofhere
endif

#Do a directory in a reverse chrono
set ALLITAP = `ls -t $PRODIR/history/itapes_moved.*`

#Do grep on the itapes_moved.* files
foreach filmov (  $ALLITAP[*] )
  grep $1 $filmov | nawk ' { printf "%s%5d %s\n", $1, $2, $3 } ' > $PRODIR/history/tapes_moved.$1
  if ( $status == 0 ) then
    goto outofhere
  endif
end

outofhere:
#Store the time of the last update
echo `date '+%j%H'` > $PRODIR/history/date_tapes_moved.$1
echo $2             > $PRODIR/history/node_tapes_moved.$1

#Sort the tapes_moved.VSN file
sort +1 -2 $PRODIR/history/tapes_moved.$1 > $PRODIR/history/tmp.$$
mv $PRODIR/history/tmp.$$ $PRODIR/history/tapes_moved.$1

#That is all
exit
