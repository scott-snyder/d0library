#!/bin/csh
#A cron-activated script to check that the submitted tape has been successfully
#processed
#Kirill Denisenko, 02/17/93
#Last Modified:
# 11/30/93 Kirill Denisenko - Check whether the tape is already WAITING

#Check that there are no other copies of it running
set running = `ps -ef | grep jobcontrol.csh | grep -v 'sh -c' | nawk ' ( $0 !~ "grep" && $0 !~ "'$$'" ){ print }' `
if ( $#running != 0 ) then
  echo "jobcontrol.csh is already running, exiting..." | /usr/sbin/Mail -
s "Many Jobcontrols" unix_proman
  exit -5
endif
unset running

#Temp file to hold the lines that don't match
set TMP = $PRODIR/resources/tmp/jcntrl.$$

#File to hold the combined set of all DSNs on selected tapes
set COMBO    = $PRODIR/resources/tmp/jcntrl_combo.$$
set VSLIST   = $PRODIR/resources/tmp/jcntrl_vslist.$$
set JOBLIST  = $PRODIR/resources/tmp/jcntrl_jblist.$$
set DSTFOUND = $PRODIR/resources/tmp/jcntrl_dstfound.$$
set STAFOUND = $PRODIR/resources/tmp/jcntrl_stafound.$$

#Set the current year
set YEAR = `date '+%Y'`

#First get today's date
set todate = `date '+%j%H'`

#Get all the tapes_moved.* dates
set curdir = $cwd
cd $PRODIR/history
set tapmov = `ls tapes_moved.*`

#Foreach file compare the date; if bigger than 24 hours
#we run a check on it
set tapes_to_check
foreach file ( $tapmov[*] )
  set datefl = "date_$file"
  if ( -e $datefl ) then
    set lsdate = `cat $datefl`
  else
    set lsdate = $todate
  endif

  @ tmelps = ( $todate - $lsdate )
  if ( $tmelps >= 200 ) then

#Check whether the tape is already WAITING
    set VSN = $file:e
    if ( $?WT == 1 ) then
      unset WT
    endif
    set WT = `nawk ' $1 ~ "'$VSN'" { if ( $3 == "WAITING" ) { print 1; exit } } ' $PRODIR/resources/injobs`
    if ( $WT == 1 ) then
      goto next_check
    endif 
    
#Create a file with all VSNs
    echo $VSN >> $VSLIST

#Save all these tapes in a new variable
    set tapes_to_check = ( $tapes_to_check $file )
    cat $file | nawk ' { split ($3, a, "\."); print a[1] }' >> $COMBO   

  endif
next_check:
end

#Grep on tpm_disp to get all lines with VSNs to check
$PRODIR/report/tpm_disp 5000 | fgrep -f $VSLIST > $JOBLIST

#Grep on dstf.dir and staf.dir
split -350 $COMBO $PRODIR/resources/tmp/cmbspl.$$
rm $COMBO
set COMBOS = `/bin/ls $PRODIR/resources/tmp/cmbspl.{$$}*`
foreach subcombo ( $COMBOS )
  fgrep -f $subcombo $PRODIR/pdbkd/$YEAR/dstf.dir >> $DSTFOUND
  if ( $status != 0 && $status != 1 ) then
    echo "Jobcontrol failed on fgrep; please check" | \
    /usr/sbin/Mail -s "Jobcontrol failed" unix_proman
    goto finish
  endif
  fgrep -f $subcombo $PRODIR/pdbkd/$YEAR/staf.dir >> $STAFOUND
  if ( $status != 0 && $status != 1 ) then
    echo "Jobcontrol failed on fgrep; please check" | \
    /usr/sbin/Mail -s "Jobcontrol failed" unix_proman
    goto finish
  endif
end
rm $COMBOS

#Loop over tapes that are old enough and not currently WAITING
foreach file ( $tapes_to_check[*] )

#Set the VSN
    set VSN = $file:e

#Unset the variables if set
    if ( $?MISSET == 1 ) then
      unset MISSET
    endif
    if ( $?statrcp == 1 ) then
      unset statrcp
    endif

#Tape has been submitted more than 24 hours ago
#If a file node_tapes_moved.VSN exists - get it
    set nodefl = "node_$file"
    if ( -e $nodefl ) then
#Get the real node name ( in case of a multisegment node )
      set FMLNODE = `cat $nodefl`
#Get the name of the output tape  
      set outtfl = $PRODIR/resources/outtape.$FMLNODE
      if ( -e $outtfl ) then
        set outvsn  = `cat $outtfl`
        set environ = `cat $PRODIR/resources/env.$FMLNODE`
        set statrcp = $PRODIR/pdbkd/$environ/STAT_${outvsn}.RCP
      endif
    endif

#First get the datasets supposed to be ready
    set DSN = `cat $file | nawk ' { print $3 }'`    
    if ( $#DSN == 0 ) then
      goto nexttape
    endif
    foreach dataset ( $DSN[*] )
#First check against the dstf.dir
      set ptrn = $dataset:r
      set tmst = `cat $JOBLIST | grep $VSN | nawk -f $PRODIR/exe/tmst.nawk `
      set tmst = `echo $YEAR $tmst | nawk ' { printf("%01d%04d",substr($1,4),$2) } ' `
      set match1 = `nawk ' BEGIN { pt = 0 }; $0 ~ "'$ptrn'" { inx = index($0, "\.RCP"); ts = substr($0,inx-7,5); if ( ts >= '$tmst') { pt = 1; exit } }; END { print pt } ' $DSTFOUND`
      set match2 = `nawk ' BEGIN { pt = 0 }; $0 ~ "'$ptrn'" { inx = index($0, "\.RCP"); ts = substr($0,inx-7,5); if ( ts >= '$tmst') { pt = 1; exit } }; END { print pt } ' $STAFOUND`
      @ match = ( $match1 + $match2 )
      if ( $match != 2 ) then
#Check against crashes and zerolength
        set match1 = `nawk ' BEGIN { pt = 0 }; $0 ~ "'$ptrn'" { pt = 1; exit }; END { print pt } ' $PRODIR/history/crashes`
        set match2 = `nawk ' BEGIN { pt = 0 }; $0 ~ "'$ptrn'" { pt = 1; exit }; END { print pt } ' $PRODIR/history/zerolength`
        @ match = ( $match1 + $match2 )
        if ( $match == 0 ) then
#Check whether it is still on the tape in the drive
          if ( $?statrcp == 1 ) then
            set match = `nawk ' BEGIN { pt = 0 }; $0 ~ "'$ptrn'" { pt = 1; exit }; END { print pt } ' $statrcp`
            if ( $match == 1 ) then
              if ( -e $TMP ) then
                rm $TMP
              endif
              goto nexttape
            endif
          endif
          echo $dataset >> $TMP
        endif
      endif
    end

#Check the length of $TMP
    if ( -e $TMP ) then
      set MISSET = `cat $TMP`
      rm $TMP
    endif
    if ( $?MISSET == 0 ) then
#Remove everything
      rm $file
      rm $datefl
      rm $nodefl
    else
      foreach runpar ( $MISSET[*] )
        nawk ' $0 ~ "'$runpar'" { print }' $file >> $TMP
      end
      mv $TMP $file
      echo $todate > $datefl
      nawk '  BEGIN { p = 0 }; $1 ~ "'$VSN'" { if ( $3 == "SUBMITTED" ) { gsub(/SUBMITTED/, "WAITING"); $4++; res = $0; p = 1 } else { p = 0; exit } }; END { if(p) print res } ' $PRODIR/resources/injobs >> $PRODIR/history/newinjobs.tmp
#     echo "Tape $VSN was found to have $#MISSET missing datasets; resubmitted on `date` " | /usr/sbin/Mail -s "Job Resubmitted" unix_proman
    endif
  
  endif
nexttape:
end
     
#Rename the file newinjobs.tmp to injobs.repro
if ( -e $PRODIR/history/newinjobs.tmp ) then
  mv $PRODIR/history/newinjobs.tmp $PRODIR/history/injobs.repro
endif

#That is all
finish:
if ( -e $TMP ) then
  rm $TMP
endif
if ( -e $COMBO ) then
  rm $COMBO
endif
if ( -e $VSLIST) then
  rm $VSLIST
endif
if ( -e $JOBLIST) then
  rm $JOBLIST
endif
if ( -e $DSTFOUND ) then
  rm $DSTFOUND
endif
if ( -e $STAFOUND ) then
  rm $STAFOUND
endif
cd $curdir
exit

