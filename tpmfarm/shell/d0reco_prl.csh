#!/bin/csh
#========================================================================
#
# Name      : D0RECO.CSH
#
# Purpose   : Control of the D0RECO job on SGI. This version for a  
#             parallel processing farm.
#
# Arguments : The following strings are assumed environment variables:
#               
#
# Created 27-Mar-1993 Kirill Denisenko
#     - To use as a standalone shell for the farmlet
#========================================================================

#Setup the executables names
cd $PRODAT/$FMLNODE
ln -sf $PRODAT/$FMLNODE/project/remote.rcp remote_rcp
ln -sf $PRODIR/exe/insrv_d0.x  insrv_d0_$FMLNODE
ln -sf $PRODIR/exe/outsrv_d0.x outsrv_d0_$FMLNODE

#-----------------------------------------------------
#
# Main processing loop
#
#-----------------------------------------------------
#
echo 'PARALLEL DORECO.CSH started' `date`
#
#Set the worker area initially to be -100 ( to start from 1 )
setenv WORKING_AREA "-100"
setenv QUEID  "-100"
setenv RUNNO  "-100"
setenv INLIST "-100"
set PIDFILE = project/pid_check.$$
set project = NONE


#Check that inspool and raw directories are in their proper place
if ( ! ( -d inspool ) ) then
  mkdir inspool
endif
if ( ! ( -d raw ) ) then
  mkdir raw
endif
if ( ! ( -d project ) ) then
  mkdir project
endif
rm project/current_area
foreach area ( 1 2 3 )
  if ( ! ( -d project/$area ) ) then
    mkdir project/$area
  endif
  rm project/$area/*
end
if ( ! ( -d proman ) ) then
  mkdir proman
endif
if ( ! ( -d proman/sta ) ) then
  mkdir proman/sta
endif
if ( ! ( -d proman/dst ) ) then
  mkdir proman/dst
endif

#Kill the stale servers for this machine
set spids = `ps -ef | grep srv_d0_${FMLNODE} | nawk ' ! /grep/ { print $2 } '`
foreach spid ( $spids[*] )
  kill -9 $spid
end

#For the cold start - clean up the areas
set R_STA = `ls proman/sta/*.sta`
foreach file ( $R_STA[*] )
  if ( ! ( -e $file:r.ous ) ) then
    rm $file
    set dstfile = `echo {$file:r}.dst | sed 's/sta/dst/' `
    rm $dstfile
  endif
end
unset R_STA

set R_RAW = `ls raw/*.done`
foreach file ( $R_RAW[*] )
  if ( $#R_RAW > 0 ) then
    set infile = $file:r
    set infile = $infile:t
    set iosf = `grep -l $infile *.ios`
    if ( $#iosf > 0 ) then
      set ousf = `echo $iosf | sed 's/ios/ous/'`
      if ( -e proman/sta/$ousf ) then
        goto next_raw
      else
        rm $iosf
      endif
    endif
    mv raw/$infile inspool/$infile
    mv raw/$infile.done inspool/$infile.done
next_raw:
    unset iosf
    unset ousf
  endif
end
unset R_RAW

#Clean the project areas

#Kill the stale processes existing on the nodes
$PRODAT/$FMLNODE/scr1 scrnw

#If a new invocation - let the old sta be spooled out
if ( $WORKING_AREA == "-100" ) then
  goto cleanup
endif

workloop:

#Check for the availability of the spooling space
$PRODIR/exe/check_disk_space
if ( $status != 0 ) then
  sleep 300
  goto cleanup
endif

#Get all the files; look at those without *.done; remove if older than 10 min
set FILEALL = `ls inspool`
if ( $#FILEALL != 0 ) then

#Check if there is a corresponding done file; disrgard if it is .done
  foreach file ( $FILEALL[*] )
    if ( $file:e != "done" && $file != "end" && $file != "error" ) then
#Check if there is no .done
      if ( ! ( -e inspool/${file}.done ) ) then
#Check the age of this file in seconds; delete if older than 10 min
        if ( `$PRODIR/exe/secnd.x inspool/$file` > 10 ) then
          rm inspool/$file
          if ( $status == 0 ) then
            echo "File $file removed; older than 10 min and no .done"
          else
            echo "File $file was not removed; check it"
          endif
        endif
      endif
    endif
  end

endif

#Search for file to process. 
set FILE = `ls -rt inspool/*.done`
if ( $#FILE >= 1 ) then

  setenv RAWFILE `echo $FILE[1] | nawk ' { gsub(/inspool\//,""); gsub(/\.done/,""); print } ' `
  echo "Next raw file to process: $RAWFILE"

#File found in inspool directory, rename it into the raw directory.
  mv inspool/$RAWFILE raw/$RAWFILE
  mv inspool/$RAWFILE.done raw/$RAWFILE.done

#Check the size of it
  set SIZF = `ls -s raw/$RAWFILE`
  if ( $SIZF[1] == 0 ) then
    echo $PRODAT/$FMLNODE/raw/$RAWFILE > $PRODAT/$FMLNODE/empty.ios
    cp $PRODIR/tpm/empty.ous $PRODAT/$FMLNODE/proman/sta/.
    touch $PRODAT/$FMLNODE/proman/sta/empty.sta
    touch $PRODAT/$FMLNODE/proman/dst/empty.dst
    goto cleanup
  endif

#Find a new run number
  set RUNNEW = `echo $RAWFILE | nawk ' { dot = index($1,"\."); print substr($1, dot-9,6) } '` 

#Find QUEID and QUELOC
  set QUEIDN = ` nawk ' { print $3 } ' raw/$RAWFILE.done `
  set QUELOC = ` nawk ' { print $2 } ' raw/$RAWFILE.done `

#Check that RUNNO is the same; if not - stop the previous
#servers and start a new pair
  if (  $RUNNEW != "$RUNNO"  ) then

    setenv RUNNO $RUNNEW
    setenv QUEID $QUEIDN

#Terminate previous servers
    if ( $WORKING_AREA != "-100" ) then
      echo 'endoflist' > $INENDL
    endif

#Before switching the WORKING_AREA make sure that the last file
#from the previous run or tape had his chance to be processed
    if ( $INLIST != "-100" ) then
waitarea:
      if ( -e $INLIST ) then
        setenv inpid_old `cat project/$WORKING_AREA/pid_in`
        ps -p $inpid_old > $PIDFILE
        set INHERE = `nawk ' /'$inpid_old'/ { print } ' $PIDFILE `
        if ( $#INHERE == 0 ) then
          goto switcher
        endif
        sleep 30
        goto waitarea
      endif
      sleep 401  # give 401 seconds for everybody to reconnect
    endif

switcher:
#Switch the area
    switch ( $WORKING_AREA )
    case -100:
      setenv WORKING_AREA 1
      setenv INPORT INPUT_PORT_A
      setenv OUTPORT OUTPUT_PORT_A
    breaksw
    case 1:
      setenv WORKING_AREA 2
      setenv INPORT INPUT_PORT_B
      setenv OUTPORT OUTPUT_PORT_B
    breaksw
    case 2:
      setenv WORKING_AREA 3
      setenv INPORT INPUT_PORT_C
      setenv OUTPORT OUTPUT_PORT_C
    breaksw
    case 3:
      setenv WORKING_AREA 1
      setenv INPORT INPUT_PORT_A
      setenv OUTPORT OUTPUT_PORT_A
    breaksw
    default:
      echo 'Improper setting for $WORKING_AREA'
      goto workloop
    endsw

#Check that spoolers are not running already off this area
    if ( -e project/current_area ) then
      set WRKOLD = `cat project/current_area`
      if ( $WRKOLD == "$WORKING_AREA" ) then
        if ( -e project/$WRKOLD/pid_in ) then
          setenv inpid_old `cat project/$WRKOLD/pid_in`
          ps -p $inpid_old > $PIDFILE 
          set INHERE = `nawk ' /'$inpid_old'/ { print } ' $PIDFILE `
          if ( $#INHERE != 0 ) then
            sleep 60
            goto switcher
          endif
        endif
        if ( -e project/$WRKOLD/pid_out ) then
          setenv oupid_old `cat project/$WRKOLD/pid_out`
          ps -p $oupid_old > $PIDFILE 
          set OUHERE = `nawk ' /'$oupid_old'/ { print } ' $PIDFILE `
          if ( $#OUHERE != 0 ) then
            sleep 60
            goto switcher
          endif
        endif
        if ( -e $PIDFILE ) then
          rm $PIDFILE
        endif
      endif
      rm project/current_area
    endif
      
#Clean the working area
    rm project/$WORKING_AREA/*

    setenv INLIST $PRODAT/$FMLNODE/project/$WORKING_AREA/next_event
    setenv INENDL $PRODAT/$FMLNODE/project/$WORKING_AREA/endoflist
    setenv CURPRJ $PRODAT/$FMLNODE/project/$WORKING_AREA/curprj

#Set cli_area and log_area
    setenv CLI_AREA  $PRODIR/clidb/`$PRODIR/exe/get_pdb_link`

#Set env for the file with pid_in; this file will be removed by
#the insrv when done
    setenv PIDIN $PRODAT/$FMLNODE/project/$WORKING_AREA/pid_in

#Define the PROJECT environment variable
    setenv PROJECT `nawk ' { print $5 } ' $PRODAT/$FMLNODE/raw/$RAWFILE.done`

#Start new servers
    insrv_d0_$FMLNODE &;  set inpid  = $child
    outsrv_d0_$FMLNODE &; set outpid = $child

#Output the next working area for the clients
    echo $WORKING_AREA > $PRODAT/$FMLNODE/project/current_area

#Output the in and out ports for clients
    echo $INPORT  > $PRODAT/$FMLNODE/project/$WORKING_AREA/port_in
    echo $OUTPORT > $PRODAT/$FMLNODE/project/$WORKING_AREA/port_out
    echo $inpid   > $PIDIN
    echo $outpid  > $PRODAT/$FMLNODE/project/$WORKING_AREA/pid_out

#Output the QUEID and RUNNO for clients
    echo $QUEID > $PRODAT/$FMLNODE/project/$WORKING_AREA/queid
    echo $RUNNO > $PRODAT/$FMLNODE/project/$WORKING_AREA/runno

  endif

next:
#Check that the input and output guys are running; send a mail if not
  set RPIDIN = `ps -p $inpid | nawk ' /'$inpid'/ { print 1 } ' `
  set RPIDOT = `ps -p $outpid | nawk ' /'$outpid'/ { print 1 } ' `
  if ( ! ( $RPIDIN == "1" && $RPIDOT == "1" ) ) then
    echo "Parallel servers on $FMLNODE will be restarted" | /usr/sbin/Mail -s 'P-server(s)' unix_proman
    kill -9 $inpid
    kill -9 $outpid

#Also check that no other parallel servers are running; kill them, too
    set spids = `ps -ef | grep srv_d0_${FMLNODE} | nawk ' ! /grep/ { print $2 } '`
    foreach spid ( $spids[*] )
      kill -9 $spid
    end

    exit 1
  endif
      
#Check for the disappearance of $INLIST file
  if ( ! ( -e $INLIST ) ) then
    echo $PROJECT > $CURPRJ
    echo $PRODAT/$FMLNODE/raw/$RAWFILE > $INLIST
  else
    sleep 10
    goto next
  endif

#End of if ( next inspool file )
endif

#Do a cleanup of the working areas
cleanup:
set OUSFILE = `ls $STA_AREA/*.ous`
foreach ousfile ( $OUSFILE[*] )
 
  set origfile = $ousfile:r
  set origfile = $origfile:t
  set inputfil = `cat $origfile.ios`

#Get a project name for this particular file from its .done
  if ( ! -e $inputfil.done ) then 
    rm $ousfile
    rm $origfile.ios
    rm $STA_AREA/$origfile
    if ( -e $inputfil ) then
      rm $inputfil
    endif
    echo "No files in raw corresponding to $inputfil:t" | \
    /usr/sbin/Mail -s "No files in raw" unix_proman
    goto next_ous
  endif
  set nproject = `nawk ' { print $5 }' $inputfil.done`
  echo "New project = $nproject"
  if ( $nproject != $project ) then
    set project = $nproject
    set PRJFIL  = $PRODIR/project/PROJECT.$project
    if ( ! -e $PRJFIL ) then
      echo "No file defining this project $project, check resource" | \
      /usr/sbin/Mail -s "Wrong Project" unix_proman
      echo "No PROJECT file for this $project, exiting..."
      exit 4
    endif

    eval setenv VRSN `nawk ' /VRSN/ { print $2 } ' $PRJFIL`
    eval setenv PASS `nawk ' /PASS/ { print $2 } ' $PRJFIL`
    eval setenv reu  `nawk ' /PRC/  { print $2 } ' $PRJFIL`

  endif

#Create filenames; first get the root of the name from the inputfile
  setenv INFILE $inputfil:t
  set ROOT   = $INFILE
  set ROOT   = $ROOT:r
  set EXNM   = $inputfil:e
  set EXND = X_DST01
  set EXNS = X_STA01
  set FLTR = `echo $EXNM | nawk ' {split($0,sa,"_"); print sa[3], sa[4] } ' `
  if ( $#FLTR == 0 ) then
    set FLTR = `echo ALL00 NONEX00`
  endif

#Create a time stamp
  set tmstmp = ` date '+%y%m%d%H' | nawk ' { print substr($0,2) }' `

#Define the STA name
  setenv STAFILE ${ROOT}.${EXNS}${reu}${VRSN}${PASS}_${FLTR[1]}_${FLTR[2]}_${tmstmp}
  echo "STA file processed: $STAFILE"
  mv $STA_AREA/$origfile.sta $STA_AREA/$STAFILE
  setenv DSTFILE ${ROOT}.${EXND}${reu}${VRSN}${PASS}_${FLTR[1]}_${FLTR[2]}_${tmstmp} 
  echo "DST file processed: $DSTFILE"
  mv $DST_AREA/$origfile.dst $DST_AREA/$DSTFILE

#Test that STA has a correct ZEBRA structure
  $PRODIR/exe/fzton.x $STA_AREA/$STAFILE > /dev/null
  if ( $status != 0 ) then
    mv $inputfil inspool/$inputfil:t
    mv $inputfil.done inspool/$inputfil:t.done
    mv $STA_AREA/$STAFILE $STA_AREA/${STAFILE}.problem
    rm $DST_AREA/$DSTFILE
    echo "Wrong Zebra in $STAFILE" | /usr/sbin/Mail -s "Bad Zebra Construct" denisenk@fnalv
    goto garbage
  endif

#Define the environment file for this node
#Test that DST has a correct ZEBRA structure
  $PRODIR/exe/fzton.x $DST_AREA/$DSTFILE > /dev/null
  if ( $status != 0 ) then
    mv $inputfil inspool/$inputfil:t
    mv $inputfil.done inspool/$inputfil:t.done
    rm $STA_AREA/$STAFILE
    mv $DST_AREA/$DSTFILE $DST_AREA/${DSTFILE}.problem
    echo "Wrong Zebra in $DSTFILE" | /usr/sbin/Mail -s "Bad Zebra Construct" denisenk@fnalv
    goto garbage
  endif

#Define the environment file for this node
  set ENVRM  = ${PRODIR}/resources/env.$FMLNODE

#Before trying to create RCP files define the PDB_AREA
  set pdb_dir = `cat $ENVRM`
  setenv PDB_AREA $PRODIR/pdbkd/$pdb_dir

#Produce the RCP-file
  setenv pm_status_1 $ousfile
  $PRODIR/exe/rcp_prl_write

  if ( $status == 0 ) then
    mv $inputfil.done $STA_AREA/$STAFILE.done 
    rm $inputfil
  else
    mv $inputfil inspool/$inputfil:t
    mv $inputfil.done inspool/$inputfil:t.done
    rm $STA_AREA/$STAFILE
    rm $DST_AREA/$DSTFILE
  endif

garbage:
#Remove the garbage
  rm $ousfile
  rm $origfile.ios

next_ous:

end  # end-of-loop over *.ous files

sleep 15
goto workloop
