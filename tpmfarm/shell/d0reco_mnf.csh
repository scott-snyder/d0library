#!/bin/csh
#========================================================================
#
# Name      : D0RECO.CSH
#
# Purpose   : Control of the D0RECO job on SGI. This version for a multi 
#             worker node farm.
#
# Arguments : The following strings are assumed environment variables:
#               
#
# Created 30-Nov-1992 Kirill Denisenko
#     - To use as a standalone shell for the farmlet
#     - 1/29/93 Modification for subsystems
#     - 6/18/93 Project independent now 
#========================================================================
#
# Get the list of nodes for a particular host
set NODELIST = `nawk '  /READY/ { print $1 } ' $PRODIR/resources/resource.$FMLNODE`
if ( $#NODELIST == 0 ) then
  echo "No available worker nodes for system $FMLNODE available; exiting..."
  goto theend
endif

#First check that a corresponding project file really exists
if ( ! -e $PRODIR/project/PROJECT.$PROJECT ) then
  echo "No file defining this project $PROJECT, check resource" | /usr/sbin/Mail
 -s "Wrong Project" unix-proman
  exit 4
endif
set PRJFIL = $PRODIR/project/PROJECT.$PROJECT

#Do a libprod here
set  def_libprod = `nawk '/LIBPROD/ { print $2, $3, $4 }' $PRJFIL`
eval libprod $def_libprod

#Get other environmentals from the project file
eval setenv reu  `nawk ' /PRC/ { print $2 }' $PRJFIL`
set def_setreco = `nawk ' /SETUP/ { print $2 }' $PRJFIL`
set def_recox   = `nawk ' /EXEC/ { print $2 }' $PRJFIL`
set def_recorcp = `nawk ' /RCP_Q/ { print $2 }' $PRJFIL`

#Setup all the worker nodes with the same definitions
foreach worker ( $NODELIST[*] )

  cd $PRODAT/$FMLNODE/$worker/run

# Get the correct environment from the project file
  eval ln -sf $def_setreco setreco
  eval ln -sf $def_recox   d0reco.x
  eval ln -sf $def_recorcp d0reco_rcp

# Remote RCP should reside in each run area
  ln -sf remote.rcp remote_rcp

# Setup RECO logical links 
  setreco
 
#End of the loop over the workers
end

#-----------------------------------------------------
#
# Main processing loop
#
#-----------------------------------------------------
#
echo 'MU DORECO.CSH started' `date`
#
#Loop over the worker nodes here
nodeloop:

#Check the disk space here; if not enough - bail out
$PRODIR/exe/check_disk_space
if ( $status != 0 ) then
  sleep 299
  goto nodeloop
endif

foreach worker ( $NODELIST[*] ) 

  cd $PRODAT/$FMLNODE/$worker

#Check the flag "node.busy"; go to the next node if it is set
  if ( -e run/node.busy ) then

#Check if minishell is running on this node
    rsh $worker -n "ps -ef | grep minishell" > tempo.$$ &; set bkgr = $child
    $PRODIR/tpm/check_rsh.csh $0 $bkgr 600 5
    if ( $status == 0 ) then
      set mini = ` nawk ' /run\/minishell/ { print $2 }' tempo.$$ `
      rm tempo.$$
      if ( $mini == "" ) then
#       echo "Please check the worker node $worker on a farmlet $FMLNODE" `date` | /usr/sbin/Mail unix_proman
        $PRODIR/exe/fix_worker_node $worker
      endif
    endif
    goto wrkloop
  endif

  cd proman

#Search for file to process. 
 
  set nonomatch	
  set FILE = `ls -rt inspool/*.done`
  if ( $#FILE >= 1 ) then

#File found in inspool directory, rename it into the raw directory.
#Remove the associated .done file
# 
    setenv INFILE `echo $FILE[1] | nawk ' { gsub(/inspool\//,""); gsub(/\.done/,""); print } ' ` 
    echo $INFILE
    mv inspool/$INFILE raw/$INFILE
    mv inspool/$INFILE.done raw/$INFILE.done
    set QUEID  = ` nawk ' { print $3 } ' raw/$INFILE.done `
    set QUELOC = ` nawk ' { print $2 } ' raw/$INFILE.done `
#
#-----------------------------------------------------
# Setup and run D0RECO
#-----------------------------------------------------
#
#Create filenames; first get the root of the name from the 
#INFILE
    set ROOT = $INFILE
    set ROOT = $ROOT:r
    set EXNM = $INFILE
    set EXNM = $EXNM:e
    set EXND = X_DST01
    set EXNS = X_STA01
    set FLTR = `echo $EXNM | nawk ' {split($0,sa,"_"); print sa[3], sa[4] } ' `
    if ( $#FLTR == 0 ) then
      set FLTR = `echo ALL00 NONEX00`
    endif

#Define the STA name; note that we use Z and Y instead of the RECO version
#and the date; that will be changed after RECO completion
    setenv STAFILE "${ROOT}.${EXNS}${reu}ZZZZ_${FLTR[1]}_${FLTR[2]}_YYYYYYY"
    echo "STA file to process: " $STAFILE
    setenv DSTFILE "${ROOT}.${EXND}${reu}ZZZZ_${FLTR[1]}_${FLTR[2]}_YYYYYYY" 
    echo "DST file to process: " $DSTFILE


# setup links for RECO datasets
    cd $PRODAT/$FMLNODE/$worker/run
    ln -sf $PRODAT/$FMLNODE/$worker/proman/raw/$INFILE      event_data
    ln -sf $PRODAT/$FMLNODE/proman/dst/$DSTFILE             dst_output_data
    ln -sf $PRODAT/$FMLNODE/proman/sta/$STAFILE             sta_output_data
    ln -sf $PRODSK/hstdb/$pdb_dir/${ROOT}.hst4              histograms
    ln -sf $PRODSK/sumdb/$pdb_dir/${ROOT}.out               summary_output
    ln -sf $PRODIR/stsdb/$pdb_dir/${PROJECT}_${QUEID}-${QUELOC}.STATUS pm_status_1

# Generate useful information for log file
    echo "For $INFILE worker $worker processing started:" `date`

# Run RECO - create a minishell and run it
    set WKDIR = $PRODAT/$FMLNODE/$worker/run
    echo 'node.busy' > node.busy
    echo '#\!/bin/csh' > $WKDIR/minishell
    echo "setenv STAFILE $STAFILE"                    >>  $WKDIR/minishell
    echo "setenv DSTFILE $DSTFILE"                    >>  $WKDIR/minishell
    echo "setenv INFILE  $INFILE"                     >>  $WKDIR/minishell
    echo "cd $WKDIR"                                  >>  $WKDIR/minishell
    echo "setenv PRODIR $PRODIR"                      >>  $WKDIR/minishell
    echo "setenv PRODAT $PRODAT/$FMLNODE"             >>  $WKDIR/minishell
    echo "setenv FMLNODE $FMLNODE"                    >>  $WKDIR/minishell
    echo "setenv FMLPHYS $FMLPHYS"                    >>  $WKDIR/minishell
    echo 'setenv RAW_AREA $PRODAT/'"$worker/proman/raw" >>  $WKDIR/minishell
    echo "setenv STA_AREA $STA_AREA"                  >>  $WKDIR/minishell
    echo "setenv DST_AREA $DST_AREA"                  >>  $WKDIR/minishell
    echo "setenv CLI_AREA $PRODIR/clidb/$pdb_dir"     >>  $WKDIR/minishell
    echo "setenv QUEID ${QUEID}-${QUELOC}"            >>  $WKDIR/minishell
    echo "setenv def_libprod ""'$def_libprod'"        >>  $WKDIR/minishell
    cat $PRODIR/tpm/mnfshell                          >>  $WKDIR/minishell
    chmod u+x minishell

#Submit the shell for the execution on the node
    set LOG_AREA = $PRODIR/logdb/`$PRODIR/exe/get_pdb_link`
    rsh $worker -n  \
    "$WKDIR/minishell >& $LOG_AREA/${PROJECT}_${QUEID}-${QUELOC}.log&" &; set bkgr = $child
    $PRODIR/tpm/check_rsh.csh $0 $bkgr 600 10
endif  # end of "done" found

wrkloop:
end  # end of the loop over the nodes
sleep 100
goto nodeloop
#
# Normal exit
theend:

#print a time
echo $QUEID ' DORECO.CSH ended' `date`
exit 0
