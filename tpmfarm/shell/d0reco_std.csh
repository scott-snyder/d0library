#!/bin/csh
#========================================================================
# 06/18/93 Kirill Denisenko A Generic Standalone Shell
#========================================================================
#
# Project independent setups
ln -sf $PRODIR/exe/inspool_l.x  inspool
ln -sf $PRODIR/exe/outspool_mu.x outspool
ln -sf $PRODIR/exe/dst_rcp_write.csh dst_rcp_write
ln -sf $PRODIR/exe/check_invol.csh check_invol
ln -sf $PRODIR/exe/copy_results.csh copy_results

#Get the corresponding project
#First check that a corresponding project file really exists
if ( ! -e $PRODIR/project/PROJECT.$PROJECT ) then
  echo "No file defining this project $PROJECT, check resource" | /usr/sbin/Mail
 -s "Wrong Project" unix-proman
  exit 4
endif
set PRJFIL = $PRODIR/project/PROJECT.$PROJECT

#Now scan the project file for keywords and do appropriate setups
eval libprod `nawk '/LIBPROD/  { print $2, $3, $4 }' $PRJFIL`
eval ln -sf `nawk ' /SETUP/    { print $2 }' $PRJFIL` setreco
eval ln -sf `nawk ' /EXEC/     { print $2 }' $PRJFIL` d0reco.x
eval ln -sf `nawk ' /RCP_S/    { print $2 }' $PRJFIL` d0reco_rcp
eval setenv reu  `nawk ' /PRC/ { print $2 }' $PRJFIL`
 
#Input and output drive setup and allocation
setup  cps
setenv TAPE_MGR cps_tape
setenv TAPE_LABELS VMS
set NODE = `/usr/local/bin/funame`
setenv FMLNODE $NODE
set TMP = $PRODIR/resources/reco_tmp_"$NODE"."$$"
set HST = $PRODIR/history/jobs.log
set UPD = $PRODIR/history/update_"$NODE"."$$"
 
# Clean the areas of the leftover files
if ( -e $INSPOOL/end ) then
  rm $INSPOOL/end
endif
if ( -e $INSPOOL/error ) then
  rm $INSPOOL/error
endif
if ( -e $STA_AREA/end ) then
  rm $STA_AREA/end
endif
if ( -e $STA_AREA/error ) then
  rm $STA_AREA/error
endif
 
#Check whether the input tape is available
set fmentry = `check_invol $INLABEL`
set dtm = `date`
set owd = ` echo $dtm | sed 's/ /_/g' `
if ( $fmentry == -1 ) then
  nawk ' $1 ~ "'$QUEID'" { $3 = "'$$'"; $7 = "INTAPE_IN_USE"; dat = "'$owd'"; gsub(/_/," ",dat); print $0, dat } '  \
  $HST > $UPD
  goto theend
endif
#-----------------------------------------------------
#
# Start the in and out spooler in the background
#
#-----------------------------------------------------
setenv INSPLST $PRODIR/resources/inspool_list_1.$NODE
inspool -inl $INLABEL -queid $QUEID -fmq $fmentry >& ${LOG_AREA}/inspool_${QUEID}.log  &
outspool -inl $OUTLABEL -path $STA_AREA >& ${LOG_AREA}/outspool_${QUEID}.log &
 
#Wait for the spoolers to start
sleep 120
#Check that the spoolers are there
ps -ef | grep dzero > $TMP
set insp_pid = `nawk '/inspool/ { print $2 }' $TMP`
set outs_pid = `nawk '/outspool/ { print $2 }' $TMP`
if ( ! ( $insp_pid == "" || $outs_pid == "" ) ) then
#Spoolers are running; mark job as running
  nawk '$1 ~ "'$QUEID'" { $3 = "'$$'"; $7 = "RUNNING"; print }'  \
  $HST > $UPD

#Create an outtape file for this node
  echo $OUTLABEL > $PRODIR/resources/outtape.$NODE

else
  goto error
endif

#Set the comp status to 0
set comp_status = 0
  
#-----------------------------------------------------
#
# Main processing loop
#
#-----------------------------------------------------
#
echo $QUEID ' DORECO.CSH started' `date`
#
# setup RECO rcp-links
  setreco
  setenv dbl3 $DBL3
#
# Search for file to process. If the file "end" appears, the job
# has ended successful. If the file "error" shows up, the spooler
# has failed.
#
set FILE = $INSPOOL/\*.done		#will need to be fixed if many files
loop: #this is the file search loop
  set nonomatch				#required for no file condition
  while (! -e $FILE)    	 	#wait for *.done file to appear
    sleep 60
    if (-e $INSPOOL/end) goto end
    if (-e $INSPOOL/error) goto error
  end
 
# File found in inspool directory, rename it into the raw directory.
# Remove the associated .done file
  ls $INSPOOL/*.done | sed "s-$INSPOOL/--" | sed "s/.done//" > tmp
  setenv INFILE `cat tmp`
  echo $INFILE
  mv $INSPOOL/$INFILE $RAW_AREA/$INFILE
  mv $INSPOOL/$INFILE.done $RAW_AREA/$INFILE.done
 
# Before running RECO, check that the outspooler is still there;
#if not - exit
  ps -ef | grep dzero > $TMP
  set outs_pid = `nawk '/outspool/ { print $2 }' $TMP`
  if ( $outs_pid == "" ) then
    set insp_pid = `nawk '/inspool/ { print $2 }' $TMP`
    kill -9 $insp_pid
    goto error
  endif
#-----------------------------------------------------
#
# Setup and run D0RECO
#-----------------------------------------------------
#
#Create filenames; first get the root of the name from the inputfile
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
  ln -sf $RAW_AREA/$INFILE       event_data
  ln -sf $DST_AREA/$DSTFILE      dst_output_data
  ln -sf $STA_AREA/$STAFILE      sta_output_data
  ln -sf $HST_AREA/${ROOT}.hst4  histograms
  ln -sf $SUM_AREA/${ROOT}.out   summary_output

# Generate useful information for log file
  echo "For $INFILE processing started:" `date`

# Run RECO
  d0reco.x

# make the PDB rcp file here
  setenv RECOSTS $status
  echo "D0RECO Completion Status is $RECOSTS"
  if ( $RECOSTS != 0 ) then
    nawk '$1 ~ "'$QUEID'" { $7 = "RUN_DS_FAIL"; print }'  \
    $HST > $UPD
    set comp_status = -1
  else
    nawk '$1 ~ "'$QUEID'" { $7 = "NEXT_DS_OK"; print }'  \
    $HST > $UPD
  endif

#Set the correct filenames
 set fdate = ` date '+%y%m%d%H' | nawk ' { print substr($0,2) }' `
 set pass = `nawk '/PROGRAM PASS NUMBER/ { printf("%02d", $5 ) } ' pm_status_1`
 set vrsn = `nawk '/PROGRAM VERSION NUMBER/ { printf("%02d", $5 ) } ' pm_status_1`
 set vers = $vrsn$pass
 set STANEW = ` echo $STAFILE | sed "s/ZZZZ/$vers/1; s/YYYYYYY/$fdate/1" `
 set DSTNEW = ` echo $DSTFILE | sed "s/ZZZZ/$vers/1; s/YYYYYYY/$fdate/1" `
 mv $STA_AREA/$STAFILE $STA_AREA/$STANEW
 mv $DST_AREA/$DSTFILE $DST_AREA/$DSTNEW
 setenv STAFILE $STANEW
 setenv DSTFILE $DSTNEW

# create the RCP file
  dst_rcp_write

# Check the number of events; if 0 - don't write it anywhere
  set NOFE = `nawk ' /EVENTS PROCESSED/ {print $4} ' pm_status_1`
  echo "Events processed $NOFE"

# If success copy the file to tape; else remove STA and DST
  rm $RAW_AREA/$INFILE
  if ( ( $RECOSTS == 0 ) && ( $NOFE >= 1 ) ) then
    mv $RAW_AREA/$INFILE.done $STA_AREA/$STAFILE.done
  else
    rm $STA_AREA/$STAFILE
    rm $DST_AREA/$DSTFILE
  endif

# Generate useful information for log file
  echo "For $INFILE processing ended:" `date`

goto loop #this is the file search loop
#
end:
rm $INSPOOL/end
#Change the job status
set dtm = `date`
set owd = ` echo $dtm | sed 's/ /_/g' `
if ( $comp_status == 0 ) then
  nawk '$1 ~ "'$QUEID'" { $7 = "SUCCESS"; dat = "'$owd'"; gsub(/_/," ",dat); print $0, dat }'  \
  $HST > $UPD
else
  nawk '$1 ~ "'$QUEID'" { $7 = "FINISHED"; dat = "'$owd'"; gsub(/_/," ",dat); print $0, dat }'  \
  $HST > $UPD
endif
goto finish

error:
echo 'spooler error' > $STA_AREA/end
set dtm = `date`
set owd = ` echo $dtm | sed 's/ /_/g' `
nawk '$1 ~ "'$QUEID'" { $3 = "'$$'"; $7 = "FAILED_ON_SP"; dat = "'$owd'"; gsub(/_/," ",dat); print $0, dat }'  \
$HST > $UPD

goto theend
finish:
#-----------------------------------------------------
#
# Wait for the final file to be spooled from the $STA area
# then finish up.
#
#-----------------------------------------------------
#
echo 'outspooler end' > $STA_AREA/endit
sleep 120

#Check that there is no outspooler process running
checkouts:
#Remove end and endit files
if ( -e $STA_AREA/end ) then
  rm $STA_AREA/end
  goto copy_here
endif
sleep 30
goto checkouts

#Spawn the process to copy the results to D0FS
copy_here:
copy_results $OUTLABEL $PM_DISK1/proman $PDB_AREA >& ${LOG_AREA}/copy_results_log.$OUTLABEL &

#Remove the file blocking the output tape contents
if ( -e $PRODIR/resources/outtape.$NODE ) then
  rm $PRODIR/resources/outtape.$NODE
endif


# Completion and error completion procedures 
 
# Normal exit
theend:
#Release the tape for the FM use
if ( $fmentry != -1 ) then
  rsh d0fsa -n -l dzero "delete/entry=$fmentry" >& /dev/null &
endif

#remove $TMP if it exists 
if ( -e $TMP ) then
  rm $TMP
endif

#Just for MC - remove tapes_moved.$INLABEL
if ( -e $PRODIR/history/tapes_moved.$INLABEL ) then
  rm $PRODIR/history/tapes_moved.$INLABEL
endif

#print a time
echo $QUEID ' DORECO.CSH ended' `date`
exit 0
