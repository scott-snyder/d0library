#!/bin/csh
#========================================================================
#
# Name      : omni_mnf.csh
#
# Purpose   : Control of the OMNI job on SGI. 
#
#
# Created 16-Jul-1992 Kirill Denisenko
#========================================================================
#
#Disable the coredumps
limit coredumpsize 0

#Do necessary setups
set stream_def = $PRODIR/resources/stream_names.$FMLPHYS
set stream_name = ` nawk ' { print $1 } ' $stream_def `
set stream_file = ` nawk ' { print $2 } ' $stream_def `
set stream_area = ` nawk ' { print $3 } ' $stream_def `

cd $PRODAT/$FMLNODE/run

# Setup OMNI logical links 
echo "Version of omni_filter as seen from omni_mnf is $VERSION"
if ( $VERSION == "V10" ) then
  libprod -r $prod/omni_filter/old/v0110 omni_filter
  $PRODIR/tpm/omni_filter_setup.csh_v10
  ln -sf $prod/omni_filter/old/v0110/new/d0reco.rcp d0reco_rcp
else
  libprod omni_filter
  $PRODIR/tpm/omni_filter_setup.csh
  ln -sf $prod/omni_filter/d0reco.rcp d0reco_rcp
endif

#-----------------------------------------------------
#
# Main processing loop
#
#-----------------------------------------------------
#
echo 'MU DORECO.CSH started' `date`
#
doneloop:
cd $PRODAT/$FMLNODE

#Check the disk space here; if not enough - bail out
$PRODIR/exe/check_disk_space
if ( $status != 0 ) then
  sleep 299
  goto doneloop
endif

#Search for file to process. 
set nonomatch	
set FILE = `ls -rt inspool/*.done`
if ( $#FILE >= 1 ) then

#File found in inspool directory, rename it into the raw directory.
#Remove the associated .done file
# 
  setenv INFILE `echo $FILE[1] | nawk ' { gsub(/inspool\//,""); gsub(/\.done/,""); print } ' ` 
  mv inspool/$INFILE raw/$INFILE
  mv inspool/$INFILE.done raw/$INFILE.done
  set QUEID  = ` nawk ' { print $3 } ' raw/$INFILE.done `
 
#-----------------------------------------------------
# Setup and run OMNI
#-----------------------------------------------------
 
#Create filenames; first get the root of the name from the 
#INFILE
  set ROOT = $INFILE
  set ROOT = $ROOT:r
  set EXTN = $INFILE
  set EXTN = $EXTN:e
  set VERS = `echo $EXTN | nawk ' { gsub(/REU/,"REE",$1); print substr($1,8,7) } '`

#Define the STA name; note that we use Z and Y instead of the RECO version
#and the date; that will be changed after RECO completion
  cd $PRODAT/$FMLNODE/run
  ln -sf $PRODIR/stsdb/$pdb_dir/${PROJECT}_${QUEID}.STATUS pm_status_1
  set OUTSTA
  foreach stream ( $stream_file[*] )
    set OUTSTA = `echo $OUTSTA $STA_AREA/${ROOT}.${stream}_YYYYYYY`
  end

# Generate useful information for log file
  echo "For $INFILE processing started:" `date` >>& $LOG_AREA/${PROJECT}_${QUEID}.log

# Run OMNI 
  set streams = ( `echo $stream_name | tr '[A-Z]' '[a-z]'` )
  set n = 0
  foreach stream ( $streams[*] )
    @ n = $n + 1
    if ( $stream_area[$n] != "0" ) then
      ln -sf $OUTSTA[$n] sta_output_data_$stream
    else
      ln -sf /dev/null sta_output_data_$stream
    endif
    ln -sf /dev/null dst_output_data_$stream
  end
 
# Make additional symbolic links
  ln -sf  $RAW_AREA/$INFILE event_data
 
# Execute the program
  `uff $prodomni_filter/omni_filter.x` >>& $LOG_AREA/${PROJECT}_${QUEID}.log
  setenv RECOSTS $status
  echo '--------------------------------------------' > sum.tmp
  echo "Summary file ${ROOT}.sum"                    >> sum.tmp
  echo '--------------------------------------------'>> sum.tmp
  cat summary_output                                 >> sum.tmp
  mv sum.tmp $HOME/sta_streaming/archive/summary/$ROOT.sum
  rm summary_output
  rm histograms

# make the PDB rcp file here
  echo "D0RECO Completion Status is $RECOSTS"

#Remove the raw file
  rm $RAW_AREA/$INFILE
  rm $RAW_AREA/$INFILE.done

#Create RCP-files
  set chislo = `date | nawk ' { printf("%02d",$3); print "-"$2"-"$6,$4} ' | tr '[a-z]' '[A-Z]'`
  set nodename = `echo $FERMICSHRC | tr '[a-z]' '[A-Z]'`
  set fdate = ` date '+%y%m%d%H' | nawk ' { print substr($0,2) }' `
  $PRODIR/exe/make_staf_rcp.x $INFILE $chislo $nodename $RECOSTS $VERS $fdate
  if ( $status != 0 ) then
      echo "OMNI-RCP creation failed at " `date` "for the dataset" $INFILE | /usr/sbin/Mail -s  "Omni RCP Error"  unix_proman
      setenv RECOSTS -3
  endif

#Set the correct filenames
  foreach outfile ( $OUTSTA[*] )
    if ( $RECOSTS == 0 ) then
      set STAFILE = $outfile:t
      set STANEW = ` echo $STAFILE | sed "s/YYYYYYY/$fdate/1" `
      set STANEW = ` echo $STANEW  | sed "s/ZZZZZZZ/$VERS/1" `
      mv $STA_AREA/$STAFILE $STA_AREA/$STANEW
      mv $STA_AREA/${STAFILE}.done $STA_AREA/${STANEW}.done
    else
      rm $outfile
      rm ${outfile}.done
      echo $INFILE >> $PRODIR/history/crashes
    endif
  end

#Create RCP files
  if ( $RECOSTS != 0  ) then
    echo "OMNI crashed at " `date` "for the dataset" $INFILE | /usr/sbin/Mail -s "Omni Crash" unix_proman
  endif

# Generate useful information for log file
echo "For $INFILE processing ended:" `date` >>& $LOG_AREA/${PROJECT}_${QUEID}.log

else
  sleep 20
endif  # end of "done" found

goto doneloop
