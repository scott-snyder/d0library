setup cps
source `uff $d0unix/d0local.cshrc`
libprod omni_filter
#
# This file should be sourced to define environment variables related to 
# STA streaming production.
#
setenv STREAMS "MIN01 QC201 QC101 ELF01 MUF01 MET01 RGE01 WZS01 TOP01 FAK01"
setenv FATSTREAMS "MIN02NONEX00 QC203NONEX00 QC102NONEX00 ELF02NONEX00 MUF02NONEX00 MET02NONEX00 ALL00RGEXX05 ALL00WZSXX02 ALL00TOPXX03 ALL00FAKXX02"
setenv NUM_STREAMS `echo $STREAMS | wc -w`
setenv NUM_JOBS 4
setenv NUM_INSPOOL 4
setenv NUM_OUTSPOOL 5
setenv PRODLIB $HOME/sta_streaming/prodlib
setenv STREAM_SPOOL "5 5 5 1 2 3 4 5 5 5"
if( $HOST == fnstfz )then
  setenv DRIVES  'sts40 sts41 sts42 sts43 sts44 sts45 sts46 sts47'
  setenv MIN_INSPOOL 1
  setenv MAX_INSPOOL 2
  setenv MIN_JOBS 1
  setenv MAX_JOBS 2
  setenv MIN_OUTSPOOL 1
  setenv MAX_OUTSPOOL 5
endif
if( $HOST == fnstfy )then
  setenv DRIVES  'sts60 sts61'
  setenv MIN_INSPOOL 3
  setenv MAX_INSPOOL 4
  setenv MIN_JOBS 3
  setenv MAX_JOBS 4
  setenv MIN_OUTSPOOL 1
  setenv MAX_OUTSPOOL 0
endif
setenv TAPE_DRIVES $HOME/database/tape_drives.$HOST
setenv PROJECT $HOME/sta_streaming/project
setenv WORK $PROJECT/work
setenv RCP_AREA $WORK/data1
setenv HOLD $HOME/sta_streaming/hold
setenv ARCHIVE $HOME/sta_streaming/archive
setenv PATH ${PRODLIB}:`dropit $PRODLIB`
setenv SLEEP_TIME 120
setenv MAX_SPACE 500000
setenv JOB $PRODLIB/omni_filter.csh
#
#Set up environmental variables for RBIO
setenv TAPE_MGR cps_tape
setenv TAPE_LABELS VMS
#
setenv input_tape $PROJECT/database/input_tape
setenv input_file $PROJECT/database/input_file
setenv output_tape $PROJECT/database/output_tape
setenv output_file $PROJECT/database/output_file
setenv run $PROJECT/run
#
# Make sure all directories exist
#
set dirs = ''
set dirs = ( $dirs $PRODLIB )
set dirs = ( $dirs $PROJECT/input )
set dirs = ( $dirs $PROJECT/database )
set dirs = ( $dirs $PROJECT/run )
set dirs = ( $dirs $PROJECT/run/input_server )
set dirs = ( $dirs $HOLD/data )
set dirs = ( $dirs $HOLD/input )
set dirs = ( $dirs $HOLD/input_server )
set dirs = ( $dirs $ARCHIVE )
set dirs = ( $dirs $ARCHIVE/todo )
set dirs = ( $dirs $ARCHIVE/tofatmen )
set dirs = ( $dirs $ARCHIVE/toproddb )
set dirs = ( $dirs $ARCHIVE/done )
set dirs = ( $dirs $ARCHIVE/event_list )
set dirs = ( $dirs $ARCHIVE/summary )
foreach dir ( $dirs )
  if( -d $dir )continue
  mkdir -p $dir
  if( -d $dir )then
    echo "Created directory $dir"
  else
    echo "Failed to create directory $dir"
  endif
end
set dirs = ''
set n = 0
while ( $n < $NUM_OUTSPOOL )
  @ n = $n + 1
  set dirs = ( $dirs $PROJECT/run/outspool$n )
  set dirs = ( $dirs $HOLD/outspool$n )
  set dirs = ( $dirs $HOLD/stat$n )
  foreach dir ( $WORK/data* )
    set dirs = ( $dirs $dir/outspool$n )
  end
end
foreach dir ( $dirs )
  if( -d $dir )continue
  mkdir -p $dir
  if( -d $dir )then
    echo "Created directory $dir"
  else
    echo "Failed to create directory $dir"
  endif
end
set dirs = ''
set n = 0
while ( $n < $NUM_STREAMS )
  @ n = $n + 1
  set dirs = ''
  set dirs = ( $dirs $HOLD/staf$n )
  set dirs = ( $dirs $HOLD/fatmen$n )
  foreach dir ( $WORK/data* )
    set dirs = ( $dirs $dir/output$n )
    set dirs = ( $dirs $dir/staf$n )
    set dirs = ( $dirs $dir/fatmen$n )
  end

  foreach dir ( $dirs )
    if( -d $dir )continue
    mkdir -p $dir
    if( -d $dir )then
      echo "Created directory $dir"
    else
      echo "Failed to create directory $dir"
    endif
  end
end
set dirs = ''
set n = 0
while ( $n < $NUM_INSPOOL )
  @ n = $n + 1
  set dirs = ( $dirs $PROJECT/run/inspool$n )
  set dirs = ( $dirs $HOLD/inspool$n )
  foreach dir ( $WORK/data* )
    set dirs = ( $dirs $dir/inspool$n )
  end
end
set n = 0
while ( $n < $NUM_JOBS )
  @ n = $n + 1
  set dirs = ( $dirs $PROJECT/run/stream$n )
  set dirs = ( $dirs $HOLD/stream$n )
  foreach dir ( $WORK/data* )
    set dirs = ( $dirs $dir/input$n )
  end
end
foreach dir ( $dirs )
  if( -d $dir )continue
  mkdir -p $dir
  if( -d $dir )then
    echo "Created directory $dir"
  else
    echo "Failed to create directory $dir"
  endif
end
