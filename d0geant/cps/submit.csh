#
# Submit a d0geant job to a production system
#
#  % submit d0geant production_system nevents iop
#
#    where:
#     d0geant = d0geant ( here just for historical reasons )
#     production_system = production system name ( as setup with "make_system")
#     nevents = number of input events to read
#     iop = class ordering in jdf file (i=input,o=output,p=production)
#
set proj = d0geant
set prod = $1
set ecnt = $2
set rand = 'pio'
setenv PTEST_DIR $cwd
set area = $PTEST_DIR/$proj/$prod
alias echo /bin/echo
setup cps

set ord = `echo $rand|tr "[A-Z]" "[a-z]"`
set f_class = `echo $ord|cut -c1,1`_class
set s_class = `echo $ord|cut -c2,2`_class
set t_class = `echo $ord|cut -c3,3`_class


set input = 'true'
while ( $input == 'true' )
  set indev = '/dev/null'
  echo '\nEnter input tape or file [ttb50.xsc]: \c'
  set infile = `line`
  if ( $infile == '' ) set infile = $PTEST_DIR/${proj}/cps_data/ttb50.xsc
  if ( -e $infile ) then
    set indev = disk
  else
    set pfx = `echo $infile |cut -c1-2 |tr '[A-Z]' '[a-z]'`
    if ( $pfx == 'vg' ) set indev = tape
  endif
  if ( $indev != '/dev/null' ) then
    echo "Input from $indev : $infile"
    echo 'Is this correct? [y]/n: \c'
    set ansr = `line`
    if ( $ansr == '' ) set ansr = 'y'
    if ( $ansr != 'y' ) then
      set input = 'true'
    else
      set input = 'false'
    endif
  else
    echo "$infile is neither an existing file nor a valid"
    echo "tape number. Try again."
  endif
end
set tmp1 = $infile:t
set defile  = $tmp1:r

set input = 'true'
while ( $input == 'true' )
  set outdev = '/dev/null'
  echo '\nEnter output tape or file [~/data]: \c'
  set outfile = `line`
  if ( $outfile == '' ) set outfile = ~/data
  set pfx = `echo $outfile |cut -c1-2 |tr '[A-Z]' '[a-z]'`
  if ( -e $outfile:h ) then
    set outdev = disk
  else
    if ( $pfx == 'vg' ) set outdev = tape
  endif
  echo '\nEnter the tape filename ['$defile']: \c'
  set CPS_TAPEFILE = `line`
  echo '\nEnter the max number of events/tapefile [100]: \c'
  set FMAXEVENT = `line`
  if ( $FMAXEVENT == '' ) set FMAXEVENT = 100
  if ( $CPS_TAPEFILE == '' ) set CPS_TAPEFILE = $defile
  if ( $outdev == 'disk' ) then
    set tmp = $outfile/$CPS_TAPEFILE
    set CPS_TAPEFILE = $tmp
  endif
  if ( $outdev != '/dev/null' ) then
    echo "Output to $outdev : $outfile"
    echo "Filename(s) is set to $CPS_TAPEFILE"
    echo "Max number of events/tapefile is set to $FMAXEVENT"
    echo 'Is this correct? [y]/n: \c'
    set ansr = `line`
    if ( $ansr == '' ) set ansr = 'y'
    if ( $ansr != 'y' ) then
      set input = 'true'
    else
      set input = 'false'
    endif
  else
    echo "$outfile is neither an existing directory nor a valid"
    echo "tape number. Try again."
  endif
end

set input = 'true'
while ( $input == 'true' )
  echo '\nEnter card input file [~/cards/stdinput]: \c'
  set cfile = `line`
  if ( $cfile == '' ) set cfile = ~/cards/stdinput
  if ( ! -e $cfile ) then
    echo "$cfile doesn't exist. Try again."
  else
    echo "Card File  : $cfile"
    echo 'Is this correct? [y]/n: \c'
    set ansr = `line`
    if ( $ansr == '' ) set ansr = 'y'
    if ( $ansr != 'y' ) then
      set input = 'true'
    else
      set input = 'false'
    endif
  endif
end


set input = 'true'
while ( $input == 'true' )
  echo '\nEnter FATMEN comment line : \c'
  set comment = `line`
  if ( "$comment" == '' ) set comment = 'NO COMMENT SUPPLIED'
    echo "FATMEN comment :\n $comment"
    echo 'Is this correct? [y]/n: \c'
    set ansr = `line`
    if ( $ansr == '' ) set ansr = 'y'
    if ( $ansr != 'y' ) then
      set input = 'true'
    else
      set input = 'false'
    endif
end

echo ""
echo "Output to     : $outfile"
echo "Input from    : $infile"
echo "Card file     : $cfile"
echo "Events to run : $ecnt"
echo "FATMEN comment: $comment"
echo "Job number is : $$"
echo ""

cd $area

/bin/rm/setup.$$ >& /dev/null
echo "#\!/bin/csh -f"						>> setup.$$
echo "setenv PTEST_DIR $PTEST_DIR"				>> setup.$$
cat ${proj}_setup 						>> setup.$$
echo "ln -sf $cfile STDIN"					>> setup.$$
if($indev  == 'disk') echo "ln -sf $infile INFILE"		>> setup.$$
if($outdev == 'disk') echo "ln -sf $outfile OUTFILE"		>> setup.$$
echo "echo Began $infile to $outfile on "'`date`'\
     "$$ >> $PTEST_DIR/${proj}_log"				>> setup.$$


/bin/rm/cleanup.$$ >& /dev/null
echo "#\!/bin/csh -f"						>> cleanup.$$
echo 'cp *.rcpdat ~/rcpdat'					>> cleanup.$$
echo "if(-e ${outfile}.success) cp ${outfile}.success ~/status"	>> cleanup.$$
echo "rm $area/setup.$$"					>> cleanup.$$
echo "rm $area/cleanup.$$"					>> cleanup.$$
echo "rm $area/${proj}$$.jcf"					>> cleanup.$$
echo "rm $area/${proj}$$.jdf"					>> cleanup.$$
echo "rm $area/production.$$"					>> cleanup.$$
echo "rm $area/input.$$"					>> cleanup.$$
echo "rm $area/output.$$"					>> cleanup.$$
echo "echo Ended $infile to $outfile on "'`date`'\
     "$$ >> $PTEST_DIR/${proj}_log"				>> cleanup.$$

/bin/rm/input.$$ >& /dev/null
echo "#\!/bin/csh -f"						>> input.$$
echo "setenv CPS_INPUT $indev"					>> input.$$
echo "setenv EVENT_COUNT $ecnt"					>> input.$$
echo "date"							>> input.$$
echo "$PTEST_DIR/${proj}/cps_batch/${proj}_input.x"		>> input.$$
echo "date"							>> input.$$


/bin/rm/output.$$ >& /dev/null
echo "#\!/bin/csh -f"						>> output.$$
echo "setenv CPS_OUTPUT $outdev"				>> output.$$
echo 'setenv CPS_DRIVE $$'					>> output.$$
echo "setenv CPS_TAPEFILE $CPS_TAPEFILE"			>> output.$$
echo "setenv FMAXEVENT $FMAXEVENT"				>> output.$$
echo "setenv TAPE_LABELS VMS"					>> output.$$
echo "setenv TAPE_MGR cps_tape"					>> output.$$
echo "setenv BATCH_DOMAIN $BATCH_DOMAIN"			>> output.$$
echo 'setenv COMMENT "'$comment'"'				>> output.$$
echo "date"							>> output.$$
echo "$PTEST_DIR/${proj}/cps_batch/${proj}_output.x"		>> output.$$
echo "date"							>> output.$$


/bin/rm/production.$$ >& /dev/null
echo "#\!/bin/csh -f"						>> production.$$
echo "setenv d0calor_off $PTEST_DIR/d0geant/cps_data/calor_off" >> production.$$
echo "setenv d0library $PTEST_DIR/d0geant/cps_data"    		>> production.$$
echo "$PTEST_DIR/${proj}/cps_batch/${proj}.x"			>> production.$$


/bin/rm/${proj}$$.jcf >& /dev/null
echo "setup file = setup.$$"					>> ${proj}$$.jcf
echo "cleanup file = cleanup.$$"				>> ${proj}$$.jcf
if ($indev == 'tape') then
  echo "class = 1"						>> ${proj}$$.jcf
  echo "tapes(1,input,exabyte) = $infile"			>> ${proj}$$.jcf
endif
if ($outdev == 'tape') then
  echo "class = 3"						>> ${proj}$$.jcf
  echo "tapes(2,output,exabyte) = $outfile"			>> ${proj}$$.jcf
endif

set return = second
goto $f_class
second:
set return = third
goto $s_class
third:
set return = submit
goto $t_class

p_class:
  cat ${proj}_pclass						>> ${proj}$$.jdf
  echo " PROGRAM = $area/production.$$"				>> ${proj}$$.jdf
  goto $return

i_class:
  cat ${proj}_iclass						>> ${proj}$$.jdf
  echo " PROGRAM = $area/input.$$"				>> ${proj}$$.jdf
  if ($indev  == 'tape') then
    echo " EXABYTE TAPES = 1"					>> ${proj}$$.jdf
  endif
  goto $return

o_class:
  cat ${proj}_oclass						>> ${proj}$$.jdf
  echo " PROGRAM = $area/output.$$"				>> ${proj}$$.jdf
  if ($outdev == 'tape') then
    echo " EXABYTE TAPES = 1"					>> ${proj}$$.jdf
  endif
  goto $return

submit:
chmod +x setup.$$ cleanup.$$ input.$$ output.$$ production.$$
cps_submit ${proj}$$ ${proj}$$ $prod
