$!========================================================================
$!
$! Name      : D0DAD_AUTO_MANAGE
$!
$! Purpose   : Daily updating of D0DAD master catalogs and files
$!    The selection of which catalog to update is made in CHOOSE_CATALOG
$!
$! Arguments : P1 - Function (ALL,UPDATE,STREAM,TRIGFILT) comma separated
$!             P2 - Mail recipient (CZAR?)
$!             P3 - Update interval for streaming (default 3x)
$!
$! Created   9-JAN-1995   John Hobbs - Rewrite/merge of existing
$!  D0DAD_AUTO_MANAGE, D0DAD_AUTO_UPDATE, D0DAD_AUTO_STREAM
$!
$!========================================================================
$
$ old=f$verify(0)
$ ON ERROR     THEN $ GOTO EXIT_SPOT
$ ON CONTROL_Y THEN $ GOTO EXIT_SPOT
$
$! All necessary global constants are initialized here...
$
$ mode="ALL"
$ if( P1.nes."" ) then mode = P1
$ mode = f$edit(mode,"UPCASE")
$
$ czar="FNALD0::HOBBS"
$ if( P2.nes."" ) then czar=P2
$
$ interval=3
$ if( P3.nes."" ) then interval=f$integer(P3)
$ delay=interval
$
$ extension="d0dad_all*"
$ td=f$cvtime(f$time())
$ today="000000"
$ last_update = "XX"
$ now=f$extract(2,2,td)+f$extract(5,2,td)+f$extract(8,2,td)
$
$ user = f$user()
$ ustart = f$locate(",",user)+1
$ uend = f$locate("]",user)
$ user = f$extract(ustart,uend-ustart,user)
$
$! Define all directories as symbolic constants in order to have easy
$! testing...
$
$ dir_todo = "d0$d0dad$todo:"
$ dir_done = "d0$d0dad$archive:"
$ dir_cats = "d0$d0dad$catalogs:"
$ dir_temp = "d0$d0dad$temp:"
$ dir_d0dad= "d0$data$d0dad:"
$ dir_trigfilt = "d0$data$d0dad$trigfilt:"
$ dir_strm = "d0$d0dad$stream_defn:"
$ dir_hook = "sys$Login_device:[''user'.d0dad]"
$
$! Define some files dependent on the directories
$
$ onejob_file="''dir_todo'management_job.present"
$ logfile_name="''dir_done'auto_manage.log"
$ stopfile_name="''dir_todo'auto_jobs.stop"
$
$! Setup the generic DCL commands as we wish...
$
$ del*ete := delete/nolog/noconf
$ ren*ame := rename/nolog/noconf
$ cop*y   := copy/nolog/noconf
$
$! Do the non-global variable setup here...
$
$ call setup_all
$ if( $status.ne.%x00000001 ) then exit '$status'
$
$! Run once per day...
$
$main_loop:
$
$  td=f$cvtime(f$time())
$  now=f$extract(2,2,td)+f$extract(5,2,td)+f$extract(8,2,td)
$  if( now.eqs.today ) 
$  then
$    wait 00:02
$    contxt=0
$    if( f$search(stopfile_name,contxt).nes."" ) then goto EXIT_SPOT
$    goto main_loop
$  endif
$  today=now
$  delay=delay+1
$
$! Define the update files for this cycle...
$
$ mail_file = "''dir_todo'errors_''today'.lis"
$ new_runs_wild = "new_runs_*.lis"
$ new_runs  = "new_runs_''today'.lis"
$ new_files = "new_files_''today'.lis"
$ 
$! Here we go...
$
$  if( f$search(mail_file).eqs."" ) then create 'mail_file'
$  open/append sys$manage_mailfile 'mail_file'
$  open/append/share=read sys$manage_logfile 'logfile_name'
$  tmp=f$cvtime(f$time())
$  tmp=f$extract(2,2,tmp)+f$extract(5,2,tmp)+f$extract(8,2,tmp)+" "+f$extract(11,2,tmp)+f$extract(14,2,tmp)
$  write sys$manage_logfile "Beginning UPDATE/STREAM cycle at "+tmp
$
$! Update the master catalogs...
$
$  if( mode.eqs."ALL" .or. f$locate("UPDATE",mode).lt.f$length(mode)) 
$  then
$    call update
$    if( $status.ne.%x00000001 ) 
$    then 
$      errcode = $status
$      write sys$manage_logfile "Fatal Error ''errcode' returned from UPDATE.  Quit"
$      goto fatal_exit
$    endif
$  endif
$
$! Check for streaming delay passed...
$
$  if( f$search('stopfile_name',contxt).nes."" ) then goto EXIT_SPOT
$  if( delay.lt.interval ) then goto main_loop
$  delay=0
$
$! Do the normal omni_filter based streaming...
$
$  if( mode.eqs."ALL" .or. f$locate("STREAM",mode).lt.f$length(mode) ) 
$  then
$    call omnifilt_stream
$    if( $status.ne.%x00000001 ) 
$    then 
$      errcode = $status
$      write sys$manage_logfile "Fatal Error ''errcode' returned from STREAMING.  Quit"
$      goto fatal_exit
$    endif
$  endif
$
$! Do the additional trig/filt based streaming...
$
$  if( mode.eqs."ALL" .or. f$locate("TRIGFILT",mode).lt.f$length(mode)) 
$  then
$    call trigfilt_stream
$    if( $status.ne.%x00000001 ) 
$    then 
$      errcode = $status
$      write sys$manage_logfile "Fatal Error ''errcode' returned from TRIGFILT.  Quit"
$      goto fatal_exit
$    endif
$  endif
$
$! Clean up and notify
$
$  close sys$manage_logfile
$  close sys$manage_mailfile
$  if( f$file_attributes(mail_file,"EOF").eq.0 ) 
$  then
$     mail/subject="D0DAD Update completed ''today'" NL: 'CZAR'
$  else
$     mail/subject="D0DAD Update Errors: ''today'" 'mail_file' 'CZAR'
$  endif
$  delete 'mail_file';*
$  goto main_loop
$
$
$EXIT_SPOT:
$  call unsetup_all
$  exit
$
$FATAL_EXIT:
$
$  write sys$manage_logfile "Done AUTO_MANAGE at "+f$time()+" **FATAL ERROR EXIT**"
$  close sys$manage_logfile
$  close sys$manage_mailfile
$  mail/subject="D0DAD Fatal error exit at ''today'" 'mail_file' 'CZAR'
$  old=f$verify(old)
$  exit $status
$
$!
$!=========================================================================
$!
$!  Do the global setup need by the management job...
$!
$!=========================================================================
$setup_all: SUBROUTINE
$
$! Do the d0dad generic setup (including a possible back door)...
$
$ temp=f$verify(0)
$ libtest d0dad
$ d0setup d0dad
$ temp=f$verify(temp)
$ if( f$search("''dir_hook'd0dad_define.com") .nes. "" ) 
$ then 
$   @'dir_hook'd0dad_define 
$ endif
$
$!  Setup 'private' log file
$
$ if( f$search(logfile_name).eqs."" ) then create 'logfile_name'
$ open/append/share=read sys$manage_logfile 'logfile_name'
$ write sys$manage_logfile "Starting d0dad_auto_manage, CZAR="+czar+", Date/Time="+now
$
$ d0dad :== 'd0dad' /debug=0
$
$! Protect against accidental startup...
$
$ czarname=czar
$ posn=f$locate("::",czar)+2
$ if(posn.le.f$length(czar)) then czarname=f$extract(posn,f$length(czar),czar)
$ a=f$edit(f$getjpi(f$pid(pidnum),"USERNAME"),"COLLAPSE")
$ if( a.nes."PROMAN" .and. a.nes.czarname .and. a.nes."SYSTEM" )  
$ then
$   write sys$OUtput "ILLEGAL USER: ''a'.  Stopping"
$   write sys$manage_logfile "ILLEGAL USER: ''a'.  Stopping"
$   exit %x00010001
$ endif
$
$! Check for halt requested...
$
$ if( f$search(stopfile_name).nes."" ) 
$ then
$   write sys$Output "FATAL ERROR: STOP FILE("+stopfile_name+") PRESENT"
$   write sys$manage_logfile "FATAL ERROR: STOP FILE("+stopfile_name+") PRESENT"
$   exit %x00030001
$ endif
$
$! Check for multiple copies of this job running...
$
$ if( f$search(onejob_file).nes."" ) 
$ then
$   write sys$Output "FATAL ERROR: MANAGEMENT JOB ALREADY RUNNING"
$   write sys$manage_logfile "FATAL ERROR: MANAGEMENT JOB ALREADY RUNNING"
$   exit %x00020001
$ endif
$
$ create 'onejob_file'
$ close sys$manage_logfile
$ exit %x00000001
$
$ENDSUBROUTINE
$!
$
$!=========================================================================
$!
$!  Update the master catalogs...
$!     Calls:  choose_catalog (below)
$!
$!=========================================================================
$update: SUBROUTINE
$ tmp=f$cvtime(f$time())
$ tmp=f$extract(2,2,tmp)+f$extract(5,2,tmp)+f$extract(8,2,tmp)+" "+f$extract(11,2,tmp)+f$extract(14,2,tmp)
$ write sys$manage_logfile "Starting D0DAD_AUTO_UPDATE at "+tmp
$
$! Move to todo directory and setup new run file...
$
$ set default 'dir_todo'
$ fname = f$search("''*.''extension'")
$ if( fname.eqs."" ) 
$ then
$   write sys$manage_logfile "  No files for updating at "+f$time()
$   exit %x00000001
$ endif
$
$! Setup new runs/files lists....
$
$ if( f$search("''dir_cats'''new_runs'").nes."" ) then -
     rename 'dir_cats''new_runs' 'dir_todo''new_runs'
$ if( f$search(new_runs).eqs."" ) then create 'new_runs'
$
$! Now process the new data...
$
$ directory/nohead/notrail *.'extension';*/output=todo.lis
$ sort todo.lis todo.lis-sort
$ rename todo.lis-sort todo.lis
$ open/read new$data todo.lis
$
$!---- This is necessary until SHARED mode works unders DFS  -------
$ copy 'dir_cats'run1a_mds.filecat run1a_mds.filecat_new
$ copy 'dir_cats'run1b_mds.filecat run1b_mds.filecat_new
$!------------------------------------------------------------------
$
$ contxt=0
$ write sys$manage_logfile "  Starting file scan at "+f$time()
$ update_loop:
$
$   read/end=DONE_UPDATE new$data fname
$   fname = f$parse(fname,,,"NAME") + f$parse(fname,,,"TYPE")
$   gosub choose_catalog
$   if( catalog.eqs."" ) then goto update_loop
$
$! Do the update
$   write sys$manage_logfile "   About to update ''catalog' using ''fname'"
$!   d0dad /mode=update 'fname' 'dir_cats''catalog'
$!------ Necessary until DFS supports SHARED/WRITE  ---------------------
$   d0dad /mode=update 'fname' 'dir_cats''catalog' 'catalog'.filecat_new
$!------------------------------------------------------------------------
$   if( $status.ne.1 ) 
$   then
$     write sys$manage_mailfile "FATAL ERROR returned from update for ''fname'" 
$     write sys$manage_logfile "FATAL ERROR returned from update for ''fname'"
$     exit %x01020001
$   endif
$   if( catalog.eqs."RUN1A_MDS" .or. catalog.EQS."RUN1B_MDS" ) then -
       d0dad /mode=info 'fname' 'new_runs'
$   gzip 'fname'
$   rename 'fname'-gz 'dir_done'
$   write sys$manage_logfile "      Done update using ''fname'"
$
$   if( f$search(stopfile_name).nes."" ) then goto DONE_UPDATE
$     
$ goto update_loop
$
$DONE_UPDATE:
$
$  close new$data
$  delete todo.lis;*
$  fname = f$search(new_runs)
$  if( fname.nes."" ) 
$  then
$    fs=f$file_attributes(fname,"EOF")
$    if( fs.eq.0 ) 
$    then 
$       delete 'new_runs';*
$!       rename run1a_QCD.filecat_new 'dir_cats'run1a_QCD.filecat
$!       rename run1b_QCD.filecat_new 'dir_cats'run1b_QCD.filecat
$       delete *.filecat_new;*
$! Necessary  until DFS supports  SHARED/WRITE.  ------
$    else
$       rename run1a_mds.filecat_new 'dir_cats'run1a_mds.filecat
$       rename run1b_mds.filecat_new 'dir_cats'run1b_mds.filecat
$!       rename run1a_QCD.filecat_new 'dir_cats'run1a_QCD.filecat
$!       rename run1b_QCD.filecat_new 'dir_cats'run1b_QCD.filecat
$!-----------------------------------------------------
$    endif
$  endif
$  purge/nolog/keep=2 'dir_cats'*.filecat
$
$  tmp=f$cvtime(f$time())
$  tmp=f$extract(2,2,tmp)+f$extract(5,2,tmp)+f$extract(8,2,tmp)+" "+f$extract(11,2,tmp)+f$extract(14,2,tmp)
$  write sys$manage_logfile "Done AUTO_UPDATE at "+tmp
$  exit %x00000001
$
$!------------------------------------------------------------------------------
$!  Determine the catalog to update based on run number and data type.
$!------------------------------------------------------------------------------
$
$choose_catalog:
$  catalog = ""
$  open/read uevtcat 'dir_todo''fname'
$  read/error=choose_catalog_done uevtcat line1
$  read/error=choose_catalog_done uevtcat line2 
$  close uevtcat
$
$  line2 = f$extract(1,f$length(line2)-1,line2)
$  ftype = f$extract(0,8,line1)
$  if( ftype.nes."D0DAD UE" ) 
$  then
$    rename 'dir_todo''fname' 'dir_done''fname'-badfile
$    write sys$manage_logfile "Unknown type "+ftype+" from file ''dir_todo'"+fname
$    return
$  endif
$ 
$  runnum = f$integer(f$extract(16,6,line2))
$  data_format = f$edit(f$extract(28,3,line2),"UPCASE")
$  stream_name = f$edit(f$extract(12,3,line2),"UPCASE")
$  if( runnum.le."69999" .and. data_format.eqs."MDS" ) then catalog="RUN1A_MDS"
$  if( runnum.le."69999" .and. data_format.eqs."MDC" ) then catalog="RUN1A_MDS"
$  if( runnum.gt."69999" .and. data_format.eqs."MDS" ) then catalog="RUN1B_MDS"
$  if( runnum.gt."69999" .and. data_format.eqs."MDC" ) then catalog="RUN1B_MDS"
$
$  if( stream_name.nes."ALL" )
$  then
$    stream_name = f$extract(0,1,stream_name)
$    if( stream_name.nes."P" ) then catalog=""
$  endif
$  
$  define/nolog this_run 'runnum'
$  if( catalog.eqs."" ) 
$  then
$    rename 'dir_todo''fname' 'dir_done''fname'-unknown-type
$    write sys$manage_logfile "Cannot determine catalog for file: "+line2
$  endif
$
$choose_catalog_done:
$  return
$ENDSUBROUTINE
$!
$
$!=========================================================================
$!
$! Do the omni_filter based streaming straight from the file catalog
$!
$!=========================================================================
$omnifilt_stream: SUBROUTINE
$
$!  If no new runs file exisits, no update needed.  D0DAD_AUTO_UPDATE will
$!  not create this file unless there are runs needing updating...
$
$ set default 'dir_todo'
$ if( f$search(new_runs_wild).eqs."" ) then exit %x00000001
$
$! Here we go...
$
$ tmp=f$cvtime(f$time())
$ tmp=f$extract(2,2,tmp)+f$extract(5,2,tmp)+f$extract(8,2,tmp)+" "+f$extract(11,2,tmp)+f$extract(14,2,tmp)
$ write sys$manage_logfile "Starting D0DAD_AUTO_STREAM at "+tmp
$
$!  Figure out which catalogs need restreaming (run1a and/or run1b)
$
$ do_run1a="no"
$ do_run1b="no"
$! contxt=10
$ check_all_loop:
$   fname = f$search(new_runs_wild)
$   if( fname.eqs."" ) then goto check_all_loop_done
$   open/read run_list 'fname'
$   runtest_loop:
$     read/end=runtest_done run_list runnum
$     runnum=f$extract(0,7,runnum)
$     if( runnum.les."  69999" ) then do_run1a="yes"
$     if( runnum.gts."  69999" ) then do_run1b="yes"
$     goto runtest_loop
$   runtest_done:
$   close run_list
$   goto check_all_loop
$ check_all_loop_done:
$
$ set def 'dir_temp'
$
$! Restream full-length d0dad files
$
$ if( do_run1a.eqs."yes" ) 
$ then
$   write sys$manage_logfile "    Starting global d0dadf file update for run1a at "+f$time()
$   d0dad /mode=stream 'dir_cats'run1a_mds.evtcat 0 'dir_strm'run1a.stream
$   if( $status.ne.1 ) 
$   then
$     write sys$manage_logfile "FATAL ERROR return from streaming Run1A"
$     write sys$manage_mailfile "FATAL ERROR return from streaming Run1A"
$     exit %x02010001
$   endif
$   rename run1a_*.d0dadf 'dir_d0dad'
$   purge 'dir_d0dad'run1a*.d0dadf
$ endif
$  
$ if( do_run1b.eqs."yes" ) 
$ then
$   write sys$manage_logfile "    Starting global d0dadf file update for run1b at "+f$time()
$   d0dad /mode=stream 'dir_cats'run1b_mds.evtcat 0 'dir_strm'run1b.stream
$   if( $status.ne.1 ) 
$   then
$     write sys$manage_logfile "FATAL ERROR return from streaming Run1B"
$     write sys$manage_mailfile "FATAL ERROR return from streaming Run1B"
$     exit %x02020001
$   endif
$   rename run1b_*.d0dadf 'dir_d0dad'
$   purge 'dir_d0dad'run1b*.d0dadf
$ endif
$
$!  Save updated runs list and tweaking the new_runs file...
$
$ set default 'dir_todo'
$ copy 'new_runs_wild' 'new_runs'_new
$ delete 'new_runs_wild';*
$ rename 'new_runs'_new 'new_runs'
$ check_newruns 'new_runs'
$ purge 'new_runs'
$ copy 'new_runs' 'dir_d0dad'
$ rename 'new_runs' 'new_files'
$ tmp=f$cvtime(f$time())
$ tmp=f$extract(2,2,tmp)+f$extract(5,2,tmp)+f$extract(8,2,tmp)+" "+f$extract(11,2,tmp)+f$extract(14,2,tmp)
$ write sys$manage_logfile "Done AUTO_STREAM at "+tmp
$
$ exit %x00000001
$ENDSUBROUTINE
$!
$
$!=========================================================================
$!
$! Do the trigger/filter based streaming from the new_files list...
$!
$!=========================================================================
$trigfilt_stream: SUBROUTINE
$  tmp=f$cvtime(f$time())
$  tmp=f$extract(2,2,tmp)+f$extract(5,2,tmp)+f$extract(8,2,tmp)+" "+f$extract(11,2,tmp)+f$extract(14,2,tmp)
$
$  set default 'dir_todo'
$  if( f$search(new_files).eqs."" ) 
$  then
$    write sys$manage_logfile "No files to stream on TRIG/FILT at"+tmp
$    exit %x00000001
$  endif
$
$ write sys$manage_logfile "Starting TRIGFILT_STREAM at "+tmp
$
$! This format initially is new_runs.  We will read this and reformat 
$! as file list only on the fly in order to get names correct...
$! This file must be in run order.  (This is insured if check_newruns is used)
$
$ open/readonly file_list 'new_files'
$trigfilt_scan_loop:
$  read/end=DONE_TRIGFILT_SCAN_LOOP file_list line
$  line=f$edit(line,"COMPRESS,TRIM")
$  runnum=f$extract(0,f$locate(" ",line),line)
$  line=f$extract(f$locate("!",line)+1,f$length(line),line)
$  get_next_file_in_line:
$    if( line.eqs."" ) then goto TRIGFILT_SCAN_LOOP
$    posn = f$locate(",",line)
$    infile = f$extract(0,posn,line)
$    write sys$output "Will scan/stream("+runnum+"): "+infile
$    write sys$manage_logfile "Will scan/stream("+runnum+"): "+infile
$    event_scan 'infile'
$    line=f$extract(posn+1,f$length(line),line)
$  goto GET_NEXT_FILE_IN_LINE
$goto TRIGFILT_SCAN_LOOP
$
$DONE_TRIGFILT_SCAN_LOOP:
$ close file_list
$ set def 'dir_temp'
$TRIGFILT_STREAM_LOOP:
$  fname = f$search("''dir_todo'events_*.lis")
$  if( fname.eqs."" ) then goto DONE_TRIGFILT_STREAM_LOOP
$  fname = f$parse(fname,,,"NAME")+f$parse(fname,,,"TYPE")
$  runnum = f$extract(7,6,fname)
$  fname  = "''dir_todo'''fname'"
$  tfname = "''dir_todo'trig_filt_''runnum'.lis"
$  sort/noduplicates 'fname' 'fname'-sort
$  rename 'fname'-sort 'fname'
$  purge 'fname'
$  if( runnum.lts."069999" ) 
$  then 
$    prefix="run1a_"
$    catalog="''dir_cats'run1a_mds"
$  else
$    prefix="run1b_"
$    catalog="''dir_cats'run1b_mds"
$  endif
$  d0dad /mode=text_stream /prefix='prefix' 'fname' 'tfname' 'catalog'
$  if( $status.ne.1 )
$  then
$    write sys$manage_logfile "FATAL ERROR return from trig/filt streaming"
$    write sys$manage_mailfile "FATAL ERROR return from trig/filt streaming"
$    exit %x03010001
$  endif
$  delete 'fname';*
$goto TRIGFILT_STREAM_LOOP
$
$! Merge the newly created D0DAD files with any existing ones...
$
$DONE_TRIGFILT_STREAM_LOOP:
$  set default 'dir_temp'
$  contxt=10
$TRIGFILT_MERGE_LOOP:
$  newfile = f$search("*.d0dadf",contxt)
$  if( newfile.eqs."" ) then goto DONE_TRIGFILT_MERGE_LOOP
$  newfile = f$parse(newfile,,,"NAME")+f$parse(newfile,,,"TYPE")
$!write sys$output "NewFile: "+newfile
$  oldfile = f$search("''dir_trigfilt'''newfile'")
$  if( oldfile.eqs."" ) then goto TRIGFILT_MERGE_LOOP
$!write sys$output "    Merging: ''oldfile'"
$!write sys$output "    and:     ''newfile'"
$  open/write mlist merge.lis
$  write mlist oldfile
$  write mlist newfile
$  close mlist
$  define file_names merge.lis
$  d0dad /mode=merge file_names temp.d0dadf
$  rename temp.d0dadf 'newfile'
$  purge 'newfile'
$  delete merge.lis;*
$  rename 'newfile' 'dir_trigfilt'
$  purge 'dir_trigfilt''newfile'
$goto TRIGFILT_MERGE_LOOP
$
$DONE_TRIGFILT_MERGE_LOOP:
$!  Just rename any d0dad files that haven't been merged
$ rename *.d0dadf 'dir_trigfilt'
$ purge 'dir_trigfilt'*.d0dadf
$ delete 'new_files';
$ tmp=f$cvtime(f$time())
$ tmp=f$extract(2,2,tmp)+f$extract(5,2,tmp)+f$extract(8,2,tmp)+" "+f$extract(11,2,tmp)+f$extract(14,2,tmp)
$ write sys$manage_logfile "Done TRIGFILT_STREAM at "+tmp
$ exit %x00000001
$
$ENDSUBROUTINE
$!
$
$!=========================================================================
$!
$!  Clean up before exitting
$!
$!=========================================================================
$unsetup_all: SUBROUTINE
$
$!  Do a thorough cleanup prior to exitting...
$
$  on error then $ exit
$  on control_y then $exit
$  contxt=0
$  open/append/share=read sys$manage_logfile 'logfile_name'
$  write sys$manage_logfile "Done AUTO_MANAGE at "+f$time()
$  close sys$manage_logfile
$  onejob_file=f$search(onejob_file)
$  if( onejob_file.nes."") then delete 'onejob_file'
$  old=f$verify(old)
$  exit %x00000001
$ENDSUBROUTINE
$!
