$!========================================================================
$!
$! Name      : SIMPLE_COPY
$!
$! Purpose   : Simple copy procedure. Uses all the TAPE_UTIL procedures.
$!              Just does a simple DCL copy. Use as a template for more
$!              complicated procedures.
$!
$! Arguments : P1 - Input list file (.CMD assumed)
$!
$! Created  13-NOV-1991   Alan M. Jonckheere 
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$!   set default sys$login      ! or wherever
$!
$!================================================
$!   Set RMS_DEFAULT to improve tape throughput
$!================================================
$   set rms_default/seq/buffer_count=10 
$   show rms_default
$!
$!================================================
$!   Get input command file and parse header protion
$!================================================
$vp1:
$   IF p1 .eqs. ""
$   THEN
$       inquire p1 "Input list file"
$       goto vp1
$   ENDIF
$   @d0$util:parse_incmd 'p1'
$!
$!================================================
$!   Name process to meaningful name
$!================================================
$   set proc/name=d_'invsn'
$!
$!================================================
$!   Allocate and mount "tapes", if IO is to disk
$!   this will do the correct thing.
$!================================================
$   @d0$util:allocate_tape 'indev' IN 'inlabel'
$   @d0$util:allocate_tape 'outdev' OUT 'outlabel'
$   @d0$util:mount_tape 'indev' IN 'invsn' 'inlabel' NOINIT
$   @d0$util:mount_tape 'outdev' OUT 'outvsn' 'outlabel' 'init_out'
$!
$!================================================
$!   Main Processing Loop:
$!      Read rest of .CMD file for file names. 
$!      Process one file at a time.
$!   Put non-file specific setups here
$!================================================
$file_loop:
$   gosub chk_fin               ! Check if we should quit now
$   read/end=end_file inlist incmd
$   incmd = f$edit(incmd,"UNCOMMENT,COLLAPSE,UPCASE")
$   if f$length(incmd) .eq. 0 then goto file_loop
$   WRITE SYS$OUTPUT "      ''in_tape'''incmd'"
$!
$!================================================
$!   Put File specific setup here and run your program.
$!     The following symbols may be useful
$!       IN_TAPE  = input directory (tape or disk)
$!       OUT_TAPE = Output directory (tape or disk)
$!       INCMD    = Input file name
$!================================================
$   copy 'in_tape''incmd' 'out_tape'
$   goto file_loop
$!
$!================================================
$!   Wait for "Flag" file. Used to synchronize between
$!   processing job and any input/output servers
$!================================================
$wait_for_go:
$   file = f$search("do_''invsn'.flg")
$   IF file .eqs. ""
$   THEN
$       wait 0:1:0
$       goto wait_for_go
$   ELSE
$       del/log 'file'
$       file = f$search("junk.flg")
$   ENDIF
$   goto file_loop
$!
$!================================================
$!   Check for FIN_'invsn'.FLG
$!   If found quit gracefully.
$!================================================
$chk_fin:
$   IF f$search("FIN_''invsn'.FLG") .nes. ""
$   THEN
$       del/log FIN_'invsn'.FLG;*
$       goto end_file
$   ENDIF
$   return
$!
$!================================================
$!   Graceful finish
$!================================================
$end_file:
$   IF outdev .nes. "DISK" .AND. outdev .nes. "NL" THEN  -
        dir/siz=all/date/output='outlabel'.lis tape_output
$   goto exit
$!
$!================================================
$!   Premature EOF error exit
$!================================================
$end_list_err:
$   WRITE SYS$OUTPUT -
        "**-F-** Premature end of file on input list file - ABORT"
$   goto exit
$!
$!================================================
$!   Normal exit, do some cleanup
$!================================================
$EXIT:
$   if f$trnlnm("inlist") .nes. "" then close inlist
$   @d0$util:deallocate_tape tape_input nounload
$   @d0$util:deallocate_tape tape_output nounload
$   if f$mode() .eqs. "BATCH" then logout
$   EXIT
