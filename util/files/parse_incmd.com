$!========================================================================
$!
$! Name      : PARSE_INCMD
$!
$! Purpose   : Read and decode Input Command File (header part only)
$!
$!   Commands recognized by the procedure. Values are returned
$!     in global symbols of the same name.
$!
$!   Format:    COMMAND: value !comment
$!              END:
$!   See SAMPLE_*.CMD
$!
$!   INDEV/OUTDEV: specify input and output devices
$!      TA81   => Generic 8mm drive on FNAL
$!      Mxxx   => Physical tape on other clusters
$!      DISK   => read/write to disk 
$!      NL     => write to NULL device
$!   INVSN/OUTVSN: are input/output tape VISUAL labels, TAPE ONLY
$!      INVSN is used as the process name and to label the various flag files
$!   INLABEL/OUTLABEL: are tape labels or directory spec as appropriate
$!   INIT_OUT: flags whether to init the output tape or not
$!      INIT   => Init the tape
$!      NOINIT => Add output file to end of tape
$!   END: ends command section
$!
$! Arguments : P1 = File_spec of command file (.CMD assumed)
$!
$! Created  24-SEP-1991   A.M.Jonckheere
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$   open/read/err=no_file_err inlist 'p1'.CMD
$   indev    == "TA81"           !FNAL generic 8mm
$   invsn    == "NONE"
$   inlabel  == ""
$   outdev   == "DISK"
$   outvsn   == ""
$   outlabel == ""
$   INIT_OUT == "NOINIT"
$init_loop:
$   read/end=end_list_err inlist incmd
$   incmd = f$edit(incmd,"UNCOMMENT,COLLAPSE,UPCASE")
$   if f$length(incmd) .eq. 0 then goto init_loop
$   n = f$locate(":",incmd)
$   len = f$length(incmd)
$   if n .ge. len
$   then
$       WRITE SYS$OUTPUT "**-E-** Illegal command line: ''incmd'"
$       goto init_loop
$   endif
$   cmd   = f$extract(0,n,incmd)
$   label = f$extract(n+1,len,incmd)
$   if cmd .eqs. "INDEV" then indev == label
$   if cmd .eqs. "INVSN" then invsn == label
$   if cmd .eqs. "INLABEL" then inlabel == label
$   if cmd .eqs. "OUTDEV" then outdev == label
$   if cmd .eqs. "OUTVSN" then outvsn == label
$   if cmd .eqs. "OUTLABEL" then outlabel == label
$   if cmd .eqs. "INIT" then init_out == label
$   if cmd .nes. "END" then goto init_loop
$!
$   sh sym indev
$   sh sym invsn
$   sh sym inlabel
$   sh sym outdev
$   sh sym outvsn
$   sh sym outlabel
$   sh sym init_out
$   goto exit
$!
$no_file_err:
$   WRITE SYS$OUTPUT "**-F-** Could not open input list file - ABORT"
$   exit 6
$!
$EXIT:
$   EXIT
