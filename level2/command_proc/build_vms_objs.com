$!========================================================================
$! Name      : BUILD_VMS_OBJs
$!             A small piece extracted from BUILD_VMS_EXE; recompile FRAMEs
$!             The full use of BUILD_VMS_EXE requires release
$!             of too many other libraries for now.
$!
$! Note      : VMS_FILTER_PARAMETERS, VMS_FILTER_INIT created, by hand, thru
$!             run of FILTER_MAKER,  and reflect the most current L2TOOL.DAT.
$!             These  FOR  files are placed in the CMS group L2SIM, compiled
$!             upon release (see below), with resulting  OBJs  kept in L2SIM.
$!
$! Arguments : p1 = SKIP will call  L2LIB_SETUP,  but perform no LIBTESTing
$!                = NOSETUP will not call L2LIB_SETUP
$!                  (option required when executing under release procedure)
$!
$! Modified 16-Jan-1993 James T. Linnemann  Use SUBROUTINE
$!
$!========================================================================
$   ON ERROR     THEN GOTO ERROR_EXIT
$   ON CONTROL_Y THEN GOTO CONTY_EXIT
$   SAY :== WRITE SYS$OUTPUT
$
$   say "*** Start Build_VMS_OBJS ''p1' ***"
$   sdir = f$environment("DEFAULT")
$   d0$status :== TRUE
$   @D0$LEVEL2$COMMAND_PROC:TEMP_DIR SET_DEFAULT
$   if .not.d0$status then goto ERROR_EXIT
$!
$! check and build directly in destination directory
$   SET DEFAULT 'L2_DESTdir'
$   IF (P1.NES."NOSETUP") THEN @D0$LEVEL2$COMMAND_PROC:L2LIB_SETUP 'p1'
$!
$   CALL MAKE_NEW VMS_FILTER_PARAMETERS
$   CALL MAKE_NEW VMS_FILTER_INIT
$   @d0$level2$command_proc:temp_dir DELETE
$   GOTO EXIT
$!
$MAKE_NEW: SUBROUTINE
$!
$   OBJtest = F$SEARCH("''P1'.FOR")
$   IF OBJtest.EQS."" THEN GOTO NO_NEW_CODE
$   OBJtest = F$SEARCH("''P1'.OBJ")
$   IF OBJtest.EQS."" THEN GOTO COMPILE_FILTER_CODE
$   OBJtest = F$SEARCH("DEB_''P1'.OBJ")
$   IF OBJtest.EQS."" THEN GOTO COMPILE_FILTER_CODE
$
$   T0 = F$FILE_ATTRIBUTES("''P1'.FOR","CDT")
$   T0 = F$CVTIME(T0)
$   T1 = F$FILE_ATTRIBUTES("''P1'.OBJ","CDT")
$   T1 = F$CVTIME(T1)
$   IF (T1.LTS.T0) THEN GOTO COMPILE_FILTER_CODE ! OBJ older than FOR
$   T1 = F$FILE_ATTRIBUTES("DEB_''P1'.OBJ","CDT")
$   T1 = F$CVTIME(T1)
$   IF (T1.GTS.T0) THEN GOTO NO_NEW_CODE ! OBJ newer than FOR
$COMPILE_FILTER_CODE:
$
$       FORT 'P1'.FOR
$       FORT/NOOPT/DEBUG=all/OBJ=DEB_'P1'.OBJ 'P1'.FOR
$       DEL *.LIS;*
$
$NO_NEW_CODE:
$ENDSUBROUTINE
$EXIT:
$   d0$status :== TRUE
$   set default 'sdir'
$   say "*** End Build_VMS_OBJS ''p1' ***"
$   EXIT
$ERROR_EXIT:
$CONTY_EXIT:
$   d0$status :== FALSE
$   set default 'sdir'
$   say "*** End Build_VMS_OBJS ''p1' -ERROR- ***"
$   EXIT
