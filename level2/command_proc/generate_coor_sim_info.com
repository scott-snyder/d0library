$!==========================================================================
$! Name      : GENERATE_COOR_SIM_INFO
$!           << COM file to be run by release procedure >>
$!
$! Purpose   : To build the official COOR_SIM output files of INFO,
$!             DAT,  and ZDAT for the L1/L2 Simulation of all sets
$!             of default Trigger Sets:  Bland,  Dzero,  Grannis
$!
$! Created   3-JAN-1992   Daniel R. Claes
$!           7-DEC-1992   Drop BLAND,GRANNIS sets.Work straight in L2SIM dir.
$!           7-DEC-1993   Set/reset FORTRAN version to match link conditions
$!                        of new COOR_SIM exe.
$!==========================================================================
$   ON ERROR     THEN $ GOTO ERROR_EXIT
$   ON CONTROL_Y THEN $ GOTO CONTY_EXIT
$   SAY :== WRITE SYS$OUTPUT
$   say "*** Start GENERATE_COOR_SIM_INFO.COM ***"
$
$!   DEFINE/NOLOG TRIGset D0$CONFIGS$SOURCE:DZERO.CFG
$
$   DEFINE/NOLOG TRIGset DZERO.CFG
$   sdir = f$environment("DEFAULT")
$
$   @D0$LEVEL2$COMMAND_PROC:TEMP_DIR SET_DEFAULT
$
$   @D0$LEVEL2$COMMAND_PROC:L2LIB_SETUP SKIP
$
$   COPY D0$CONFIGS$SOURCE:DZERO.GLB-TRIGLIST *
$   COPY D0$CONFIGS$SOURCE:TRIG_RESOURCES.CTL *
$   TRIGP DZERO.GLB-TRIGLIST -resource trig_resources.ctl
$
$!
$!   @site_products$root:[com]setproduct.COM FORTRAN.V6
$!   @LIB:[LIB.FORTRAN.V6]SETUP
$   SET NOON
$   IF ( F$SEARCH("SYS$SYSTEM:FORTRAN$MAIN.EXE") .NES. "" )
$   THEN
$	DTN := "Define/Translation_Attributes=Terminal/NoLog"
$	DTN Fortran$Main    Fortran$Main
$	DTN Mthrtl          Mthrtl
$	DTN Vmthrtl         Vmthrtl
$       SAY "FORTRAN Version 6.0 has been SETUP to run COOR_SIM"
$   ELSE
$	WRITE SYS$ERROR "%SETUP-E-NOFORTV6, FORTRAN V6 has not been installed yet."
$	GOTO ERROR_EXIT
$   ENDIF
$!
$   RUN D0$CONFIGS$COOR_SIM:COOR_SIM.EXE
      D0$CONFIGS$SOURCE:COOR_SIM.CTL
        TRIGset
$   COPY TRIG_FILT_RUN.INFO TRIG_FILT_0000000.INFO
$! Need to wait for generation of ZDAT file
$   COUNT = 0
$ZDAT_WAIT:
$   WAIT 00:00:05.00
$   Ztest = F$SEARCH("RCP_0000001.ZDAT")
$   TIME = F$TIME()
$   IF Ztest.EQS."" 
$     THEN 
$     SAY "Waiting for RCP_0000001.ZDAT to be created"
$     COUNT = COUNT + 1
$     IF COUNT.EQS.50
$       THEN GOTO MISS
$     ENDIF
$     GOTO ZDAT_WAIT
$   ENDIF
$   GOTO EXIT
$MISS:
$   TYPE/PAGE RCP_0000001.ERR
$   SAY "** WARNING **"
$   SAY " New .ZDAT not created "
$   SAY " "
$   GOTO ERROR_EXIT
$EXIT:
$   RENAME *.INFO,*.RES,RUN*.DAT,RCP*.DAT,RCP*.ZDAT 'L2_destdir'
$   @d0$level2$command_proc:temp_dir delete
$   d0$status :== TRUE
$   SET DEFAULT 'sdir'
$!   SETUP FORTRAN.V5
$   @LIB:[LIB.FORTRAN.V5]SETUP
$   SAY "Reset to FORTRAN Version 5"
$   say "*** End GENERATE_COOR_SIM_INFO.COM ***"
$   EXIT
$ERROR_EXIT:
$CONTY_EXIT:
$   d0$status :== FALSE
$   SET DEFAULT 'sdir'
$   say "*** End GENERATE_COOR_SIM_INFO.COM ***"
$   EXIT
