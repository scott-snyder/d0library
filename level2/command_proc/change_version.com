$!========================================================================
$!
$! Name      : CHANGE_VERSION   p1  p2 (p3)
$!
$! Purpose   : Switch from one MC-version STP file to another
$!                               SFTVSN 1 data (series M or earlier)
$!                               SFTVSN 2 data (series N)
$!                               SFTVSN 3 data (series P or Q)
$!                               SFTVSN 4 data (series R,S or MC w/CADMAKE)
$!                      and switch between old and new Muon Constants
$!
$! Arguments : p1 = SFTVSN #
$!             p2 = MUVER   ( OLD or NEW or SRV )
$!             p3 = "STP" for calls from within NEW_STP.COM (rebuild .STP)
$!                    "" just to reset VMS_FILTER_STP logical to standard
$!                                    prebuilt STP files
$!
$! Created  29-SEP-1991   James T. Linnemann
$!          15-OCT-1991   DC  switch to either MC series in single COM
$!                            plus numerous other goodies
$!          28-APR-1992   James T. Linnemann add muon; remove std SFTVSN = 1
$!          12-MAY-1992   DC  DATA option sets SFTVSN/Mu_VSN
$!          23-JUL-1992   DC  New SRV option for Muon DATA-only STP
$!                            and new L2_EM_RCPs for MC and TB(data)
$!          09-NOV-1992   DC  Changes in CADMAKE format
$!          28-NOV-1992   James T. Linnemann simplify CAHITS, CALEVT
$! Modified 15-JAN-1993   James T. Linnemann unify mods of l2lib, l2rcp
$!          25-JUN-1993   DC  SRV option also used to select MU$CONST for
$!                            Series S Monte Carlo
$!========================================================================
$   ON ERROR     THEN $ GOTO ERROR
$   ON CONTROL_Y THEN $ GOTO ERROR
$   SAY :== WRITE SYS$OUTPUT
$   say "*** Start CHANGE_VERSION ''p1' ''p2' ''p3' ***"
$   DEF_DIR = F$ENVIRONMENT( "DEFAULT" )
$!
$START:
$ IF p1.EQS.""
$    THEN SAY " "
$    SAY "SFTVSN=D (Setup for DATA SFTVSN=2 w/Mu_VSN=SRV)"
$    SAY "SFTVSN=D (Setup for DATA w/CADMAKE)"
$    SAY "SFTVSN=1 (MC series M or earlier)"
$    SAY "SFTVSN=2 (MC series N)"
$    SAY "SFTVSN=3 (MC series P or Q)"
$    SAY "SFTVSN=4 (MC series R,S or MC w/CADMAKE)"
$    INQUIRE/NOPUNCT p1 "SFTVSN="
$ ENDIF
$ IF (p1.EQS."D").OR.(p1.EQS."DATA")
$   THEN p1 = "2"
$   p2 = "SRV"
$   VSN = "D"
$   RCP_TYPE = "DEFAULT"        != data
$   mufile = "D0$STP$MUON:MUON_L2_4_94.CONST"
$   GOTO OK_ALL
$ ENDIF
$ VSN = p1
$ IF p1.EQS."3"
$   THEN SAY "        **** WARNING ****        "
$   SAY "If VERSION P,Q Monte Carlo data crashes L2SIM,"
$   SAY "run again with   PATH = GEAN   in CADMAKE.RCP"
$!   SAY "    If this doesn't work contact Chip Stewart"
$   SAY " "
$ ENDIF
$ IF (p1.EQS."1").OR.(p1.EQS."2").OR.(p1.EQS."3").OR.(p1.EQS."4") THEN GOTO OK_PAR
$ p1 = ""
$ GOTO START
$OK_PAR:
$   RCP_TYPE = "MC"
$ IF (p2.EQS."OLD").OR.(p2.EQS."NEW").OR.(p2.EQS."SRV") THEN GOTO OK_PAR2
$ p2 = ""
$ SAY " "
$ SAY "Mu Version=OLD (MC series N or before 3.14 :     use MUO_STPFILE.DAT)"
$ SAY "Mu Version=NEW (MC series P,R or any special N:  use MUO_STPFILE_1.DAT)"
$ SAY "Mu Version=SRV (Survey geometry for MC series S: use MUO_STPFILE_SRV_2.DAT)"
$ INQUIRE/NOPUNCT p2 "Mu Version="
$ GOTO START
$OK_PAR2:
$ IF (p2.EQS."OLD") THEN mufile = "D0$STP$MUON:MUO_STPFILE.DAT"
$ IF (p2.EQS."NEW") THEN mufile = "D0$STP$MUON:MUO_STPFILE_1.DAT"
$!IF (p2.EQS."SRV") THEN mufile = "D0$STP$MUON:MUO_STPFILE.SRV"
$ IF (p2.EQS."SRV") THEN mufile = "D0$STP$MUON:MUO_STPFILE_SRV_2.DAT"
$OK_ALL:
$ SAY "Using ''mufile'"
$ dquote := """"" "
$ squote := """ "
$!
$! Edit RCP_TYPE parameter call to _D0USER.COM in L2LIB_SETUP.COM
$!
$   CALL COPY_FILE_HEADER L2LIB_SETUP.COM
$   filt_line = "$@FILTER_DEFAULT:VMS_FILTER_D0USER.COM "
$   WRITE NEW_FILE filt_line + dquote + dquote + dquote + rcp_type
$   filt_line = "$ DEFINE VMS_FILTER_STP D0$LEVEL2$L2SIM:VMS_FILTER_''VSN'_''p2'_MU.STP"
$   IF p3.NES."" THEN filt_line = "$ DEFINE VMS_FILTER_STP 'DEF_DIR'VMS_FILTER.STP"
$   WRITE NEW_FILE filt_line
$   CLOSE NEW_FILE
$   @l2lib_setup.com SKIP
$ IF p3.EQS.""
$   THEN
$!  Just trying to change the logicals in l2lib_setup
$   TEST = F$SEARCH("D0$LEVEL2$L2SIM:VMS_FILTER_''VSN'_''p2'_MU.STP")
$   IF (TEST.EQS."")
$   THEN
$      SAY "Can't find standard STP for SFTVSN = ''VSN' and Mu Version = ''p2'"
$      SAY "You will have to make one by @D0$LEVEL2$COMMAND_PROC:NEW_STP"
$      GOTO EXIT
$   ENDIF
$ ELSE
$   DEFINE DBL3$CAL D0$STP$CAL  ! pick up archived gain-corrected DAT file
$!  Since making a new STP file, modify L2RCP_SETUP.COM in user's area
$   CALL COPY_FILE_HEADER L2RCP_SETUP.COM
$   filt_line = "$@FILTER_DEFAULT:L2RCP_STANDARD.COM "
$   WRITE NEW_FILE filt_line + dquote + dquote + dquote + rcp_type
$!
$!   WRITE NEW_FILE "$ DEFINE CAHITS_RCP   D0$LEVEL2$L2SIM:CAHITS_NOGAINS.RCP"
$   WRITE NEW_FILE "$ DEFINE CL2HITS_RCP  RCP_AREA:CL2HITS_''VSN'.RCP"
$   WRITE NEW_FILE "$ DEFINE MU$CONST     ''mufile'"
$   put = "WRITE SYS$OUTPUT "
$!   filt_line = " ---------CAHITS_RCP redefined for STP building---------"
$!   WRITE NEW_FILE put + squote + filt_line + squote
$   filt_line = " ------DBL3$CAL logical redefined for STP building------"
$   WRITE NEW_FILE put + squote + filt_line + squote
$   filt_line = "     Reset by @L2LIB_SETUP before running VMS_FILTER"
$   WRITE NEW_FILE put + squote + filt_line + squote
$   CLOSE NEW_FILE
$   @L2RCP_SETUP.COM            !and run it
$ ENDIF
$EXIT:
$   say "*** End CHANGE_VERSION ''p1' ''p2' ''p3' ***"
$   EXIT
$ERROR:
$ CLOSE OLD_FILE
$ CLOSE NEW_FILE
$   say "*** End CHANGE_VERSION ''p1' ''p2' ''p3' -ERROR- ***"
$   EXIT
$COPY_FILE_HEADER:  SUBROUTINE
$   filename = P1
$   TEST = F$SEARCH("''filename'")
$!                                              ! Check if COM file is being
$   IF TEST.EQS."" THEN COPY D0$LEVEL2$COMMAND_PROC:'filename' 'DEF_DIR''filename'
$   OPEN/READ/ERROR=OPEN_ERR OLD_FILE 'filename'
$   OPEN/WRITE NEW_FILE 'filename'
$READ_LOOP:
$   READ  OLD_FILE LINE
$   WRITE NEW_FILE LINE
$   IF f$locate("$SKP:",LINE).NE.0 THEN GOTO READ_LOOP
$   CLOSE OLD_FILE
$!leave NEW_FILE open so new info can be appended
$EXIT
$OPEN_ERR:
$      SAY " "
$      SAY "Failed to find your ''FILENAME'"
$      SAY " "
$EXIT
$ENDSUBROUTINE
