$!------------------------------------------------
$!
$! Name      : SETUP_ISARCP
$!
$! Purpose   : Sets it up.
$!
$! Arguments : P1 = ISARCP.RCP (RCP file)
$!             P2 = PTLOW
$!             P3 = PTHI
$!             P4 = Print_file
$!             P5 = OUTPUT_DEVICE
$!             P6 = NUMBER OF EVENTS TO GENERATE PER PT RANGE
$!
$! Created   7-NOV-1989   Rajendran Raja
$! Modified 21-DEC-1990   Harrison B. Prosper
$!      Check P1, add call to SETUP_USER
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$   IF P1 .EQS. ""
$   THEN
$       P1 = "ISARCP"
$   ENDIF
$
$@DEFINE
$DEFINE/NOLOG ISARCP_RCP  'P1'.RCP
$SHOW LOGICAL ISARCP_RCP
$DEFINE/NOLOG CAL_STPFILE D0$STP:CAL_STPFILE.DAT
$
$!================================================
$!
$!   Do user setup if needed
$!
$!================================================
$   IF F$SEARCH("SETUP_USER.COM") .NES. ""
$   THEN
$
$   CMDFILE = "PT_''P2'_''P3'.CMD"
$   OUTFILE = "''P5':PT_''P2'_''P3'.DAT"
$   PRTFILE = "''P5':PT_''P2'_''P3'.PRT"
$   COPY  PTXLOTOXHI.CMD   'CMDFILE'
$   SWAP/NOLOG 'CMDFILE' XLO 'P2'
$   SWAP/NOLOG 'CMDFILE' XHI 'P3'
$   SWAP/NOLOG 'CMDFILE' XEV 'P6'
$   PURGE 'CMDFILE',*.LOG
$
$       WRITE SYS$OUTPUT "  "
$       WRITE SYS$OUTPUT " The following command file ''cmdfile' has been made "
$       WRITE SYS$OUTPUT "  "
$TYPE/NOPA 'CMDFILE'
$
$       WRITE SYS$OUTPUT "  "
$       WRITE SYS$OUTPUT "  --- @ SETUP_USER --- "
$       WRITE SYS$OUTPUT "  "
$
$       @SETUP_USER  'CMDFILE' 'PRTFILE' 'OUTFILE'
$   ELSE
$       WRITE SYS$OUTPUT "  "
$       WRITE SYS$OUTPUT "  **** WARNING **** "
$       WRITE SYS$OUTPUT "  "
$       WRITE SYS$OUTPUT "  Use a SETUP_USER.COM to define the logicals:"
$       WRITE SYS$OUTPUT "  "
$       WRITE SYS$OUTPUT "  PRINT_FILE, OUTPUT_DEVICE, COMMAND_FILE"
$       WRITE SYS$OUTPUT "  "
$   ENDIF
$EXIT:
$   EXIT
