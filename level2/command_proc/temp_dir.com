$!========================================================================
$! Name      : TEMP_DIR .COM
$!           << COM file to be run by release procedure >>
$!
$! Purpose   : Create or Destroy a temporary working directory for L2 release
$!             Also defines global symbol L2_DESTDIR
$!             returns d0$status: success or failure
$!
$! Arguments : p1       = SET_DEFAULT   build .TEMP if needed; then set default
$!                                          to it
$!                                      It's under either the production area,
$!                                      or L2SIM.
$!                        DELETE        delete the .TEMP directory and its
$!                                          contents
$! Created   6-JAN-1992   Daniel R. Claes
$! Modified 20-JAN-1993   James T. Linnemann this piece as separate .com's
$!========================================================================
$   ON ERROR     THEN $ GOTO ERROR_EXIT
$   ON CONTROL_Y THEN $ GOTO CONTY_EXIT
$   SAY :== WRITE SYS$OUTPUT
$!
$   say "*** Start TEMP_DIR ''p1' ***"
$   WORKDIR = F$TRNLNM("D0$LEVEL2$L2SIM")
$
$   IF WORKDIR.EQS."PROD$L2PROD"
$   THEN
$       WORKDIR = F$TRNLNM("PROD$L2PROD")   ! capture the leading string in
$       L2_DESTDIR == F$TRNLNM("PROD$L2PROD") ! the search list (usually L2$NEW)
$   ELSE
$       WORKDIR = F$TRNLNM("D0$LEVEL2$ROOT") - ".]" + "]"
$       L2_DESTDIR == F$TRNLNM("D0$LEVEL2$ROOT") - "]" + "L2SIM]"
$   ENDIF
$
$   SET DEFAULT 'WORKdir'
$
$   DIRtest = F$SEARCH("TEMP.DIR")
$   IF DIRtest.EQS.""
$   THEN
$      CREATE/DIRECTORY [.TEMP]
$   ENDIF
$   SET DEFAULT [.TEMP]
$
$ IF (P1.EQS."DELETE")
$   THEN
$     IF F$SEARCH("*.*").NES."" THEN DELETE/NOCONFIRM *.*;*
$     SET DEFAULT [-]
$     SET FILE/PROTECTION=(O:RWED) TEMP.DIR
$     SET ACL/DEFAULT TEMP.DIR
$     DELETE/NOCONFIRM TEMP.DIR;*
$   ENDIF
$
$EXIT:
$   d0$status :== TRUE
$   say "*** End TEMP_DIR ''p1' ***"
$   EXIT
$ERROR_EXIT:
$CONTY_EXIT:
$   d0$status :== FALSE
$   say "*** End TEMP_DIR ''p1' -ERROR- ***"
$   EXIT
