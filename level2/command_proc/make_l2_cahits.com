$!========================================================================
$!
$! Name      : MAKE_L2_CAHITS
$!
$! Purpose   : Build L2-specific CAHITS file for L2 STP building
$!             To be run under release of LEVEL2
$!
$! Created  31-DEC-1993   James T. Linnemann
$!          11-FEB-1994   Dan Claes - modified to run under release
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO ERROR_EXIT
$   ON CONTROL_Y THEN $ GOTO CONTY_EXIT
$   SWAP :== $D0$BETA_UTIL:SWAP
$   WORKDIR = F$TRNLNM("D0$LEVEL2$L2SIM")
$   IF WORKDIR.EQS."PROD$L2PROD"
$    THEN
$       WORKDIR = F$TRNLNM("PROD$L2PROD")
$    ELSE
$       WORKDIR = F$TRNLNM("D0$LEVEL2$ROOT") - "]" + "L2SIM]"
$   ENDIF
$   sdir = f$environment("DEFAULT")
$   SET DEFAULT 'WORKDIR'
$
$   copy d0$calor_off:cahits.rcp cahits.rcp
$   swap cahits.rcp "PEDESTAL_SIGMAS  T" "PEDESTAL_SIGMAS  F"
$   swap cahits.rcp  " DO_HOTSUP          T" " DO_HOTSUP          F"
$   DELETE SWAP.LOG;*
$   PURGE CAHITS.RCP
$EXIT:
$   d0$status :== TRUE
$   set default 'sdir'
$   EXIT
$ERROR_EXIT:
$CONTY_EXIT:
$   d0$status :== FALSE
$   set default 'sdir'
$   EXIT
