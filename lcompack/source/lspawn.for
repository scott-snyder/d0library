      INTEGER FUNCTION LSPAWN(CMDLIN,FILEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Spawn subprocess to perform single command,
C-                         save output for later display. VAX-specific
C-
C-   Inputs  : CMDLIN: Command to perform in subprocess
C-             FILEN: Input command file name (SYS$COMMAND for terminal)
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CMDLIN,FILEN
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C&IF VAXVMS
      INTEGER LIB$SPAWN,ISTAT,TRULEN
C----------------------------------------------------------------------
      CALL BROADC(.FALSE.)     !Turn off broadcast trapping
      IF(TRULEN(FILEN).EQ.0) THEN
        ISTAT=LIB$SPAWN(CMDLIN,,,)
      ELSE
        ISTAT=LIB$SPAWN(CMDLIN,FILEN,,)
      ENDIF
      LSPAWN=ISTAT
      CALL BROADC(.TRUE.)      !Turn on broadcast trapping again
C&ELSE
C&      CALL INTMSG('0SPAWN not implemented on this machine!'//CHAR(7))
C&      LSPAWN=1
C&ENDIF
      RETURN
      END
