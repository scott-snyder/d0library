      INTEGER FUNCTION SPAWIT(CMDLIN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Spawn subprocess with output saved in special 
C-                         FILSAV file. Put output back on screen with 
C-                         REASAV.  VAX-specific.
C-
C-   Inputs  : CMDLINE: VMS command to perform
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CMDLIN
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
C&IF VAXVMS
      INTEGER LIB$SPAWN,TRULEN,ISTAT
C----------------------------------------------------------------------
      CALL BROADC(.FALSE.)           !Turn off broadcast trapping
      ISTAT=LIB$SPAWN(CMDLIN,,FILSAV,)
      IF(.NOT.ISTAT) THEN
        CALL MSGSCR(ISTAT,'SPAWN failed-->')
      ELSE
        CALL REASAV
      ENDIF
      CALL BROADC(.TRUE.)            !Turn on broadcast trapping again
      SPAWIT=ISTAT
C&ELSE
C&      CALL INTMSG('0SPAWN not implemented on this machine!'//CHAR(7))
C&      SPAWIT=1
C&ENDIF
      RETURN
      END
