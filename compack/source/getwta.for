      LOGICAL FUNCTION GETWTA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find out if running from a WORKSTATION or not.
C-                         Extremely VAX-specific.
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   Returns .TRUE. when running in a workstation VT220 window
C-
C-   Created   9-DEC-1987   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C&IF VAXVMS
      INTEGER DVI$_DEVNAM
      PARAMETER (DVI$_DEVNAM = 32)    ! Device name - STRING - 64 bytes
      INTEGER*2 IOS(4),ITMLST(8)
      INTEGER ITM(3),BUF(16),ISTAT,SYS$GETDVIW
      CHARACTER*64 CHRBUF
      EQUIVALENCE (ITM(1),ITMLST(3)),(CHRBUF,BUF)
C----------------------------------------------------------------------
      ITMLST(1)=4
      ITMLST(2)=DVI$_DEVNAM
      CHRBUF=' '
      ITM(1)=%LOC(BUF(1))
      ITM(2)=%LOC(BUF(2))
      ISTAT=SYS$GETDVIW(,,'SYS$COMMAND:',%REF(ITMLST),IOS,,,,)
      IF(ISTAT.NE.1) CALL MSGSCR(ISTAT,' GETWTA-->')
      IF(INDEX(CHRBUF,'WTA').GT.0) THEN
        GETWTA=.TRUE.
      ELSE
        GETWTA=.FALSE.
      ENDIF
C&ELSE
C&      GETWTA=.FALSE.
C&ENDIF
      RETURN
      END
