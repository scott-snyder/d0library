      SUBROUTINE BKRECB(LRECB)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank RECB
C-
C-   Inputs  : none
C-   Outputs : Link of Booked RECB Bank
C-   Controls: None
C-
C-   Created  26-JUL-1990 10:27:01.49  Chip Stewart
C-   Updated   5-JUL-1991              Scott Snyder
C-    supply runno to bkheadr()
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LRECB
      INTEGER IXIO
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZRECB.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER RUNNO
      EXTERNAL RUNNO
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LRECB = 0
      IF(FIRST)THEN
C
        CALL MZFORM('RECB','-I',IXIO)        ! Describe Bank format
C
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF (LHEADR.EQ.0) THEN
        CALL BKHEADR (RUNNO())
      ENDIF
      IF (LHEADR.EQ.0) THEN
        CALL ERRMSG('NO-HEADR-BANK','BKRECB',
     &    'Can NOT book begin run header bank','W')
        GOTO 999
      ENDIF
C
      CALL MZBOOK
     &  (IXDVR,LRECB,LHEADR,-IZRECB,'RECB',3,3,10,IXIO,0)
C
  999 RETURN
      END
