      SUBROUTINE BKTCOR(LTCOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank TCOR: tables to remove temperature
C-   correction for run1b/run1c
C-
C-   Inputs  : LTGEN  [I] Address of the parent bank.
C-                          = 0, will find it for you.
C-   Outputs : LTCOR  [I] Address of booked TCOR bank.
C-   Controls: None
C-
C-   Created  26-FEB-1996   A. ZYLBERSTEJN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTCOR
C----------------------------------------------------------------------
      INTEGER ND,NL,NS,IXIO
      INTEGER GZTHEN
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTCOR.LINK'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LTCOR = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('TCOR','-F',IXIO)        ! Describe Bank format
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LTGEN .LE. 0 ) THEN
        CALL ERRMSG('TGEN-NOT-FOUND','BKTCOR',
     &    'TGEN BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
      NL = 0
      NS = 0
      ND = 8500
      CALL MZBOOK(IXSTP,LTCOR,LTGEN,-IZTCOR,'TCOR',NL,NS,ND,IXIO,0)
C
  999 RETURN
      END
