      SUBROUTINE BKCCPH(LSCAL2,LCCPH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CCPH
C-
C-   Inputs  : LSCAL1 = Link of parent bank.
C-                      = 0, will find it for you.
C-   Outputs : Link of Booked CCPH Bank
C-   Controls: None
C-
C-   Created   5-JUL-1990 17:59:21.21  Jan Guida, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCCPH
      INTEGER IXIO
      INTEGER GZSCAL,LSCAL1,LSCAL2
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZCCPH.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LCCPH = 0
      LSCAL1 = LSCAL2
      IF(FIRST)THEN
C
        CALL MZFORM('CCPH','6I',IXIO)        ! Describe Bank format
        FIRST = .FALSE.
C
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LSCAL1.EQ.0 ) THEN
        LSCAL1 = GZSCAL ('STPC')
        IF ( LSCAL1.EQ.0 ) THEN
          CALL BKSCAL('STPC',LSCAL1)
        ENDIF
      ENDIF
C
      CALL MZBOOK
     &  (IDVSTP,LCCPH,LSCAL1,-IZCCPH,'CCPH',4,4,6,IXIO,0)
C
      IC(LCCPH+1) = 1               ! Bank version
  999 RETURN
      END
