      SUBROUTINE BKCNSH(LSCAL1,LCNSH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CNSH
C-
C-   Inputs  : LSCAL1 = Link of parent bank.
C-                      = 0, will find it for you.
C-   Outputs : Link of Booked CNSH Bank
C-   Controls: None
C-
C-   Created  31-JUL-1991    Jan Guida, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCNSH
      INTEGER LSCAL1
      INTEGER IXIO
      INTEGER GZSCAL
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZCNSH.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LCNSH = 0
      IF(FIRST)THEN
C
        CALL MZFORM('CNSH','6I',IXIO)        ! Describe Bank format
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
     &  (IDVSTP,LCNSH,LSCAL1,-IZCNSH,'CNSH',4,4,6,IXIO,0)
C
  999 RETURN
      END
