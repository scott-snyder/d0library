      SUBROUTINE BKCCPT(LCCPH,LCCPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CCPT
C-
C-   Inputs  : LCCPH = Link of parent bank.
C-                      = 0, will find it for you.
C-   Outputs : Link of Booked CCPT Bank
C-   Controls: None
C-
C-   Created  31-JUL-1991  Jan Guida, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCCPT
      INTEGER LCCPH
      INTEGER IXIO
      INTEGER GZCCPH
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZCCPT.LINK/LIST'
      INCLUDE 'D0$ZEB$SCAL:CCPT.ZEB/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LCCPT = 0
      IF(FIRST)THEN
C
        CALL MZFORM('CCPT','-I',IXIO)        ! Describe Bank format
        FIRST = .FALSE.
C
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LCCPH.EQ.0 ) THEN
        LCCPH = GZCCPH ()
        IF ( LCCPH.EQ.0 ) THEN
          CALL BKCCPH(0,LCCPH)
        ENDIF
      ENDIF
C
      CALL MZBOOK
     &  (IDVSTP,LCCPT,LCCPH,-IZCCPT,'CCPT',1,1,2311,IXIO,0)
C
      IC(LCCPT+1) = 1               ! Bank version
  999 RETURN
      END
