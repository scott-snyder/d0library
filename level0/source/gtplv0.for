      SUBROUTINE GTPLV0(ICONT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the data words from the PLV0 bank.
C-
C-   Inputs  : none
C-   Outputs : ICONT(20)
C-   Controls: none
C-
C-   Created  20-JUL-1992   Jeffrey Bantly
C-   Updated   8-JUL-1993   Jeffrey Bantly  copy all 20 words
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER ICONT(20),IWORD,LKPLV0
      INTEGER GZPLV0
      EXTERNAL GZPLV0
C-----------------------------------------------------------------
      LKPLV0=GZPLV0()
      IF(LKPLV0.LE. 0) THEN
        CALL VFILL(ICONT,20,-1)
      ELSE
        DO 100 IWORD=1,20
          ICONT(IWORD)=IQ(LKPLV0+IWORD)
  100   CONTINUE
      ENDIF
C-----------------------------------------------------------------
  999 RETURN
      END
