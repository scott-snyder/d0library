      SUBROUTINE ZHSCDD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : make histograms for the CD raw data banks
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  22-JUN-1992   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INTEGER ICDDN, LCDDN, HISTID
      REAL    LENGTH
C----------------------------------------------------------------------
C
      IF (LHEAD .LE. 0) GOTO 999
      DO 100 ICDDN = IZCDD1, IZCDD4
        LCDDN = LQ(LHEAD - ICDDN)
        LENGTH = IQ(LCDDN -1)
        HISTID = 32 + ICDDN - IZCDD1
        CALL HF1(HISTID,LENGTH,1.)
  100 CONTINUE
C
  999 RETURN
      END
