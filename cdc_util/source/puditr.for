      SUBROUTINE PUDITR
C-----------------------------------------------------------------------
C-   Purpose: Eliminates the unused portion of
C-            bank 'DITR' after it has been filled.
C- 
C-   Created  3-MAY-1994  Norman A. Graf (copied from PUFITR)
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER GZDITR, LKDITR
      INTEGER NEXCESS
C----------------------------------------------------------------------
C
      LKDITR=GZDITR()
      IF(LKDITR.GT.0) THEN
        NEXCESS = IQ(LKDITR-1) - IQ(LKDITR+1)*IQ(LKDITR+2) - 2
        IF ( NEXCESS .GT. 0 ) THEN
          CALL MZPUSH(IXCOM,LKDITR,0,-NEXCESS,'R')
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
