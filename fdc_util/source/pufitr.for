      SUBROUTINE PUFITR
C-----------------------------------------------------------------------
C-   Purpose: Eliminates the unused portion of
C-            bank 'FITR' after it has been filled.
C- 
C-   Created  27-NOV-1991   Robert E. Avery
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER GZFITR, LKFITR
      INTEGER NEXCESS
C----------------------------------------------------------------------

      LKFITR=GZFITR()
      IF(LKFITR.GT.0) THEN
        NEXCESS = IQ(LKFITR-1) - IQ(LKFITR+1)*IQ(LKFITR+2) - 2
        IF ( NEXCESS .GT. 0 ) THEN
          CALL MZPUSH(IXCOM,LKFITR,0,-NEXCESS,'R')
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
