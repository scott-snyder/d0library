      SUBROUTINE GTFITR(ITRK,TRKDAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return FDC track data from FITR
C-
C-   Inputs  : ITRK - track number
C-   Outputs : TRKDAT - track data
C-
C-   Created  16-MAR-1989   Jeffrey Bantly
C-   Updated  19-MAR-1990   Jeffrey Bantly  cleanup 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:FDLTRK.INC'
      INTEGER ITRK, I, LBASE, DUM
      INTEGER GZFITR
C
      REAL    TRKDAT(9)
C----------------------------------------------------------------------
      IF ( LFITR .LE. 5 ) LFITR = GZFITR(DUM)
      IF ( LFITR .LE. 5 ) GO TO 999
      IF ( ITRK .GT. IQ(LFITR+1) ) GOTO 999
      LBASE = LFITR + 2 + IQ(LFITR+2)*(ITRK-1)
      DO 10 I = 1, IQ(LFITR+2)
        TRKDAT(I) = Q(LBASE+I)
   10 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
