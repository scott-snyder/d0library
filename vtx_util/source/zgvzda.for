      SUBROUTINE ZGVZDA( LAYER, NBADC, PTHITS, NBHITS, LHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract pointer and number of hits in VZDA
C-                         for given LAYER
C-
C-   Inputs  : LAYER = requested layer
C-   Outputs : NBADC             = number of FADC in this sector
C-             PTHITS(0:NBADC-1) = pointer on first hit on this FADC relative
C-             to the beginning of the bank
C-             NBHITS(0:NBADC-1) = number of hits on this FADC
C-             LHIT              = number of words per hits. 0 if no hits
C-
C-   Created   4-OCT-1988   Tom Trippe            
C-   Updated  17-OCT-1988   Ghita Rahal-Callot  : added -1 for the definition
C-   of the pointers to be compatible with CDC
C-   Modified 13-DEC-1988   Peter Grudberg : created ZGVZDA from ZGVWDA
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVZST.LINK'
      INTEGER LAYER, NBADC, PTHITS(0:*), NBHITS(0:*), LHIT
      INTEGER KPVZDA, IFADC, LVZST, NBSTRP, NBEND, NWLAYR
C----------------------------------------------------------------------
      KPVZDA = LVZDA( LAYER )
      LVZST = LC( LVGEH - IZVZST )
      NWLAYR = IC( LVZST + 2 )
      NBSTRP = IC( LVZST + 3 + NWLAYR*LAYER )
      NBEND  = IC( LVZST + 4 + NWLAYR*LAYER )
      NBADC  = NBSTRP * NBEND
      IF (KPVZDA .NE. 0) THEN
        LHIT  = IQ( LVTXH + 9 )
        NBADC = IQ( KPVZDA + 2 )
        DO 6 IFADC = 0 , NBADC - 1
          NBHITS (IFADC) = IQ(KPVZDA+IFADC+4)
          PTHITS (IFADC) = IQ(KPVZDA+NBADC+4+IFADC) - 1
    6   CONTINUE
      ELSE
        LHIT = 0
        CALL VZERO( NBHITS(0), NBADC )
      ENDIF
      RETURN
      END
