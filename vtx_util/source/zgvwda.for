      SUBROUTINE ZGVWDA( LAYER, SECTOR, NBADC, PTHITS, NBHITS, LHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract pointer and number of hits in VWDA
C-                         for LAYER and SECTOR datas
C-
C-   Inputs  : LAYER, SECTOR = requested sector
C-   Outputs : NBADC             = number of FADC in this sector
C-             PTHITS(0:NBADC-1) = pointer on first hit on this FADC relative
C-             to the beginning of the bank
C-             NBHITS(0:NBADC-1) = number of hits on this FADC
C-             LHIT              = number of words per hits. 0 if no hits
C-
C-   Created   4-OCT-1988   Tom Trippe            
C-   Updated  17-OCT-1988   Ghita Rahal-Callot  : added -1 for the definition
C-   of the pointers to be compatible with CDC 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'
      INTEGER LAYER, SECTOR, NBADC, PTHITS(0:*), NBHITS(0:*), LHIT
      INTEGER KPVWDA, IFADC
C----------------------------------------------------------------------
      KPVWDA = LVWDA( SECTOR, LAYER )
      NBADC  = IQ( LVTXH + 7 )
      LHIT   = IQ( LVTXH + 6 )
      IF (KPVWDA .NE. 0) THEN
        NBADC = IQ( KPVWDA + 2 )
        DO 6 IFADC = 0 , NBADC - 1
          NBHITS (IFADC) = IQ (KPVWDA+IFADC+4)
          PTHITS (IFADC) = IQ(KPVWDA+NBADC+4+IFADC) - 1
    6   CONTINUE
      ELSE
        LHIT = 0
        CALL VZERO( NBHITS(0), NBADC )
      ENDIF
      RETURN
      END
