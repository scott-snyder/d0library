      SUBROUTINE ZGDCDA( LAYER, SECTOR, NBADC, PTHITS, NBHITS, LHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract pointer and number of hits in DCDA
C-                         for LAYER and SECTOR datas
C-
C-   Inputs  : LAYER, SECTOR = requested cell
C-   Outputs : NBADC             = number of FADC in this cell
C-             PTHITS(0:NBADC-1) = pointer on first hit on this FADC
C-             NBHITS(0:NBADC-1) = number of hits on this FADC
C-             LHIT              = number of words per hits. 0 if no hits
C-
C-   Updated  11-FEB-1988   Ghita Rahal-Callot   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INTEGER LAYER, SECTOR, NBADC, PTHITS(0:*), NBHITS(0:*), LHIT
      INTEGER KPDCDA, WIRE
C----------------------------------------------------------------------
      KPDCDA = LDCDA( SECTOR, LAYER )
      NBADC  = IQ( LCDCH + 5 )
      LHIT   = IQ( LCDCH + 4 )
      IF (KPDCDA .NE. 0) THEN
        NBADC = IQ( KPDCDA +2 )
        DO 6 WIRE = 0 , NBADC - 1
          NBHITS (WIRE) = IQ (KPDCDA+WIRE+4)
          PTHITS (WIRE) = KPDCDA + IQ(KPDCDA+NBADC+4+WIRE)
    6   CONTINUE
      ELSE
        LHIT = 0
        CALL VZERO( NBHITS(0), NBADC )
      ENDIF
      RETURN
      END
