      SUBROUTINE ZGFPDA( HALF,SECTOR,NBFADC,PTHITS,NBHITS,LHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract pointer and number of hits in FPDA
C-                         for HALF and SECTOR datas
C-
C-   Inputs  : HALF, SECTOR  = requested cell
C-   Outputs : NBFADC             = number of FADC in this cell
C-             PTHITS(0:NBFADC-1) = pointer on first hit on this FADC,
C-                                  gives the value J as in FPDA.ZEB
C-             NBHITS(0:NBFADC-1) = number of hits on this FADC
C-             LHIT               = number of words per hits. 0 if no hits
C-
C-   Updated  11-FEB-1988   Ghita Rahal-Callot   
C-   Updated  10-OCT-1988   Jeffrey Bantly  modified for new hit format 
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER HALF, SECTOR
      INTEGER NBFADC, PTHITS(0:*), NBHITS(0:*), LHIT
      INTEGER LKFPDA, WIRE
      INTEGER GZFPDA
C----------------------------------------------------------------------
      LKFPDA = GZFPDA(HALF,SECTOR)
      NBFADC = 0
      LHIT   = 0
      CALL VZERO( NBHITS(0), NBFADC )
      IF (LKFPDA .NE. 0) THEN
        NBFADC = IQ( LKFPDA + 2 )
        LHIT   = IQ( LKFPDA + 3 )
        DO 6 WIRE = 0 , NBFADC - 1
          NBHITS (WIRE) = IQ (LKFPDA+WIRE+4)
          PTHITS (WIRE) = LKFPDA + IQ(LKFPDA+NBFADC+4+WIRE) - 1
    6   CONTINUE
      ENDIF
C----------------------------------------------------------------------
      RETURN
      END
