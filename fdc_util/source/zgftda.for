      SUBROUTINE ZGFTDA( HALF,QUAD,SECTOR,
     &                          NBFADC,PTHITS,NBHITS,LHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract pointer and number of hits in FTDA
C-                         for HALF, QUAD, and SECTOR datas
C-
C-   Inputs  : HALF, QUAD, SECTOR = requested cell
C-   Outputs : NBFADC             = number of FADC in this cell
C-             PTHITS(0:NBFADC-1) = pointer on first hit on this FADC,
C-                                  gives the value J as in FTDA.ZEB
C-             NBHITS(0:NBFADC-1) = number of hits on this FADC
C-             LHIT              = number of words per hits. 0 if no hits
C-
C-   Updated  11-FEB-1988   Ghita Rahal-Callot   
C-   Updated  10-OCT-1988   Jeffrey Bantly  modified for new hit format 
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER HALF, QUAD, SECTOR
      INTEGER NBFADC, PTHITS(0:*), NBHITS(0:*), LHIT
      INTEGER LKFTDA, WIRE
      INTEGER GZFTDA
C----------------------------------------------------------------------
      LKFTDA = GZFTDA( HALF, QUAD, SECTOR )
      NBFADC = 0
      LHIT   = 0
      CALL VZERO( NBHITS(0), NBFADC )
      IF (LKFTDA .NE. 0) THEN
        NBFADC = IQ( LKFTDA + 2 )
        LHIT   = IQ( LKFTDA + 3 )
        DO 6 WIRE = 0 , NBFADC - 1
          NBHITS (WIRE) = IQ (LKFTDA+WIRE+4)
          PTHITS (WIRE) = LKFTDA + IQ(LKFTDA+NBFADC+4+WIRE) - 1
    6   CONTINUE
      ENDIF
C---------------------------------------------------------------------
      RETURN
      END
