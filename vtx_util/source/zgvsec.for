      SUBROUTINE ZGVSEC ( LAYER, SECTOR, NBWIR, PTHITS, NBHITS, LHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract pointer and number of hits in VSEC
C-                         for LAYER and SECTOR datas
C-
C-   Inputs  : LAYER, SECTOR = requested cell
C-   Outputs : NBWIR             = number of wires in the sector
C-             PTHITS(0:NBWIR-1) = pointer on first hit on this wire
C-             NBHITS(0:NBWIR-1) = number of hits on this wire
C-             LHIT              = number of words per hits. 0 if no hits
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-SEP-1988   Ghita Rahal-Callot : adapted from ZGDSEC routine
C-   Updated  17-OCT-1988   Ghita Rahal-Callot : Added -1 to the pointer to
C-   correct from the difference between the pointers in VSEC and DSEC
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'
      INTEGER LAYER, SECTOR, PTHITS(0:*), NBHITS(0:*), LHIT
      INTEGER KPVSEC, WIRE, NBWIR,GZVSEC
C----------------------------------------------------------------------
      LVSEC(SECTOR,LAYER)=GZVSEC(LAYER,SECTOR)
      KPVSEC = LVSEC( SECTOR, LAYER )
      IF (KPVSEC .NE. 0) THEN
        LHIT   = IQ( KPVSEC + 3 )
        NBWIR  = IQ( KPVSEC +2 )
        DO 6 WIRE = 0 , NBWIR - 1
          NBHITS (WIRE) = IQ (KPVSEC+WIRE+4)
          PTHITS (WIRE) = KPVSEC + IQ(KPVSEC+NBWIR+4+WIRE) - 1
    6   CONTINUE
      ELSE
        LHIT = 0
        CALL VZERO( NBHITS(0), NBWIR )
      ENDIF
  999 RETURN
      END
