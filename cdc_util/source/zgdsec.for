      SUBROUTINE ZGDSEC( LAYER, SECTOR, NBWIR, PTHITS, NBHITS, LHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract pointer and number of hits in DSEC
C-                         for LAYER and SECTOR datas
C-
C-   Inputs  : LAYER, SECTOR = requested cell
C-   Outputs : NBWIR             = number of wires in the sector
C-             PTHITS(0:NBWIR-1) = pointer on first hit on this wire
C-             NBHITS(0:NBWIR-1) = number of hits on this wire
C-             LHIT              = number of words per hits. 0 if no hits
C-
C-   Updated  10-AUG-1987   Olivier Callot
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INTEGER LAYER, SECTOR, PTHITS(0:6), NBHITS(0:6), LHIT
      INTEGER KPDSEC, WIRE, NBWIR,GZCDCH,GZDSEC
C----------------------------------------------------------------------
C
      IF(LCDCH.EQ.0) LCDCH=GZCDCH()
C
      IF(LCDCH.EQ.0) THEN
        LHIT=0
        RETURN
      ENDIF
C
      IF(LDSEC(SECTOR,LAYER).EQ.0) 
     &  LDSEC(SECTOR,LAYER)=GZDSEC(SECTOR,LAYER)
      IF(LDSEC(SECTOR,LAYER).EQ.0) THEN
        LHIT=0
        RETURN
      ENDIF
C
      KPDSEC = LDSEC( SECTOR, LAYER )
      NBWIR  = IQ( LCDCH + 3 )
      LHIT   = IQ( LCDCH + 2 )
      IF (KPDSEC .NE. 0) THEN
        NBWIR = IQ( KPDSEC +2 )
        DO 6 WIRE = 0 , NBWIR - 1
          NBHITS (WIRE) = IQ (KPDSEC+WIRE+4)
          PTHITS (WIRE) = KPDSEC + IQ(KPDSEC+NBWIR+4+WIRE)
    6   CONTINUE
      ELSE
        LHIT = 0
        CALL VZERO( NBHITS(0), NBWIR )
      ENDIF
      RETURN
      END
