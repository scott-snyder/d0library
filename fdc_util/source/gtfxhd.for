      SUBROUTINE GTFXHD(HALF,UNIT,QUAD,SECTOR,WIRE,NHIT,HITDIS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the drift distances for a given 
C-                         FDC wire for up to ten hits.
C-
C-   Inputs  : HALF,UNIT,QUAD,SECTOR,WIRE
C-   Outputs : NHIT,HITDIS
C-
C-   Created  15-MAY-1990   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,NHIT
      INTEGER LBANK,LHIT0,LHIT,IHIT,J
      INTEGER GZFTSC,GZFPSC
C
      REAL    HITDIS(MX_HIT_WIRE,0:1)
C----------------------------------------------------------------------
      NHIT=0
      CALL VZERO(HITDIS,MX_HIT_WIRE)
C
C  Get link to hit bank
C
      IF(UNIT.LE.0) THEN
        LBANK=GZFTSC(HALF,QUAD,SECTOR)
      ELSE
        LBANK=GZFPSC(HALF,SECTOR)
      ENDIF
      IF(LBANK.LE.5) GOTO 999
C
C  Get number of hits on specified wire
C
      NHIT=IQ(LBANK+4+WIRE)
      IF(NHIT.LE.0) GOTO 999
      LHIT0=LBANK+IQ(LBANK+4+WIRE+IQ(LBANK+2))
C
C  Get positive and negative drift distances (must include mirror image)
C
      DO 10 IHIT=1,NHIT
        J=LHIT0+(IQ(LBANK+3)*(IHIT-1))-1
        HITDIS(IHIT,0)=Q(J+2)
        HITDIS(IHIT,1)=Q(J+3)
   10 CONTINUE
C----------------------------------------------------------------------
  999 RETURN
      END
