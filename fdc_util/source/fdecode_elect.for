      SUBROUTINE FDECODE_ELECT( CRATE,CARD,CHANNEL,
     &                          HALF,UNIT,QUAD,SECTOR,WIRE,UBIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Give mapping from FDC electronics coordinates
C-            to logical coordinates.
C-            Given FADC CRATE (5-115), CARD (0-15) and CHANNEL (0-15),
C-            return HALF,UNIT,QUAD,SECTOR,WIRE, UBIT
C-                      
C-   Inputs  : CRATE,CARD,CHANNEL
C-   Outputs : HALF,UNIT,QUAD,SECTOR,WIRE,UBIT
C-
C-   Created  19-SEP-1991   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Input:
      INTEGER CRATE,CARD,CHANNEL
C  Output:
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UBIT
C  Local:
      INTEGER CRT, CONFIG 
      INTEGER SECTOR_DL(0:15)
      INTEGER WIRE_FCELL(0:15)
      INTEGER WIRE_HCELL(0:15)
      INTEGER SECTOR_PHI(7:15,2:5,0:1)
      INTEGER QUAD_THETA(0:15,0:1,0:1)
      INTEGER CONF_THETA(0:15,0:1,0:1)
C
      DATA SECTOR_PHI / 18,19,20,21,22,23,24,25,26,
     &                   9,10,11,12,13,14,15,16,17,
     &                  35,34,33,32,31,30,29,28,27,
     &                   8, 7, 6, 5, 4, 3, 2, 1, 0,
     &                   9,10,11,12,13,14,15,16,17,
     &                  18,19,20,21,22,23,24,25,26,
     &                   8, 7, 6, 5, 4, 3, 2, 1, 0,
     &                  35,34,33,32,31,30,29,28,27 /
      DATA QUAD_THETA / 
     &  1,1,1,2,2,2,5,5,5,6,6,6,6,5,2,1,
     &  0,0,0,3,3,3,4,4,4,7,7,7,7,4,3,0,
     &  1,1,1,2,2,2,6,6,6,7,7,7,7,6,2,1,
     &  0,0,0,3,3,3,5,5,5,4,4,4,4,5,3,0 /
      DATA CONF_THETA / 
     &  2,1,0,2,1,0,0,1,2,0,1,2,3,3,3,3,
     &  0,1,2,0,1,2,2,1,0,2,1,0,3,3,3,3,
     &  0,1,2,0,1,2,2,1,0,2,1,0,3,3,3,3,
     &  2,1,0,2,1,0,0,1,2,0,1,2,3,3,3,3 /
      DATA SECTOR_DL
     &          /6,5,4,3,2,1,0,7,7,0,1,2,3,4,5,6/
      DATA WIRE_HCELL
     &          /0,1,2,3,4,5,6,7,7,6,5,4,3,2,1,0/
      DATA WIRE_FCELL
     &          /0,1,3,5,7,6,4,2,0,1,3,5,7,6,4,2/
C----------------------------------------------------------------------
      IF (CRATE .GT. 115) GOTO 900
      IF (CARD .GT. 15) GOTO 900
      IF (CHANNEL .GT. 15) GOTO 900
C
      HALF = CRATE/60
      UBIT = 0
      CRT = MOD( CRATE/10, 6)
      IF ( MOD(CRT,6) .LE.1 ) THEN
        UNIT = 0
        QUAD = QUAD_THETA(CARD,CRT,HALF)
        CONFIG = CONF_THETA(CARD,CRT,HALF)
        IF ( CONFIG .LT.3 ) THEN
          SECTOR = 2*CONFIG + (1 - CHANNEL/8)
          IF ( SECTOR.LE.2 ) THEN
            WIRE = WIRE_HCELL(CHANNEL)
          ELSE
            WIRE = WIRE_FCELL(CHANNEL)
          ENDIF
        ELSE
          SECTOR = SECTOR_DL(CHANNEL)
          WIRE = 8 + CHANNEL/8
          IF (SECTOR.GE.6) THEN
            UBIT = 1
          ENDIF
        ENDIF
      ELSE
        IF (CARD.LT. 7) GOTO 900
        UNIT = 1
        QUAD = 0
        SECTOR = SECTOR_PHI(CARD,CRT,HALF)
        WIRE = CHANNEL
      ENDIF
      GOTO 999
C
  900 CONTINUE                         
      HALF = -1
      QUAD = -1
      SECTOR = -1
      WIRE = -1
C
  999 RETURN
      END
