      SUBROUTINE FDRIFTDIR_TABLE(PHI_THETA,PHI_PHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize lookup table for FDRIFTDIR.
C-
C-   Outputs : PHI_THETA,PHI_PHI
C-
C-   Created   1-JUL-1991   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      REAL    PHI_THETA(0:MXSECT, 0:MXQUAD, 0:MXHALF)
      REAL    PHI_PHI(0:MXSECP, 0:MXHALF)
C
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE
      REAL    SDRIFT,CDRIFT,PHI
C
      REAL    XWIRE,YWIRE,ZWIRE
      REAL    XCENTER(0:1),YCENTER(0:1)
C----------------------------------------------------------------------
      DO HALF =  0, 1
C
C Find Center
C
        XCENTER(HALF) = 0
        YCENTER(HALF) = 0
        DO QUAD =  0, 3
          CALL GTFALH(
     &              HALF,0,QUAD,0,0,
     &              XWIRE,YWIRE,ZWIRE)
          XCENTER(HALF) = XCENTER(HALF) + XWIRE/4
          YCENTER(HALF) = YCENTER(HALF) + YWIRE/4
        ENDDO
C
C Theta
C
        UNIT=0
        DO QUAD =  0, MXQUAD 
          DO SECTOR =  0, MXSECT 
            CALL GTFALH(
     &              HALF,0,QUAD,SECTOR,0,
     &              XWIRE,YWIRE,ZWIRE)
C
            XWIRE = XWIRE - XCENTER(HALF) 
            YWIRE = YWIRE - YCENTER(HALF) 
            PHI = ATAN2(YWIRE,XWIRE)
            IF (SECTOR.EQ.1) PHI = PHI + PI
            PHI_THETA(SECTOR,QUAD,HALF) = PHI
          ENDDO
        ENDDO
C
C Phi
C
        UNIT=1
        DO SECTOR =  0, MXSECP
          CALL GTFALH(
     &              HALF,1,0,SECTOR,0,
     &              XWIRE,YWIRE,ZWIRE)
C
          XWIRE = XWIRE - XCENTER(HALF) 
          YWIRE = YWIRE - YCENTER(HALF) 
          PHI = ATAN2(YWIRE,XWIRE)
          PHI = PHI + PI/2. ! Drift direction normal to wire
          PHI_PHI(SECTOR,HALF) = PHI
        ENDDO
      ENDDO
C
  999 RETURN
      END
