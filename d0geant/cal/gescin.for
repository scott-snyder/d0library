      SUBROUTINE GESCIN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C      Geometry for the scintillator rings to correct for
C      energy lost in the cryostat walls.
C      The rings cover 0.1 units in eta and 2*pi/64 radians
C      in phi.  The name of the ring contains 10 times the
C      eta value of the lower eta edge of the tower.
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  16-JAN-1989   Chip Stewart
C    BASED ON CODE  by Z. Wolf, Mar 1988
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER  IZ,IETA
      CHARACTER SCNARR*32, DIVARR*32,ZEE(2)*1
      DATA ZEE/'+','-'/
C----------------------------------------------------------------------
C
C--   FIRST SET UP SCINTILLATOR MATERIAL AND TRACKING PARAMETERS
C
      CALL MXCAL('ICD_MIXTURES')    !Set up ICD Mixtures
C
C
C ****  LOOP OVER ICD CONES
C
      DO 100 IZ = 1, 2
        DO 100 IETA = 9, 14
          WRITE(SCNARR,110)IETA,ZEE(IZ)
          WRITE(DIVARR,112)IETA,ZEE(IZ)
          CALL VOLPOS(SCNARR)
          CALL VOLPOS(DIVARR)
  100 CONTINUE
  110 FORMAT('ICD_ETA_',I2.2,A1)
  112 FORMAT('ICD_DIV_',I2.2,A1)
      RETURN
      END
