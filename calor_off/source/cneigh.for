      FUNCTION CNEIGH(ETAI,PHII,LYRI,ETAJ,PHIJ,LYRJ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Determines if Two cells are neighbor or not
C-
C-   Returned value  : .true. or .false.
C-   Inputs  : ETAI,PHII,LYRI,ETAJ,PHIJ,LYRJ
C-             are the Physics indices of the two cells
C-   Outputs : CNEIGH
C-   Controls:
C-
C-   Created   6-MAY-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CNEIGH
      INTEGER ETAI,PHII,LYRI,ETAJ,PHIJ,LYRJ,DELETA,DELPHI,DELLYR
      REAL ETAIM,ETAJM,SIGN
C----------------------------------------------------------------------
      CNEIGH = .FALSE.
C
      ETAIM=ETAI-SIGN(0.5,FLOAT(ETAI))         ! Solve the displaced zero
      ETAJM=ETAJ-SIGN(0.5,FLOAT(ETAJ))
C
      IF(ABS(ETAIM-ETAJM).LE.1.0)THEN      ! crude algorithm for time being
        IF((IABS(PHII-PHIJ).LE.1).OR.IABS(PHII-PHIJ).EQ.63)THEN
          CNEIGH = .TRUE.
        ENDIF
      ENDIF
  999 RETURN
      END
