      SUBROUTINE FSECT_CRATE(HALF,UNIT,QUAD,SECTOR,CRATE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given HALF,UNIT,QUAD,SECTOR, return
C-                      FADC crate number.
C-
C-   Inputs  : HALF,UNIT,QUAD,SECTOR
C-   Outputs : CRATE
C-
C-   Created  19-AUG-1991   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Input:
      INTEGER HALF,UNIT,QUAD,SECTOR
C  Output:
      INTEGER CRATE
C  Local:
      INTEGER CRATE_PHI(0:3,0:1)
      INTEGER CRATE_THETA(0:7,0:1) 
      DATA CRATE_PHI    / 55, 35, 25, 45,
     &                   105, 85, 95,115 / 
      DATA CRATE_THETA  / 15,  5,  5, 15, 15,  5,  5, 15,
     &                    75, 65, 65, 75, 75, 75, 65, 65 /

C----------------------------------------------------------------------
      IF ( UNIT .EQ. 0  ) THEN
        CRATE = CRATE_THETA(QUAD,HALF)
      ELSE
        CRATE = CRATE_PHI(SECTOR/9,HALF)
      ENDIF
  999 RETURN
      END
