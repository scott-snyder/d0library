      SUBROUTINE SKF_VERXYZ_GEANT (IVER, VERTEX, NV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get vertex coordinates from the ISAE bank.
C-
C-   Inputs  : none.
C-   Outputs : VERTEX(3) - vertex coordinates.
C-   Controls: none.
C-
C-   Created  25-JAN-1994   Alexander Efimov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL VERTEX(3)
      INTEGER IVER, NV
      INTEGER GNPMX
      PARAMETER (GNPMX=100)
      INTEGER GNPART, GID(GNPMX), GID2(GNPMX)
      REAL    GX(GNPMX), GY(GNPMX), GZ(GNPMX)
      REAL    GPX(GNPMX), GPY(GNPMX), GPZ(GNPMX)
      REAL    GP(GNPMX), GPHI(GNPMX), GTHETA(GNPMX), GETA(GNPMX)
C
      CALL GISAMU (GNPART, GID, GID2, GPX, GPY, GPZ,
     &             GP, GPHI, GTHETA, GETA, GX, GY, GZ)
      IF( GNPART.EQ.0 ) THEN
        NV = -1
        IVER = -1
      ELSE
        VERTEX(1) = GX(1)
        VERTEX(2) = GY(1)
        VERTEX(3) = GZ(1)
        NV = 1
        IVER = 1
      END IF
C
      RETURN
      END
