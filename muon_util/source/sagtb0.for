C+
      SUBROUTINE SAGTB0 (STATION, RTUBE, VTUBE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Transform tube parameters from station
C-                         coordinates system into global D0 system.
C-
C-   Inputs  : STATION - station numbes,
C-             RTUBE(3) - vectors coordinates of tubes center from
C-                        center of station
C-             VTUBE(3) - tubes axis vectors, |VTUBE| = 1
C-   Outputs : RTUBE(3), VTUBE(3)
C-   Controls: none.
C-
C-   Created  19-NOV-1992   Alexander Efimov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER STATION
      REAL    RTUBE(3), VTUBE(3)
      REAL    ST_CENTER(3), ST_ANGLES(3), ST_SIZE(3), ST_HOLE(3)
      REAL*8  LOCK(3), SYS(6), GLOB(3)
      INTEGER J
C
      CALL SAGSTA (STATION, ST_CENTER, ST_ANGLES, ST_SIZE, ST_HOLE)
      DO J = 1, 3
        SYS(J) = ST_CENTER(J)
        SYS(J+3) = ST_ANGLES(J)
        LOCK(J) = RTUBE(J)
      END DO
      CALL SAGLSY (LOCK, SYS, GLOB)
      DO J = 1, 3
        RTUBE(J) = GLOB(J)
        LOCK(J) = VTUBE(J)
        SYS(J) = 0.0
      END DO
      CALL SAGLSY (LOCK, SYS, GLOB)
      DO J = 1, 3
        VTUBE(J) = GLOB(J)
      END DO
C
      RETURN
      END
