C+
      INTEGER FUNCTION SAGSTA (STATION, CENTER, ANGLES, SIZE, HOLE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get SAMUS station geometry parameters
C-
C-   Inputs  : STATION number.
C-   Outputs : CENTER(3) - coordinates of station center,
C-             ANGLES(3) - Ailer angles of the station orientation,
C-             SIZE(3) - half sizes of the station,
C-             HOLE(3) - half sizes of the station hole.
C-   Controls: none.
C-
C-   Created  27-SEP-1990   Alexander Efimov
C-   Updated  30-APR-1991   Andrei Kiryunin: geometry from banks SSTH.
C-   Updated  12-NOV-1992   Alexander Efimov: add Ailer angles of the
C-                          SAMUS stations.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER STATION
      REAL    CENTER(3), ANGLES(3), SIZE(3), HOLE(3)
      INTEGER N_STATIONS
      PARAMETER (N_STATIONS=6)
      INTEGER LSSTA, GZSSTA
      EXTERNAL GZSSTA
C
      SAGSTA = -1
      IF (STATION .LT. 1 .OR. STATION .GT. N_STATIONS) GO TO 999
      LSSTA = GZSSTA (STATION)
      IF (LSSTA .EQ. 0) GOTO 999
      CALL UCOPY (C(LSSTA+10), CENTER, 3)
      CALL UCOPY (C(LSSTA+13), ANGLES, 3)
      CALL UCOPY (C(LSSTA+16),   SIZE, 3)
      CALL UCOPY (C(LSSTA+19),   HOLE, 3)
      SAGSTA = +1
C
  999 CONTINUE
      RETURN
      END
