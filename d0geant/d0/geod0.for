      LOGICAL FUNCTION GEOD0()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GEANT3 Geomtry Definition of DZERO.
C-
C-   Units are centimeters.
C-
C-   RECTANGULAR, RIGHT HANDED COORDINATE SYSTEM WITH BEAM ALONG Z, Y UP AND
C-   DOWN, AND X IS TRANSVERSE.  Y POSITIVE IS UP, X POSITIVE IS TO THE LEFT
C-   LOOKING DOWNSTREAM.  Z = 0 IS FRONT FACE OF TARGET
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   5-JUN-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      GEOD0 = .TRUE.
C
C ****  Set up local Mother Volumes for D0
C
      CALL GEOMVL
C
  999 RETURN
      END
