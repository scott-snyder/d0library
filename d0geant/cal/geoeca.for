      SUBROUTINE GEOECA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Steering Routine for End Cap Geometry
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  14-NOV-1985   Rajendran Raja
C-   Updated  13-SEP-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
C  units are centimeters
C  rectangular, right handed coordinate system with beam along z and y  up.

      IMPLICIT NONE
C
C----------------------------------------------------------------------
      CALL GEECEL          !Set up EC ELectromagnetic section
      CALL GEECIP          !Set up EC Insert Plug
      CALL GEECMH          !Set up EC Middle Hadronic section
      CALL GEECOH          !Set up EC Outer (Coarse) Hadronic section
C
      END
