      LOGICAL FUNCTION MENVTX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-            Menu  for VTX package.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: D0COM.INC
C-
C-   Created   5-JUN-1989   Harrison B. Prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0COM.INC'        ! Selected MENU id and command
C----------------------------------------------------------------------
      MENVTX = .TRUE.
      IF ( NMENU .NE. IDVTX ) GOTO 999
C
  999 RETURN
      END
