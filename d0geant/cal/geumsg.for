      SUBROUTINE GEUMSG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Define and position the Massless Gaps in the CC
C-
C-   Inputs  : /UCMSGP/ all variables (entered in GUCDTA)
C-   Outputs : GEANT positioned volumes
C-
C-   Created  10-AUG-1987   A.M.Jonckheere
C-   Updated   8-OCT-1988   Rajendran Raja  New SRCP way 
C-   Updated   4-DEC-1988   Stuart Fuess  Changed SRCP names 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL VOLPOS('CC_SOUTH_MASSLESS_GAP_VOLUME')
      CALL VOLPOS('CC_NORTH_MASSLESS_GAP_VOLUME')
  999 RETURN
      END
