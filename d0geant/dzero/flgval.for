      LOGICAL FUNCTION FLGVAL(text)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy version - always returns .TRUE.
C-
C-   Returned value  : .TRUE.
C-   Inputs  : TEXT - PBD package name
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-OCT-1989   A.M.Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*40 text
C----------------------------------------------------------------------
      FLGVAL = .TRUE.
C ****  Check if pause has been requested
C&IF VAXELN
C&      CALL WAIT_NOPAUSE
C&ENDIF
  999 RETURN
      END
