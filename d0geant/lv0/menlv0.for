      LOGICAL FUNCTION MENLV0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Handle LV0 Menu calls
C-                              Called by LUIGET
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   6-DEC-1988   A.M.Jonckheere
C-   Updated   6-JUN-1989   Harrison B. Prosper  
C-   Made into program-builder interface function 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0COM.INC'        ! Selected MENU id and command
C----------------------------------------------------------------------
      MENLV0 = .TRUE.
      IF ( NMENU .NE. IDLV0 ) GOTO 999
C
  999 RETURN
      END
