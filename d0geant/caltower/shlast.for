      SUBROUTINE SHLAST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End routine for Shower Library in GEANT
C-
C-   Inputs  : ISUNIT and ISEED from /SHLCON/
C-   Outputs : none
C-
C-   Created  12-OCT-1987   John Womersley
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Character*72 MSG
C----------------------------------------------------------------------
C
      INCLUDE 'D0$INC:SHLCON.INC'
C
      CALL RZEND('SHOWER_LIBRARY')
      CLOSE(ISUNIT)
      CALL ERRMSG('ENDED','SHLAST',' Shower Library file closed','I')
      WRITE(MSG,1000)ISEED
 1000 Format(' Random number seed after last event is',I16)
      CALL ERRMSG('LASTRAN','SHLAST',MSG,'I')
C
      RETURN
      END
