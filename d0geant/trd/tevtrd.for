      LOGICAL FUNCTION TEVTRD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called before each event by LUTREV.
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  23-JUL-1987   A.M.Jonckheere
C-   Updated   9-JUN-1989   Harrison B. Prosper  
C-   Made into a pbd interface function 
C-   Updated   7-DEC-1992   Robert E. Avery  Set STP version in Head bank. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      TEVTRD = .TRUE.
      IF ( DTRD .LT. 1 ) GOTO 999
C
C Set STP version in EVENT_HEAD bank. 
      CALL CD_HEAD_FLG('TRD')
C
  999 RETURN
      END
