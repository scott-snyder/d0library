      SUBROUTINE DETCAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Steering routine for Calorimeter
C-
C-   Inputs  : /D0LOG/ switches
C-   Outputs : NONE
C-
C-   Created   8-JUL-1987   A.M.Jonckheere
C-   Updated  22-NOV-1988   Harrison B. Prosper
C-                          Put in call to select SRCP set-definition
C-                          bank SRCP_SETCAL.
C-   Updated  21-DEC-1988   Harrison B. Prosper
C-                          Set-definition now REST bank.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C----------------------------------------------------------------------
C
      CALL SLSRCP('SRCP_REST')          ! Select REST bank
      IF (DUCA.GE.2) CALL DETUCA
      IF (DECA.GE.2) CALL DETECA
      IF (DECA.GE.2) CALL DETSCN
      IF (DEAD.GE.2) CALL DETDED        ! All dead material in MCAL
      CALL RSSRCP                       ! Select previously selected bank
C
      END
