      LOGICAL FUNCTION ANLCDC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyse results - called after each event
C-
C-   Inputs  : None 
C-   Outputs : None 
C-   Calledby: LUOUT
C-
C-   Created  24-JUL-1987   A.M.Jonckheere
C-
C-   Updated   9-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C-   Updated  10-SEP-1990   Qizhong Li-Demarteau   fix a mistyping 
C-                                                  DVTX -> DCDC
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      ANLCDC = .TRUE.
      IF ( DCDC .LT. 4 ) GOTO 999
C
  999 RETURN
      END
