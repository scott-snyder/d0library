      LOGICAL FUNCTION BOKCDC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book all CDC detector Histograms
C-                           Use Histogram #'s 8000-8999
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  23-JUL-1987   A.M.Jonckheere
C-
C-   Updated   9-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      BOKCDC = .TRUE.
      IF ( DCDC .LT. 2 ) GOTO 999
C
  999 RETURN
      END
