      LOGICAL FUNCTION BOKFDC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book all FDC detector Histograms
C-                           Use Histogram #'s 9000-9999
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
      BOKFDC = .TRUE.
      IF ( DFDC .LT. 2 ) GOTO 999
C
  999 RETURN
      END
