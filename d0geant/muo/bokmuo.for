      LOGICAL FUNCTION BOKMUO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book all Muon Histograms here
C-                           Use Histogram #'s 2000-2999
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  23-JUL-1987   A.M.Jonckkhere
C-
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      BOKMUO  = .TRUE.
      IF ( DMUO.LT.2 ) GOTO 999
  999 RETURN
      END
