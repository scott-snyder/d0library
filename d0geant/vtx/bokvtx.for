      LOGICAL FUNCTION BOKVTX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book all VTX detector Histograms
C-                           Use Histogram #'s 6000-6999
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  23-JUL-1987   A.M.Jonckheere
C-   Updated  14-JUL-1989   Harrison B. Prosper  
C-   Made into pbd logical function 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      BOKVTX = .TRUE. 
      IF ( DVTX .LT. 2 ) GOTO 999
C
  999 RETURN
      END
