      LOGICAL FUNCTION BOKSAM ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book all SAMUS histograms
C-                         Use histograms #'s 10000-10999
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  20-SEP-1990   A.Kiryunin
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      BOKSAM  = .TRUE.
      IF ( DSAM.LT.2 ) GOTO 999
C
  999 RETURN
      END
