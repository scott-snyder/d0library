      INTEGER FUNCTION GZGEAN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return Link to bank GEAN
C-
C-   Returned value  : Link to Zebra Bank GEAN
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   11-NOV-1986 Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C Main ZEBRA store
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZGEAN.LINK/LIST'
C----------------------------------------------------------------------
C
      GZGEAN = 0
      IF ( LHEAD.GT.0 ) THEN
        GZGEAN = LQ(LHEAD-IZGEAN)
      ENDIF
  999 RETURN
      END
