      SUBROUTINE BKGEAN(LGEAN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book Zebra bank GEAN
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-FEB-1989   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZGEAN.LINK/LIST'
      INTEGER LGEAN
C----------------------------------------------------------------------
C
      LGEAN = LQ(LHEAD-IZGEAN)
      IF( LGEAN.LE.0 ) THEN
        CALL MZBOOK(IXMAIN,LGEAN,LHEAD,-IZGEAN,'GEAN',5,5,0,0,0)
      ENDIF
  999 RETURN
      END
