      SUBROUTINE BKGHIT(LGHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book Zebra bank GHIT
C-
C-   Inputs  :
C-   Outputs : LGHIT  in /CALLNK/ - link to bank GHIT in /ZEBCOM/
C-   Controls:
C-
C-   Created   9-FEB-1989   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZGHIT.LINK/LIST'
      INTEGER LGEAN,LGHIT,GZGHIT
C----------------------------------------------------------------------
C
      LGHIT = GZGHIT()
      IF( LGHIT.LE.0 ) THEN
        CALL BKGEAN(LGEAN)
        CALL MZBOOK(IXMAIN,LGHIT,LGEAN,-IZGHIT,'GHIT',8,8,0,0,0)
      ENDIF
  999 RETURN
      END
