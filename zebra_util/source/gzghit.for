      INTEGER FUNCTION GZGHIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find link to Zebra Bank GHIT
C-
C-   Returned value  : Link to bank
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-FEB-1989   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZGHIT.LINK'
      INTEGER LGEAN,GZGEAN
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      GZGHIT = 0
      LGEAN = GZGEAN()
      IF ( LGEAN.GT.0 ) THEN
        IF ( IQ(LGEAN-2).EQ.3 ) THEN
          GZGHIT = LQ(LGEAN-3)          ! OLD VERSION
          IF(FIRST)THEN
            WRITE (6,*)'Using links for D0GEANT V2.3 and older'
            FIRST=.FALSE.
          ENDIF
        ELSE
          GZGHIT = LQ(LGEAN-IZGHIT)
          IF(FIRST)THEN
            WRITE (6,*)'Using links for D0GEANT V3.0 and newer'
            FIRST=.FALSE.
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
      END
