      SUBROUTINE FCBANK(IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: 
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER IERR
C-----------------------------------------------------------------------
C
      CALL MZBOOK(IXDDAD,LFCHD,LDADH,-JFFC,'FCHD',0,0,NDFC+JRECFC,2,0)
      IF( LFCHD.GT.0 ) THEN
         IERR=0
         IQ(LDADH+JFFC)=IQ(LDADH+JFFC)+1
      ELSE
         IERR = -1
      ENDIF
C
  999 RETURN
      END
