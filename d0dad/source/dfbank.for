      SUBROUTINE DFBANK(IERR)
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
      INTEGER IERR,NDF,NWORDS
C-----------------------------------------------------------------------
C
      NWORDS=NDDF+JDFHED+JPRECDF
      CALL MZBOOK(IXDDAD,LDFHD,LDADH,-JFDF,'DFHD',0,0,NWORDS,2,0)
      IF( LDFHD.GT.0 ) THEN
         IERR=0
         NDF=IQ(LDADH+JFDF)
         NDF=NDF+1
         IQ(LDADH+JFDF)=NDF
      ELSE
         IERR = -1
      ENDIF
C
  999 RETURN
      END
