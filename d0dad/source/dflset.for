      SUBROUTINE DFLSET(ILUN,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Set up the protected links for the d0dad file
C-     on ILUN.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-Jan-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER ILUN,IERR
      INTEGER LZFIND
      EXTERNAL LZFIND
C-----------------------------------------------------------------------
C
      IERR=0
      IF( ILUN.NE.IQ(LDFHD+JLUN) ) THEN
         LDFHD=LZFIND(IXDDAD,LQ(LDADH-JFDF),ILUN,JLUN)
         IF( LDFHD.EQ.0 ) THEN
           IERR = -1
           GOTO 999
         ENDIF
      ENDIF
C
  999 RETURN
      END
