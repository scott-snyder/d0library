      SUBROUTINE ECDRUN(IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Drop the current RDAT bank. This assumes that
C-     ECLSET has been called (eg by ECGET or D0DAD_ECMERGE) to setup 
C-     the protected links.
C-
C-   Inputs  :
C-   Outputs : IERR - 0 ==> No problems
C-   Controls:
C-
C-   Created   7-Mar-1995   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER IERR
C-----------------------------------------------------------------------
C
      IERR=0
      IF( LRDAT.LE.0 ) GOTO 999
C
      IQ(LECHD+JIRUN)=0
      IQ(LECHD+JNRUN)=0
      IQ(LECHD+JIEVT)=0
      IQ(LECHD+JNEVT)=0
      CALL MZDROP(IXDDAD,LRDAT,' ')
      LRDAT=0
C
  999 RETURN
      END
