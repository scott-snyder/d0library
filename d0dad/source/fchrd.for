      SUBROUTINE FCHRD(ILUN,IFILE,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read the header from a file catalog
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      CHARACTER*8 CSTR
      INTEGER ILUN,IFILE,IERR
      INTEGER IREC(JRECFC)
      REAL    QREC(JRECFC),VER
      EQUIVALENCE(IREC,QREC)
C----------------------------------------------------------------------
C
      READ(ILUN,REC=1,ERR=997) IREC
      CALL UHTOC(IREC,4,CLINE,JNCHFC)
      READ(CLINE,1102,ERR=998) CSTR,VER,IFILE,CFCTAG
 1102 FORMAT(A8,'        ',F8.2,'    ',I8,'    ',A20)
      CALL D0DAD_CPAD(CFCTAG)
C
      IF( LFCHD.LE.0 ) GOTO 999
      CALL UCOPY(IREC,IQ(LFCHD+NDFC+1),JRECFC)
C
  999 CONTINUE
      IERR=0
      RETURN
C
 997  CONTINUE
      IERR = -1
      RETURN
C
 998  CONTINUE
      IERR = -2
      RETURN
      END
