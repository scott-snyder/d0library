      SUBROUTINE FCHWRT(ILUN,IFILE,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write a header to a file catalog.
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
      INTEGER ILUN,IFILE,IERR
      CHARACTER*8 CSTR,CRLF*4
      INTEGER IREC(JRECFC),I
      REAL    QREC(JRECFC)
      EQUIVALENCE(IREC,QREC)
C----------------------------------------------------------------------
      CRLF=CHAR(13)//CHAR(10)//'  '
C
      CALL D0DAD_CPAD(CFCTAG)
      WRITE(CLINE,1102) D0DADBASE,'FC',
     +  100*VER_D0DAD_MAJOR+VER_D0DAD_MINOR,IFILE,CFCTAG,
     +  NFCFN,NFCGN,NFCTAP,NFCCOM
 1102 FORMAT(A5,' ',A2,'        ',I8,'    ',I8,'    ',A20,4(2X,I6))
      CALL UCTOH(CLINE,IREC,4,JNCHFC)
      CALL UCTOH(CRLF,IREC(JRECFC),4,4)
      WRITE(ILUN,REC=1,ERR=998) IREC
C
      IF( LFCHD.LE.0 ) GOTO 999
      CALL UCOPY(IREC,IQ(LFCHD+NDFC+1),JRECFC)
C
  999 CONTINUE
      IERR=0
      RETURN
C
 998  CONTINUE
      IERR = -1
      RETURN
      END
