      SUBROUTINE DFHWRT(ILUN,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write a header to a d0dad file.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  14-NOV-1993   John D Hobbs
C-   Modified 16-MAR-1995   JDH Move to fortran direct access I/O
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER ILUN,IERR
      CHARACTER CSTR*8
      INTEGER IREC(JDFHED),I,J,NWRITE
C----------------------------------------------------------------------
C
C  Setup header commons
C
      WRITE(CSTR,1101) D0DADBASE,'DF'
 1101 FORMAT(A5,' ',A2)
      CALL UCTOH(CSTR,IREC,4,8)
      IREC(3)=100*VER_D0DAD_MAJOR + VER_D0DAD_MINOR
      CALL D0DAD_CPAD(CDFTAG)
      CALL UCTOH(CDFTAG,IREC(4),4,20)
      IREC(9)=JDFHED
      IREC(10)=JRECDF
      IREC(11)=TESTBSWAP
      IREC(12)=0
      IREC(13)=JPRECDF
C
C  Keep a copy in the Zebra bank for this file.
C
      IF( LDFHD.LE.0 ) GOTO 997
      CALL UCOPY(IREC,IQ(LDFHD+NDDF+1),JDFHED)
C
C  Write the header
C
      CALL VZERO(DF_IOREC,JPRECDF)
      CALL UCOPY(IQ(LDFHD+NDDF+1),DF_IOREC,JPRECDF)
      WRITE(ILUN,REC=1,ERR=998) DF_IOREC
C
      IERR=0
  999 CONTINUE
      RETURN
C
 998  CONTINUE
      IERR = -1
      RETURN
C
 997  CONTINUE
      IERR = -2
      RETURN
      END
