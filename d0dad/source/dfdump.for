      SUBROUTINE DFDUMP(ILUN,IOPTS,NOPTS,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write a formattted dump of a d0dad file.
C-     The file should be opened using D0DAD_OPEN
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  14-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER ILUN,NOPTS,IOPTS(*)
      INTEGER IRUN,IEVT,IFID,IZRN,IZBO,IERR
      INTEGER IROLD,IREC(JRECDF),I,NEV,NEVRUN,IRONE,IDATE(2)
      CHARACTER*8 CFID,CTAG*20
      LOGICAL LEVTS,D0DAD_RE_RANGE
      EXTERNAL D0DAD_RE_RANGE
C----------------------------------------------------------------------
C
      CALL DFLSET(ILUN,IERR)
      IF( IERR.NE.0 ) THEN
        IERR = -4
        GOTO 999
      ENDIF
C
C  Header info
C
      CALL UHTOC(IQ(LDFHD+NDDF+4),4,CTAG,20)
      CFID='D0DAD '//CFTYPE(JFDF)
      WRITE(*,1000) CFID,IQ(LDFHD+NDDF+3),CTAG
 1000 FORMAT(/,/,
     +       '   ID Field: ',A,/,
     +       '   Version: ',I5,/,
     +       '   Tag String: ',A20,/,/)
C
C  Body
C
      IRONE = -1
      IROLD = -1
      LEVTS=IOPTS(1)
      NEV=0
      NEVRUN=0
      IF( .NOT.LEVTS ) THEN
        WRITE(*,2001)
 2001   FORMAT('      Run    #Events')
      ELSE
        WRITE(*,1002)
 1002   FORMAT('   Run    Event      FID      Record     ByteOffset')
      ENDIF
C
 10   CONTINUE
         CALL DFGET(ILUN,IRUN,IEVT,IFID,IZRN,IZBO,IDATE,IERR)
         IF( IERR.EQ.1 ) GOTO 999
         IF( IERR.LT.0 ) THEN
            WRITE(*,9001) IERR,ILUN,IRUN,IEVT
 9001       FORMAT(' DFDUMP: Error ',I4,' returned from DFGET (lun=',
     +           I3,') for r/e: ',2I8)
            IERR = -1
            RETURN
         ENDIF
         IF( .NOT.D0DAD_RE_RANGE(IOPTS(2),IOPTS(4),IRUN,IEVT) ) GOTO 10
         IF( LEVTS ) WRITE(*,1001) IRUN,IEVT,IFID,IZRN,IZBO
 1001    FORMAT(' ',I7,1X,I8,1X,I8,2X,I6,4X,I6)
         IF( IRUN.NE.IROLD .AND. .NOT.LEVTS ) THEN
           IF( IROLD.NE.(-1) ) THEN
             WRITE(*,1004) IROLD,NEVRUN
 1004        FORMAT(' ',I8,' ',I8)
             NEVRUN=0
             IRONE=IROLD
           ENDIF
         ENDIF
         IROLD=IRUN
         NEV=NEV+1
         NEVRUN=NEVRUN+1
      GOTO 10
C
  999 CONTINUE
      IF( IRUN.NE.IRONE .AND. .NOT.LEVTS ) WRITE(*,1004) IRUN,NEVRUN
      WRITE(*,1003) NEV
 1003 FORMAT(/,' D0Dad file contains ',I10,' events.',/)
      IERR=0
      RETURN
      END
