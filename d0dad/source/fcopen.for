      SUBROUTINE FCOPEN(LWRITE,LUN,IKEY,FNAME,CSTAT,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open an unsorted event catalog file.
C-      Called from D0DAD_OPEN.
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
      LOGICAL LWRITE,OK
      INTEGER LUN,IRECL,IERR,ITMP,IFILE,IKEY
      CHARACTER*(*) FNAME,CSTAT,CHOPT*2
C----------------------------------------------------------------------
C
C  Setup...
C
      IERR=0
      IRECL=JRECFC
C
C  Do the open...
C
      CHOPT='IUS'                                     ! Sharable
      IF( LWRITE ) CHOPT='OUS'
      IF( LWRITE .AND. CSTAT.EQ.'OLD' ) CHOPT='MUS'
CJDH      CHOPT='IU'                                      ! Non-shared
CJDH      IF( LWRITE ) CHOPT='OU'
CJDH      IF( LWRITE .AND. CSTAT.EQ.'OLD' ) CHOPT='MU'
C
      CALL D0RZOPEN(LUN,FNAME,CHOPT,4*IRECL,OK)
      IF( .NOT.OK ) GOTO 901
C
C  Create the control Zebra bank.
C
      CALL FCBANK(IERR)
      IF( IERR.NE.0 ) GOTO 904
      IQ(LFCHD+JLUN)=LUN
      IF( LWRITE ) IQ(LFCHD+JRW)=1
      IQ(LFCHD+JKEY)=IKEY
C
C  Do the header manipulation...
C
      IF( CSTAT.EQ.'OLD' ) THEN
         CALL FCHRD(LUN,IFILE,IERR)
         IF( IERR.NE.0 ) GOTO 903
      ELSE
         CALL FCHWRT(LUN,0,IERR)
         IF( IERR.NE.0 ) GOTO 903
      ENDIF
C
  999 CONTINUE
      IERR=0
      RETURN

 901  CONTINUE
      WRITE(D0DAD_ERRTXT,1001) FNAME,IERR
 1001 FORMAT('File "',A40,'", Open(write) error, Error=',I4)
      IF(LDDBG.GT.0) CALL ERRMSG(' ','FCOPEN',D0DAD_ERRTXT,'W')
      IERR = -1
      RETURN

 902  CONTINUE
      WRITE(*,1002) FNAME,IERR
 1002 FORMAT('File "',A40,'", Open(read) error, Error=',I4)
      IF(LDDBG.GT.0) CALL ERRMSG(' ','FCOPEN',D0DAD_ERRTXT,'W')
      IERR = -2
      RETURN

 903  CONTINUE
      WRITE(*,1003) FNAME,IERR
 1003 FORMAT('File "',A40,'", Header r/w  error ',I4)
      IF(LDDBG.GT.0) CALL ERRMSG(' ','FCOPEN',D0DAD_ERRTXT,'W')
      IERR = -3
      RETURN

 904  CONTINUE
      WRITE(D0DAD_ERRTXT,1004) FNAME,IERR
 1004 FORMAT('File "',A40,'", Cannot create Zebra bank.')
      IF(LDDBG.GT.0) CALL ERRMSG(' ','FCOPEN',D0DAD_ERRTXT,'W')
      IERR = -4
      END
