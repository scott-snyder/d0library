      SUBROUTINE UEOPEN(LWRITE,LUN,IKEY,FNAME,CSTAT,IERR)
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
      INTEGER LUN,IERR,ikey
      CHARACTER*(*) FNAME,CSTAT,CHOPT*4
C----------------------------------------------------------------------
C
C  Setup control  
C
      IERR=0
      CHOPT='IFL'
      IF( LWRITE ) CHOPT='OFL'
      IF( CSTAT.EQ.'OLD' .AND. LWRITE ) CHOPT='MAFL'
      CALL D0OPEN(LUN,FNAME,CHOPT,OK)
      IF( .NOT.OK ) GOTO 901
C
C  Setup the Zebra bank to hold file info...
C
      CALL UEBANK(IERR)
      IF( IERR.NE.0 ) GOTO 904
      IQ(LUEHD+JLUN)=LUN
      IF(LWRITE) IQ(LUEHD+JRW)=1
      IQ(LUEHD+JKEY)=IKEY
C
C  Setup header copy
C
      IF( CSTAT.EQ.'OLD' ) THEN
         IF( .NOT.LWRITE ) CALL UEHRD(LUN,IERR)
         IF( IERR.NE.0 ) GOTO 903
      ELSE
         CALL UEHWRT(LUN,IERR)
         IF( IERR.NE.0 ) GOTO 903
      ENDIF
C
  999 CONTINUE
      IERR=0
      RETURN

 901  CONTINUE
      IERR = -1
      RETURN

 903  CONTINUE
      IERR = -3
      RETURN

 904  CONTINUE
      IERR = -4
      RETURN

      END
