      SUBROUTINE D0DAD_CREATE(FNBASE,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create an event catalog and file catalog
C-      pair.
C-
C-   Inputs  : FNBASE - File name base
C-   Outputs : IERR   - 0 ==> All is OK.
C-   Controls: 
C-
C-   Created   1-DEC-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INCLUDE 'D0$INC:d0dad.inc/NOLIST'
      CHARACTER*(*) FNBASE
      INTEGER IERR
      CHARACTER*45 CECNAM,CFCNAM
      INTEGER LENOCC,LFN
      EXTERNAL LENOCC
C----------------------------------------------------------------------
C
C  Parse the tag fields and names
C
      LFN=LENOCC(FNBASE)
      CECTAG=FNBASE(1:LFN)
      CFCTAG=FNBASE(1:LFN)
      CECNAM=FNBASE(1:LFN)//'.evtcat'
      CFCNAM=FNBASE(1:LFN)//'.filecat'
C
C  Create the event catalog...
C
      CALL D0DAD_OPEN(JFEC,CECNAM,'W',IDADEC,IERR)
      IF( IERR.NE.0 ) THEN
         WRITE(*,*) ' UPDATE: Return from D0DAD_OPEN(ec)=',IERR
         IERR = -1
         GOTO 999
      ENDIF
*
*  Create the file catalog...
*
      CALL D0DAD_OPEN(JFFC,CFCNAM,'W',IDADFC,IERR)
      IF( IERR.NE.0 ) THEN
         WRITE(*,*) ' UPDATE: Return from D0DAD_OPEN(fc)=',IERR
         IERR = -2
         GOTO 999
      ENDIF
C
C  And done...
C
      CALL D0DAD_CLOSE(IDADEC,IERR)
      CALL D0DAD_CLOSE(IDADFC,IERR)
      IERR = 0
C
  999 CONTINUE
      RETURN
      END
